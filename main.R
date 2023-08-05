library(sf)
library(s2)
library(dplyr)
library(stars)
library(units)
library(ggthemes)
library(ggplot2)
library(plyr)
source("s2_union_split_agg.R")
dir.create("./data", showWarnings = FALSE)

# Define grid parameters - (~50km x 50km)
crs <- st_crs(4326)
grid_template <- st_as_stars(st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90), crs = crs), nx = 720, ny = 360, values = 0)
grid_template$id <- 1:prod(dim(grid_template))
world_map <- s2_data_countries() |>
  s2_union_agg(options = s2_options(model = "closed")) |>
  st_as_sf() |>
  st_simplify()

bbox_robinson <- st_bbox(c(xmin = -145, xmax = 180, ymin = -55, ymax = 80), crs = st_crs(4326)) |>
  st_as_sfc(bbox) |>
  st_transform(crs = "+proj=robin")

ylim_robinson <- c(st_bbox(bbox_robinson)["ymin"], st_bbox(bbox_robinson)["ymax"])
xlim_robinson <- c(st_bbox(bbox_robinson)["xmin"], st_bbox(bbox_robinson)["xmax"])

# Read mining properties from reporting production at any time between 2000 and 2017
# This data was extracted from the S&P SNL database as of 2020
mining_properties <- st_read("./data/snl2020.gpkg", quiet = TRUE) |>
  st_transform(crs)

if(!file.exists("./data/mining_properties_buffer.gpkg")){

# Create a buffer around the mining properties
mining_properties_buffer <- st_buffer(mining_properties, set_units(10, km)) |>
  st_as_s2(mining_properties_buffer) |>
  s2_union_split_agg(options = s2_options(model = "closed")) |>
  st_as_sf() |>
  select(geom = "geometry") |>
  filter(st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON")) |>
  st_cast("POLYGON") |>
  st_make_valid() |>
  filter(st_is_valid(geom)) |>
  st_transform(crs)

  st_write(mining_properties_buffer, "./data/mining_properties_buffer.gpkg", driver = "GPKG")

}

mining_properties_buffer <- st_read("./data/mining_properties_buffer.gpkg", quiet = TRUE)

# Merge global mining land datasets if not processed yet
if(!file.exists("./data/global_mining_land_use.gpkg")){
  # Download mining land use data from Maus et al. 2022 (doi: 10.1594/PANGAEA.942325)
  # This dataset was derived from satellite images acquared in 2019
  if(!file.exists("./data/maus2022.gpkg")){
    download.file("https://download.pangaea.de/dataset/942325/files/global_mining_polygons_v2.gpkg", destfile = "./data/maus2022.gpkg")
  }
  maus2022 <- st_read("./data/maus2022.gpkg", quiet = TRUE) |>
    select(geom) |>
    st_cast("POLYGON") |>
    dplyr::filter(st_geometry_type(geom) %in% c("POLYGON"), !st_is_empty(geom)) |>
    st_transform(crs)

  # Download mining land use data from Tang & Werner 2023 (doi: 10.5281/zenodo.7894216)
  # This dataset represents the mining land use derived from images acquared in various years
  options(timeout = 600)
  download.file(url = "https://zenodo.org/record/7894216/files/74548_projected%20polygons.dbf?download=1",
                destfile = "./data/tang2023.dbf", mode = "wb")
  download.file(url = "https://zenodo.org/record/7894216/files/74548_projected%20polygons.prj?download=1",
                destfile = "./data/tang2023.prj", mode = "wb")
  download.file(url = "https://zenodo.org/record/7894216/files/74548_projected%20polygons.shp?download=1",
                destfile = "./data/tang2023.shp", mode = "wb")
  download.file(url = "https://zenodo.org/record/7894216/files/74548_projected%20polygons.shx?download=1",
                destfile = "./data/tang2023.shx", mode = "wb")

  tang2023 <- st_read("./data/tang2023.shp", quiet = TRUE) |>
    dplyr::select(geom = "geometry") |>
    st_cast("POLYGON") |>
    st_simplify() |>
    st_make_valid() |>
    filter(st_is_valid(geom)) |>
    dplyr::filter(st_geometry_type(geom) %in% c("POLYGON"), !st_is_empty(geom)) |>
    st_transform(crs)

  # Merge mining land use datasets and and remove overlaps
  mining_land_use <- bind_rows(maus2022, tang2023) |>
    filter(s2_is_valid(geom)) |>
    st_as_s2() |>
    s2_union_split_agg(options = s2_options(model = "closed")) |>
    st_as_sf() |>
    select(geom = "geometry") |>
    st_cast("POLYGON") |>
    st_make_valid() |>
    filter(st_is_valid(geom)) |>
    mutate(area = set_units(st_area(geom), km^2))

  # Save mining land use data
  st_write(mining_land_use, "./data/global_mining_land_use.gpkg", delete_layer = TRUE)

  # remove variables clear cache
  rm(mining_land_use, tang2023, maus2022)
  gc(reset = TRUE, full = TRUE)

}

mining_land_use <- st_read("./data/global_mining_land_use.gpkg", quiet = TRUE)

# Calculate intersetion between mining land use and mining properties buffers
if(!file.exists("./data/mining_land_use_diff_10km.gpkg")){

  buffers_intersects <- st_intersects(mining_land_use, mining_properties_buffer)

  mining_land_use_diff_10km <- llply(seq_along(buffers_intersects), .progress = "text", function(i){
    
    bf <- st_geometry(mining_properties_buffer[buffers_intersects[[i]], ]) |>
      st_union()

    if(length(bf) < 1){
      return(mining_land_use[i,'geom'])
    }
    mining_land_use[i, ] |>
      select(geom) |>
      st_difference(bf)
  }) |>
    bind_rows()

  st_write(mining_land_use_diff_10km, "./data/mining_land_use_diff_10km.gpkg", delete_layer = TRUE)

  # remove variables clear cache
  rm(mining_land_use_diff_10km, buffers_intersects)
  gc(reset = TRUE, full = TRUE)

}

mining_land_use_diff_10km <- st_read("./data/mining_land_use_diff_10km.gpkg", quiet = TRUE) |>
    mutate(area = set_units(st_area(geom), km^2))

mining_land_use_diff_10km$area |> sum() / mining_land_use$area |> sum()

# Create mining land use grid
gap_grid <- mining_land_use_diff_10km |>
  dplyr::transmute(dataset = 1, geom = st_centroid(geom)) |>
  st_rasterize(template = grid_template, file = "./data/gap_grid.tif", proxy = FALSE, options = "ALL_TOUCHED=TRUE") |>
  st_as_sf() |>
  filter(dataset == 1)

# Plot gap map with 50km grid
gp <- ggplot(data = world_map) +
  geom_sf(fill = "#D3D3D3", colour = NA) +
  xlab("Longitude") + ylab("Latitude") +
  theme_map() +
  geom_sf(data = gap_grid, fill = "#cc3e5b", colour = NA) +
  theme(
    legend.position = 'none',
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) + 
  coord_sf(xlim = xlim_robinson, ylim = ylim_robinson, crs = st_crs("+proj=robin"))

ggsave(gp, filename = "./data/gap_grid_map.pdf", width = 12.1, height = 6.5, dpi = 300)
ggsave(gp, filename = "./data/gap_grid_map.png", width = 12.1, height = 6.5, dpi = 300)



### Other spatial relation checks

# Calculate mining land use intersecting 10km buffer from mining properties
mining_land_use_intersects_10km <- st_filter(mining_land_use, mining_properties_buffer, .predicate = st_intersects)
sum(mining_land_use_intersects_10km$area) / sum(mining_land_use$area)
nrow(mining_land_use_intersects_10km) / nrow(mining_land_use)

# Calculate mining land use within a 10km distance from mining properties
mining_land_use_within_10km <- st_filter(mining_land_use, mining_properties, .predicate = st_is_within_distance, dist = set_units(10, km))
sum(mining_land_use_within_10km$area) / sum(mining_land_use$area)
nrow(mining_land_use_within_10km) / nrow(mining_land_use)

# Calculate mining land use centroids within a 10km distance from mining properties
mining_land_use_centroid_within_10km <- mining_land_use |>
  mutate(geom = st_centroid(geom)) |>
  st_filter(mining_properties, .predicate = st_is_within_distance, dist = set_units(10, km))
sum(mining_land_use_centroid_within_10km$area) / sum(mining_land_use$area)
nrow(mining_land_use_centroid_within_10km) / nrow(mining_land_use)


# Gap distance
# Calculate the distance from each mining land use polygon to nearest mine property
nearest_property <- mining_land_use |>
  st_nearest_feature(mining_properties)

mining_land_use$dist <- mining_land_use |>
  st_distance(mining_properties[nearest_property,], by_element = TRUE) |>
  set_units(km)

dist_grid <- select(st_as_sf(grid_template), id)

nearest_property_dist_grid <- mining_land_use |>
  st_join(dist_grid, st_intersects) |>
  st_drop_geometry() |>
  group_by(id) |>
  dplyr::summarise(dist = max(dist), area = mean(area)) 

dist_grid_map <- dist_grid |>
  left_join(nearest_property_dist_grid) |>
  filter(!is.na(area)) |>
  filter(dist > set_units(10, km))

gp <- ggplot(data = world_map) +
  geom_sf(fill = "#D3D3D3", colour = NA) +
  xlab("Longitude") + ylab("Latitude") +
  theme_map() +
  geom_sf(data = dist_grid_map, aes(fill = as.numeric(dist)), colour = NA) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.spacing = unit(0.0, "cm"),
    legend.key.size = unit(0.3, "cm"),
    legend.key.width = unit(35, "mm"),
    plot.margin = margin(t = -1, r = -1, b = 0, l = -1, unit = "cm")
  ) +
  labs(fill = bquote(Distance~to~nearest~property~(km))) +
  viridis::scale_fill_viridis(option = "turbo", 
    labels = seq(10, 1300, 290), 
    breaks = seq(10, 1300, 290), 
    limits = c(10, max(dist_grid_map$dist)),
    begin = 0.15, end = 1, direction = 1, discrete = FALSE) +
  coord_sf(xlim = xlim_robinson, ylim = ylim_robinson, crs = st_crs("+proj=robin"))

ggsave(gp, filename = "./gap_dist_grid.png", width = 12.1, height = 6.5, dpi = 300)
