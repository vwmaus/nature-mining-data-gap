library(sf)
library(s2)
library(dplyr)
library(stars)
library(units)
library(ggthemes)
library(ggplot2)

dir.create("./data", showWarnings = FALSE)

# Define grid parameters - 5 arcmin (~10km x 10km)
crs <- st_crs(4326)
grid_template <- st_as_stars(st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90), crs = crs), nx = 720, ny = 360, values = 0)
world_map <- s2_data_countries() |>
  s2_union_agg(options = s2_options(model = "closed")) |>
  st_as_sf() |>
  st_simplify()

bbox_robinson <- st_bbox(c(xmin = -145, xmax = 180, ymin = -55, ymax = 80), crs = st_crs(4326)) |>
  st_as_sfc(bbox) |>
  st_transform(crs = "+proj=robin")

ylim_robinson <- c(st_bbox(bbox_robinson)["ymin"], st_bbox(bbox_robinson)["ymax"])
xlim_robinson <- c(st_bbox(bbox_robinson)["xmin"], st_bbox(bbox_robinson)["xmax"])

# Download mining land use data from Maus et al. 2022
# This dataset represents the mining land use circa 2019
if(!file.exists("./data/global_mining_polygons_v2.gpkg")){
  download.file("https://download.pangaea.de/dataset/942325/files/global_mining_polygons_v2.gpkg", destfile = "./data/global_mining_polygons_v2.gpkg")
}
mining_land_use <- st_read("./data/global_mining_polygons_v2.gpkg", quiet = TRUE) |> 
  dplyr::select(geometry = "geom") |> 
  st_cast("POLYGON") |> 
  dplyr::filter(st_geometry_type(geometry) %in% c("POLYGON"), !st_is_empty(geometry)) |>
  st_transform(crs) |>
  rename(geom = geometry) |>
  mutate(area = set_units(st_area(geom), km^2))

# Read mining properties from reporting production at any time between 2000 and 2017
# This data was extracted from the S&P SNL database as of 2020
mining_properties <- st_read("./data/snl2020.gpkg", quiet = TRUE) |>
  st_transform(crs)

# Create mining land use grid
mining_land_use_grid <- mining_land_use |>
  dplyr::transmute(dataset = 1) |>
  st_rasterize(template = grid_template, proxy = FALSE, options = "ALL_TOUCHED=TRUE")

# Create mining properties grid
mining_properties_grid <- mining_properties |>
  dplyr::transmute(dataset = 1) |>
  st_rasterize(template = grid_template, proxy = FALSE, options = "ALL_TOUCHED=TRUE")

# Calculate grid cells statistics
matches_cells <- mining_land_use_grid + ( 2 * mining_properties_grid)
matches_cells <- matches_cells * matches_cells / matches_cells
cell_freq <- table(matches_cells)
round(cell_freq / sum(cell_freq) * 100, 2)

gap_cells <- st_as_sf(matches_cells) |>
  filter(dataset == 1)

# Plot gap map
gp <- ggplot(data = world_map) +
  geom_sf(fill = "#D3D3D3", colour = NA) +
  xlab("Longitude") + ylab("Latitude") +
  theme_map() +
  geom_sf(data = gap_cells, fill = "#cc3e5b", colour = NA) +
  theme(
    legend.position = 'none',
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) + 
  coord_sf(xlim = xlim_robinson, ylim = ylim_robinson, crs = st_crs("+proj=robin"))

ggsave(gp, filename = "./data/gap_map_50km_grid.pdf", width = 12.1, height = 6.5, dpi = 300)


### Other spatial coverage checks

# Calculate mining land use intersecting 10km buffer from mining properties
mining_land_use_intersects_10km <- st_filter(mining_land_use, st_buffer(mining_properties, set_units(10, km)), .predicate = st_intersects)
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
