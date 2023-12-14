library(sf)
library(s2)
library(dplyr)
library(stars)
library(units)
library(ggthemes)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(ggpattern)
library(readr)

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
snl2020 <- st_read("./data/snl2020.gpkg", quiet = TRUE)

mining_properties <- st_transform(snl2020, crs)

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
    select(country, geom) |>
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
    filter(st_is_valid(geom)) 

  # Save mining land use data
  st_write(mining_land_use, "./data/global_mining_land_use.gpkg", delete_layer = TRUE)

  # remove variables clear cache
  rm(mining_land_use, tang2023, maus2022)
  gc(reset = TRUE, full = TRUE)

}

mining_land_use <- st_read("./data/global_mining_land_use.gpkg", quiet = TRUE)|>
    mutate(area = set_units(st_area(geom), km^2))

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

# percentage of area outside the 10km buffer
mining_land_use_diff_10km$area |> sum() / mining_land_use$area |> sum()

# percentage of area inside the 10km buffer
(mining_land_use$area |> sum() - mining_land_use_diff_10km$area |> sum()) / mining_land_use$area |> sum()

# total area check
(mining_land_use$area |> sum() - mining_land_use_diff_10km$area |> sum()) + mining_land_use_diff_10km$area |> sum()

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

# Plot gap map with 50km grid
gp <- ggplot(data = world_map) +
  geom_sf(fill = "#D3D3D3", colour = NA) +
  xlab("Longitude") + ylab("Latitude") +
  theme_map() +
  geom_sf(data = st_transform(mining_properties, crs = st_crs("+proj=robin")), size = 0.05) +
  theme(
    legend.position = 'none',
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) + 
  coord_sf(xlim = xlim_robinson, ylim = ylim_robinson, crs = st_crs("+proj=robin"))

ggsave(gp, filename = "./data/snl_coords_map.pdf", width = 12.1, height = 6.5, dpi = 300)
ggsave(gp, filename = "./data/snl_coords_map.png", width = 12.1, height = 6.5, dpi = 300)

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





#################################################################################
#
#  Additional plots and summaries 
#

if(file.exists("data/data_gap_country.csv")){
    
    area_data <- read_csv("data/data_gap_country.csv")    

} else {

    sf_use_s2(TRUE)
    
    mining_area_missing <- st_read("./data/mining_land_use_diff_10km.gpkg", quiet = TRUE) |>
        mutate(area = st_area(geom) |> units::set_units(km^2) |> as.numeric()) 

    sf_use_s2(FALSE)

    world_map <- ne_countries(scale = "medium", returnclass = "sf") |>
        select(admin, adm0_a3) 

    mining_area_covered <- st_read("./data/global_mining_land_use.gpkg", quiet = TRUE) |> 
        st_join(world_map, join = st_nearest_feature) |>
        st_drop_geometry()  |>
        as_tibble()

    mining_area_missing <- mining_area_missing |> 
        st_join(world_map, join = st_nearest_feature) |> 
        st_drop_geometry() |> 
        as_tibble()

    country_area_missing <- mining_area_missing |>
        dplyr::group_by(admin, adm0_a3) |>
        dplyr::summarise(area_missing = sum(area))

    country_area_covered <- mining_area_covered |>
        dplyr::group_by(admin, adm0_a3) |>
        dplyr::summarise(area_total = sum(area))

    area_data <- left_join(country_area_missing, country_area_covered) |>
        mutate(area_covered = round(area_total - area_missing, 10)) |>
        select(admin, adm0_a3, area_covered, area_missing, area_total) |>
        mutate(ifelse(area_covered < 0), 0, area_covered) # clean numerical imprecision

    readr::write_csv(area_data, "data/data_gap_country.csv")
}

round(sum(area_data$area_total))

round(sum(area_data$area_missing))

round(sum(area_data$area_total) - sum(area_data$area_missing))

round(sum(area_data$area_missing) / sum(area_data$area_total)*100)

# --------------------------------------------------------------------------------------
# define ggplot theme ------------------------------------------------------------------
textwidth <- 345 # Get from latex comand: \the\textwidth in pt -- divide by 2.835 to mm
textheight <- 550 # Get from latex comand: \the\textheight in pt -- divide by 2.835 to mm
font_size <- 6 # font size in pt 
pt_to_mm <- 2.835
font_family <- "Helvetica"

th <- ggplot2::theme(axis.text = ggplot2::element_text(size = font_size, family = font_family), 
                     text = ggplot2::element_text(size = font_size, family = font_family)) 


# --------------------------------------------------------------------------------------
# sorted missing
country_area_covered <- area_data |> 
  dplyr::ungroup() |>
  dplyr::arrange(desc(area_missing)) |>
  dplyr::mutate(cum_area = cumsum(area_missing) / sum(area_missing)) |>
  dplyr::mutate(Country = ifelse(cum_area < .90, as.character(admin), "Rest of the world"),
                iso3 = ifelse(cum_area < .90, as.character(adm0_a3), "ROW")) %>% 
  dplyr::group_by(Country, iso3) %>%
  dplyr::summarise(area_total = sum(area_total), area_missing = sum(area_missing), area_covered = sum(area_covered), cum_area = tail(cum_area, 1), n = n()) |>
  dplyr::mutate(group = tail(cum_area, 1), n = sum(n)) |>
  dplyr::arrange(group, dplyr::desc(area_missing)) |>
  dplyr::ungroup() |>
  dplyr::mutate(Country = factor(Country, levels = c(Country[Country != "Rest of the world"], "Rest of the world")),
                iso3 = factor(iso3, levels = c(iso3[iso3 != "ROW"], "ROW")))

country_area_covered <- country_area_covered |>
  dplyr::arrange(group, dplyr::desc(area_missing))

coutry_factors <- c(as.character(country_area_covered$Country)[country_area_covered$iso3=="ROW"], 
                    as.character(country_area_covered$Country[country_area_covered$iso3!="ROW"])[order(country_area_covered$area_missing[country_area_covered$iso3!="ROW"])])

plot_data <- select(country_area_covered, Country, area_covered, area_missing) |>
    tidyr::pivot_longer(cols = -Country, names_to = "pattern", values_to = "area") |>
    mutate(pattern = factor(pattern, c("area_covered", "area_missing")),
           Country = factor(Country, coutry_factors))

gp_country_area <- plot_data  |>
  ggplot2::ggplot(aes(x = Country, y = area, fill = pattern)) +
  ggplot2::geom_bar(stat = "identity", position="stack") +
  ggplot2::coord_flip() +
#  ggplot2::scale_y_continuous(breaks=pretty_breaks(8), limits = c(0, 15000), expand = c(0,0), labels = comma) +
  ggplot2::labs(x = NULL, y = bquote('Mining land use ('*Km^2*')')) +
  ggplot2::theme(legend.position = c(0.7, .4),
                 legend.direction = "vertical",
                 legend.title = element_blank(),
                 legend.key.size = unit(0.3, "cm"),
                 rect = element_blank(),
                 axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
                 axis.ticks.y = element_blank(),
                 axis.line.x = element_line(),
                 panel.grid = element_blank(),
                 plot.margin = unit(c(0.1, 0.1, 0.0, 0.1), "cm")) +
   ggplot2::scale_fill_manual(
        values = c(area_covered = "darkgoldenrod1", area_missing = "brown2"),
        labels = c("Documented production", "Undocumented production")) +
  th

ggsave(filename = "./data/country-area-bar-sort-missing.pdf", plot = gp_country_area, bg = "#ffffff",
       width = textwidth/pt_to_mm, height = textheight/pt_to_mm/2.2, units = "mm", scale = 1)





# --------------------------------------------------------------------------------------
# sorted total

country_area_covered <- country_area_covered |>
  dplyr::arrange(group, dplyr::desc(area_total))

coutry_factors <- c(as.character(country_area_covered$Country)[country_area_covered$iso3=="ROW"], 
                    as.character(country_area_covered$Country[country_area_covered$iso3!="ROW"])[order(country_area_covered$area_total[country_area_covered$iso3!="ROW"])])

plot_data <- select(country_area_covered, Country, area_covered, area_missing) |>
    tidyr::pivot_longer(cols = -Country, names_to = "pattern", values_to = "area") |>
    mutate(pattern = factor(pattern, c("area_covered", "area_missing")),
           Country = factor(Country, coutry_factors))

gp_country_area <- plot_data  |>
  ggplot2::ggplot(aes(x = Country, y = area, fill = pattern)) +
  ggplot2::geom_bar(stat = "identity", position="stack") +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = bquote('Mining land use ('*Km^2*')')) +
  ggplot2::theme(legend.position = c(0.7, .4),
                 legend.direction = "vertical",
                 legend.title = element_blank(),
                 legend.key.size = unit(0.3, "cm"),
                 rect = element_blank(),
                 axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
                 axis.ticks.y = element_blank(),
                 axis.line.x = element_line(),
                 panel.grid = element_blank(),
                 plot.margin = unit(c(0.1, 0.1, 0.0, 0.1), "cm")) +
   ggplot2::scale_fill_manual(
        values = c(area_covered = "darkgoldenrod1", area_missing = "brown2"),
        labels = c("Documented production", "Undocumented production")) +
  th

ggsave(filename = "./data/country-area-bar-sort-total.pdf", plot = gp_country_area, bg = "#ffffff",
       width = textwidth/pt_to_mm, height = textheight/pt_to_mm/2.2, units = "mm", scale = 1)


# Read mining properties from reporting production at any time between 2000 and 2017
# This data was extracted from the S&P SNL database as of 2020
mining_properties <- snl2020 |>
    st_drop_geometry()  |>
    as_tibble()


#######################
# Collection of points to share:

select(mutate(country_area_covered, perc_cove = area_covered / area_total), -cum_area)

# SNL all coordinates country
mining_properties |> 
    group_by(country) |>
    tally() |>
    arrange(desc(n)) |>
    mutate(percent = n / sum(n), cum_percent = cumsum(n) / sum(n))

# SNL all coordinates primary commodity
mining_properties |> 
    group_by(primary_commodity) |>
    tally() |>
    arrange(desc(n)) |>
    mutate(percent = n / sum(n), cum_percent = cumsum(n) / sum(n))

mining_properties |> 
    filter(primary_commodity == "Coal") |>
    group_by(country) |>
    tally() |>
    arrange(desc(n)) |>
    mutate(percent = n / sum(n), cum_percent = cumsum(n) / sum(n))

countr_count <- mining_properties |> 
    group_by(country) |>
    tally() |>
    select(country, n_country = n)

comm_count <- mining_properties |> 
    group_by(country, primary_commodity) |>
    tally() |>
    dplyr::rename(n_comm = n) |>
    left_join(countr_count)

comm_count |>
    mutate(percent = n_comm / n_country) |>
    filter(n_country > 100) |>
    arrange(desc(percent))
    


mining_properties |> 
    filter(primary_commodity == "Gold") |>
    group_by(country) |>
    tally() |>
    arrange(desc(n)) |>
    mutate(percent = n / sum(n), cum_percent = cumsum(n) / sum(n))

mining_properties |> 
    filter(primary_commodity == "Copper") |>
    group_by(country) |>
    tally() |>
    arrange(desc(n)) |>
    mutate(percent = n / sum(n), cum_percent = cumsum(n) / sum(n))

mining_properties |> 
    filter(primary_commodity == "Iron Ore") |>
    group_by(country) |>
    tally() |>
    arrange(desc(n)) |>
    mutate(percent = n / sum(n), cum_percent = cumsum(n) / sum(n))

mining_properties |> 
    filter(primary_commodity == "Zinc") |>
    group_by(country) |>
    tally() |>
    arrange(desc(n)) |>
    mutate(percent = n / sum(n), cum_percent = cumsum(n) / sum(n))

mining_properties |> 
    filter(primary_commodity == "Diamonds") |>
    group_by(country) |>
    tally() |>
    arrange(desc(n)) |>
    mutate(percent = n / sum(n), cum_percent = cumsum(n) / sum(n))

mining_properties |> 
    filter(primary_commodity == "Lithium") |>
    group_by(country) |>
    tally() |>
    arrange(desc(n)) |>
    mutate(percent = n / sum(n), cum_percent = cumsum(n) / sum(n))
