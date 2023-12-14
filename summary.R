library(sf)
library(s2)
library(dplyr)
library(stars)
library(units)
library(ggthemes)
library(ggplot2)
library(plyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(ggpattern)
library(readr)

if(file.exists("data_gap_country.csv")){
    
    area_data <- read_csv("data_gap_country.csv")    

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
        mutate(area_covered = round(area_total - area_missing, 10), ratio_missing = area_missing / area_total)

    readr::write_csv(area_data, "data_gap_country.csv")
}

round(sum(area_data$area_total))

round(sum(area_data$area_missing))

round(sum(area_data$area_total) - sum(area_data$area_missing))

round(sum(area_data$area_missing) / sum(area_data$area_total))

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
mining_properties <- st_read("./data/snl2020.gpkg", quiet = TRUE) |>
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
