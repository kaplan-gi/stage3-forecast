# Title: Stage 3 Incidence and Prevalence Forecast by ARIMA Shiny Application, global with packages and data
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2025-09-17
# Updated: 2025-09-22
# R version 4.5.0 (2025-04-11)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sequoia 15.6.1


# load libraries
library(rsconnect) # version 0.8.18
library(shiny) # version 1.6.0
library(shinyjs) # show/hide function
library(bslib)
library(shinyWidgets)
library(readr) # version 1.4.0; parsing date information
library(tidyverse) # version 0.18
library(shinycssloaders)
library(leaflet) # version 2.0.4.1
library(sf)
library(geojsonsf)
library(plotly)
library(fontawesome)


# prep polygon data 
# study_regions <- c("Canada", "United States", "Scotland", "Catalonia", "Sweden", "Denmark", "Hungary", "Israel", "New Zealand")
# geo <- geojson_sf("https://raw.githubusercontent.com/kaplan-gi/geo_files/main/GIVES_geofiles.geojson") %>% 
#   filter(name %in% study_regions) %>% 
#   mutate(name_format = case_when(
#                           name == "Scotland" ~ "Scotland (Lothian)",
#                           name == "United States" ~ "USA (Olmsted County)",
#                           name == "Hungary" ~ "Hungary (Veszprém)",
#                           name == "New Zealand" ~ "New Zealand (Canterbury)",
#                           TRUE ~ name))
# 
# st_write(geo,  dsn = "~/Documents/Kaplan Lab 2025/Stage3_Forecast_ARIMA/Shiny_Data_Geo.geojson")


# load datasets
#path <- "~/Documents/Kaplan Lab 2025/Stage3_Forecast_ARIMA/"
path <- "https://raw.githubusercontent.com/kaplan-gi/stage3-forecast/main/"

raw_dis <- read.csv(paste0(path, "Shiny_Data_Disease.csv"), header = TRUE)
raw_age <- read.csv(paste0(path, "Shiny_Data_AgeGroups.csv"), header = TRUE)
raw_sex <- read.csv(paste0(path, "Shiny_Data_Sex.csv"), header = TRUE)

raw_data <- list(dis = raw_dis, age = raw_age, sex = raw_sex)

geo <- geojson_sf(paste0(path, "Shiny_Data_Geo.geojson"))


# assign colors to countries for consistency (colourblind friendly tol_muted palette)
plot_pal <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#D3BD4E', '#999933','#CC6677', '#882255', '#AA4499', '#363538')
names(plot_pal) <- c("Canada", "Catalonia", "Denmark", "Hungary", "Israel", "New Zealand", "Scotland", "Sweden", "United States", "Global")


# edit data
data <- lapply(raw_data, function (x) {
  df <- x %>% 
    mutate(index = row_number(),  # add index
           year = as.character(.$year),  # convert year type to char
           name = case_when(name == "ISRAEL" ~ "Israel",
                            name == "NewZealand" ~ "New Zealand",
                            name == "USA" ~ "United States", 
                            TRUE ~ name), # recode name to match geo
           color = plot_pal[name]) %>%  # add palette
    full_join(geo, ., by = "name", multiple = "all")  # merge in geo data
})


# generate subsets
inc_dis <- data[["dis"]] %>% filter(data_type == "Incidence") %>% mutate(agegrp = "All ages", sex = "Both sexes")
inc_age <- data[["age"]] %>% filter(data_type == "Incidence") %>% mutate(sex = "Both sexes")
inc_sex <- data[["sex"]] %>% filter(data_type == "Incidence") %>% mutate(agegrp = "All ages")
prev_dis <- data[["dis"]] %>% filter(data_type == "Prevalence") %>% mutate(agegrp = "All ages", sex = "Both sexes")
prev_age <- data[["age"]] %>% filter(data_type == "Prevalence") %>% mutate(sex = "Both sexes")
prev_sex <- data[["sex"]] %>% filter(data_type == "Prevalence") %>% mutate(agegrp = "All ages")

# test <- inc_dis %>% 
#   st_drop_geometry() %>%
#   group_by(name, disease_type) %>% 
#   filter(forecast == "Forecasted" | (forecast == "Observed" & year == max(year[forecast == "Observed"], na.rm = TRUE)))


# format data for download
inc_dl <- rbind(inc_dis, inc_age, inc_sex) %>% 
  st_drop_geometry() %>% 
  mutate(age = fct_recode(agegrp, "Pediatric (<18)" = "Peds (<18)",
                          "Adult (18 to 64)" = "Adults (18 to 64)",
                          "Seniors (>65)" = "Elderly (65+)")) %>%
  select(data_type, region = name, year, disease_type, age_group = agegrp, sex, rate, lower_bound = lb, upper_bound = ub)

prev_dl <- rbind(prev_dis, prev_age, prev_sex) %>% 
  st_drop_geometry() %>% 
  mutate(age = fct_recode(agegrp, "Pediatric (<18)" = "Peds (<18)",
                          "Adult (18 to 64)" = "Adults (18 to 64)",
                          "Seniors (>65)" = "Elderly (65+)")) %>%
  select(data_type, region = name, year, disease_type, age_group = agegrp, sex, rate, lower_bound = lb, upper_bound = ub)



# options
options(spinner.color="#52D6F4")#, warn = -1)




  
