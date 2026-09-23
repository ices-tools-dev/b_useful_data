library(tidyverse)
library(vegan)
library(mFD)
library(RColorBrewer)
library(knitr)
library(sf)
library(tidyr)
source("R/fcts.R")

p.occurrence <- read_rds("./boot/data/nea/mean_p_occurrence.rds")
p.occurrence$Year <- p.occurrence$Year |> as.character() |> as.numeric()
fish_diversity <- read_rds("./boot/data/nea/biodiversity_1989-2050.RDS")
# biomass <- read_rds("./boot/data/nea/biomass.rds")
# alpha_fd_indices <- readRDS("./boot/data/nea/alpha_fd_indices.rds")
# traits <- read_rds("./boot/data/nea/traits.rds")
# functional_richness <- readRDS("./boot/data/nea/f_ric.rds")
ices_ecoregions <- st_read("./boot/data/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp") %>% st_make_valid()

# fish_diversity 

fish_diversity <- fish_diversity %>% rename("Richness" = "rRichness", "shannon" = "Shannon", "evenness" = "Evenness") |> 
  select(1:26) |> mutate(row_id = row_number())
fish_div_spatial <- fish_diversity %>% st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ices_ecoregions))
fish_div_spatial <-  fish_div_spatial %>%  st_join(ices_ecoregions)

# trends
diversity_trends <- calculate_diversity_trend(fish_diversity, metrics = c("Richness", "shannon", "evenness", "fric", "feve", "fdis", "fdiv"))

# Prepare grids
grid_low_res <- fish_div_spatial %>% select(Cell, Lon_c, Lat_c) %>% unique()

#prepare sdm spatial
p.occurrence_test <- p.occurrence %>% st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ices_ecoregions))
p.occurrence_test <-  p.occurrence_test |> st_join(ices_ecoregions)
p.occurrence_test <- p.occurrence_test |> mutate(ices_ecoregions = case_when(!is.na(Ecoregion.y) ~Ecoregion.y, 
                                                                  .default = Ecoregion.x))

p.occurrence_test$ecoregion[p.occurrence_test$ecoregion %in% c("North Sea")] <- "Greater North Sea"
p.occurrence_test$ecoregion[p.occurrence_test$ecoregion %in% c("Southern Norway") & p.occurrence_test$Lat_c >= 60.2] <- "Norwegian Sea"
p.occurrence_test$ecoregion[p.occurrence_test$ecoregion %in% c("Southern Norway") & p.occurrence_test$Lat_c < 60.2] <- "Greater North Sea"
p.occurrence_test$ecoregion[p.occurrence_test$ecoregion %in% c("South and West Iceland", "North and East Iceland")] <- "Icelandic Waters"
p.occurrence_test$ecoregion[p.occurrence_test$ecoregion %in% c("East Greenland Shelf")] <- "West Greenland Shelf"
p.occurrence_test$ecoregion[p.occurrence_test$ecoregion %in% c("North and East Barents Sea")] <- "Barents Sea"
p.occurrence_test$ecoregion[p.occurrence_test$ecoregion %in% c("Northern Norway and Finnmark")] <- "Barents Sea"
p.occurrence_test$ecoregion[p.occurrence_test$ecoregion %in% c("South European Atlantic Shelf")] <- "Bay of Biscay and the Iberian Coast"
p.occurrence_test$ecoregion[p.occurrence_test$ecoregion %in% c("Saharan Upwelling")] <- "Bay of Biscay and the Iberian Coast"

p.occurrence <- cbind(p.occurrence, ices_ecoregion = p.occurrence_test$ecoregion)
p.occurrence <- p.occurrence |> filter(!ices_ecoregion == "Faroes")
# Save these in a different format?
# saveRDS(p.occurrence, file = "data/nea_fish_p_occurrence.rds")
# saveRDS(fish_diversity, file = "data/nea_fish_diversity.rds")
# saveRDS(fish_div_spatial, file = "data/nea_fish_div_spatial.rds")

arrow::write_parquet(p.occurrence,"data/nea_fish_p_occurrence.parquet")
arrow::write_parquet(fish_diversity,"data/nea_fish_diversity.parquet")
sfarrow::st_write_parquet(fish_div_spatial,"data/nea_fish_div_spatial.parquet")

diagnostics <- readRDS("./boot/data/nea/performance_metrics.RDS")
diagnostics <- rename(diagnostics, model_component = model, metric_name = metric)
saveRDS(diagnostics, file = "data/nea_fish_diagnostics.rds")
saveRDS(diversity_trends, file = "data/nea_fish_diversity_trends.rds")
saveRDS(grid_low_res, file = "data/nea_grid_low_res.rds")
rm(list = ls())


#Make geopackage for data downloads
#gpkg <- "data/b_useful_nea_model_outputs.gpkg"


write.csv(diagnostics,"data/nea_fish_diagnostics.csv")

traits <- read_rds("./boot/data/nea/traits.rds")
write.csv(traits,"data/nea_fish_traits.csv")

# Make zipfile for github release
zip(zipfile = "data/b_useful_data_bundle_nea", 
    files = c("Disclaimer_B-USEFUL.txt", 
              "data/nea_fish_diagnostics.csv",
              "data/nea_fish_traits.csv",
              "data/nea_species_p_occurrence.parquet",
              "data/nea_fish_diversity.parquet",
              "data/nea_fish_div_spatial.parquet")
)