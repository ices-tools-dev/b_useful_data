library(tidyverse)
library(vegan)
library(mFD)
library(RColorBrewer)
library(knitr)
library(sf)
library(tidyr)
source("R/fcts.R")

p.occurrence <- read_rds("./boot/data/iceland/p_occurrence.rds") |> 
  rename(Lon_c = centroid.cell_lon, Lat_c = centroid.cell_lat) 
biomass <- read_rds("./boot/data/iceland/biomass.rds") |> 
  rename(Lon_c = centroid.cell_lon, Lat_c = centroid.cell_lat)
fish_diversity <- read_rds("./boot/data/iceland/diversity.rds") |> 
  rename(Richness = richness,
         fric = fric_pa)

species <- names(p.occurrence)[!names(p.occurrence) %in%
                                 c("Year", "Ecoregion", "Cell", "longitude", "latitude", "Lon_c", "Lat_c", "centroid.cell_lon", "centroid.cell_lat", "Season")]

p.occurrence <- p.occurrence %>%
  group_by(latitude, longitude, Year) %>%
  summarise(
    Lat_c = first(Lat_c),
    Lat_c = first(Lat_c),
    Lon_c = first(Lon_c),
    Cell = first(Cell),
    across(all_of(species), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )
colnames(p.occurrence) <- colnames(p.occurrence) |> stringr::str_replace_all(pattern = "\\.", replacement = " ")
arrow::write_parquet(p.occurrence,"data/iceland_species_p_occurrence.parquet")
rm(p.occurrence)

biomass <- biomass %>%
  group_by(latitude, longitude, Year) %>%
  summarise(
    Lat_c = first(Lat_c),
    Lat_c = first(Lat_c),
    Lon_c = first(Lon_c),
    Cell = first(Cell),
    across(all_of(species), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )
colnames(biomass) <- colnames(biomass) |> stringr::str_replace_all(pattern = "\\.", replacement = " ")
arrow::write_parquet(biomass,"data/iceland_species_biomass.parquet")

cell_lookup <- p.occurrence |> select(Cell, Lon_c, Lat_c, longitude, latitude) |> unique()

fish_diversity <- fish_diversity %>% left_join(cell_lookup, by = c("longitude", "latitude"))

metrics <- c("Richness", "shannon", "evenness", "fric", "feve", "fdis", "fdiv")
fish_diversity <- fish_diversity |> 
  select(any_of(c("Year", 
                  "Ecoregion", 
                  "Cell", 
                  "longitude", 
                  "latitude", 
                  "Lon_c", 
                  "Lat_c", 
                  "centroid.cell_lon", 
                  "centroid.cell_lat", 
                  "Season",
                  metrics)))%>%
  group_by(latitude, longitude, Year) %>%
  summarise(
    Lat_c = first(Lat_c),
    Lat_c = first(Lat_c),
    Lon_c = first(Lon_c),
    Cell = first(Cell),
    across(all_of(metrics), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |> 
  mutate(row_id = row_number())

arrow::write_parquet(fish_diversity,"data/iceland_fish_diversity.parquet")
rm(biomass)

ices_ecoregions <- st_read("./boot/data/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp") %>% 
  st_make_valid()
fish_div_spatial <- fish_diversity %>% st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ices_ecoregions))
fish_div_spatial <-  fish_div_spatial %>%  st_join(ices_ecoregions) |> 
  select(-c(Shape_Leng, Shape_Le_1, Shape_Area, OBJECTID))

sfarrow::st_write_parquet(fish_div_spatial,"data/iceland_fish_div_spatial.parquet")

# Prepare grids
grid_low_res <- fish_div_spatial %>% select(Cell, Lon_c, Lat_c) %>% unique()
saveRDS(grid_low_res, file = "data/iceland_grid_low_res.rds")



diversity_trends <- calculate_diversity_trend_new(mutate(fish_diversity, Ecoregion = "Icelandic Waters"), metrics = c("Richness", "shannon", "evenness", "fric", "feve", "fdis", "fdiv"))
saveRDS(diversity_trends, file = "data/iceland_fish_diversity_trends.rds")

# reform diagnostics, split into model type, rename columns, pivot long, rejoin
diagnostics <- readRDS("boot/data/iceland/diagnostics.rds")

biomass_cols <- c("species", stringr::str_subset(colnames(diagnostics), pattern = "biomass")) 
names(biomass_cols) <- stringr::str_remove(biomass_cols, "_biomass")
biomass_diagnostics <- diagnostics |> 
  select(all_of(biomass_cols)) |> 
  pivot_longer(cols = names(biomass_cols)[-1], names_to = "metric_name") |> 
  mutate(model_component = "biomass") 

occurrence_cols <- stringr::str_subset(colnames(diagnostics), pattern = "biomass", negate = T)
names(occurrence_cols) <- stringr::str_remove(occurrence_cols, "_p.occurrence")
occurrence_diagnostics <- diagnostics |> 
  select(all_of(occurrence_cols)) |>
  pivot_longer(cols = names(occurrence_cols)[-1], names_to = "metric_name") |> 
  mutate(model_component = "occurrence") 

diagnostics <- rbind(biomass_diagnostics, occurrence_diagnostics)

saveRDS(diagnostics, "data/iceland_fish_diagnostics.rds")




#alpha_fd_indices <- readRDS("./boot/data/iceland/alpha_fd_indices.rds")
# traits <- read_rds("./boot/data/iceland/traits.rds")
#functional_richness <- readRDS("./boot/data/iceland/f_ric.rds")

# alpha_files <- dir("boot/data/iceland/alpha_fd_indices")
# 
# alpha_fd_indices <- data.frame()
# for (i in seq_along(alpha_files)) {
#   tmp <- readRDS(paste0("boot/data/iceland/alpha_fd_indices/", alpha_files[i]))
#   alpha_fd_indices <- bind_rows(alpha_fd_indices, tmp$functional_diversity_indices)
# }
# alpha_communities <- rownames(alpha_fd_indices)
# alpha_fd_indices <- alpha_fd_indices |> mutate(community = as.numeric(stringr::str_remove(string = alpha_communities, "community_")))
# 
# 
# fric_files <- dir("boot/data/iceland/fric_pa/")
# fric_indices <- data.frame()
# for (i in seq_along(alpha_files)) {
#   tmp <- readRDS(paste0("boot/data/iceland/fric_pa/", fric_files[i]))
#   fric_indices <- bind_rows(fric_indices, tmp$functional_diversity_indices)
# }
# fric_communities <- rownames(fric_indices)
# fric_indices <- fric_indices |> mutate(community = as.numeric(stringr::str_remove(string = fric_communities, "community_")))

# tst <- readRDS(paste0("boot/data/iceland/alpha_fd_indices/", alpha_files[1]))
# str(tst$functional_diversity_indices)
#table(names(p.occurrence) == names(biomass))



# test <- p.occurrence |> mutate(community = 1:nrow(p.occurrence)) |> left_join(alpha_fd_indices, by = "community")
# test[!complete.cases(test),]




# # Taxonomic diversity
# threshold <- 0.5
# realized_occurrence <- p.occurrence %>%
#   mutate_at(.vars = species, # Apply transformation only to species
#             .funs = function(x) ifelse(x >= threshold, 1, 0))
# 
# richness <- cbind(realized_occurrence %>% select(-all_of(species)), # Keep time and location variables
#                   Richness = rowSums(realized_occurrence %>% select(all_of(species)))) # Sum of species occurrences
# 
# test <- 
# p.occurrence |> group_by(longitude, latitude, Ecoregion, Cell, centroid.cell_lon, centroid.cell_lat,  Year) |> arrange(.by_group = TRUE) |> summarize(across(species, ~ mean(.x, na.rm = TRUE)))
# 
# 
# #Shannon diversity
# shannon <- diversity(exp(biomass[, species]) - 1, index = "shannon")
# nSp <- rowSums(ifelse(biomass[, species] > 0, 1, 0))
# evenness <- shannon/log(nSp)
# 
# fish_diversity <- cbind(richness,
#                         potentialRichness = nSp,
#                         shannon = shannon,
#                         evenness = evenness) %>%
#   mutate(Year = as.numeric(as.character(Year)))
# 
# 
# # Prepare functional diversity indices
# #alpha_fd_indices$functional_diversity_indices$sp_richn
# #functional_richness$functional_diversity_indices
# 
# fish_diversity <- cbind(fish_diversity,
#                         fric_pa = functional_richness$functional_diversity_indices$fric,
#                         alpha_fd_indices$functional_diversity_indices
# )



#### Calculate trends

# r.slopes <- richness %>%
#   # define a unique identifier per location
#   mutate(id = paste(longitude, latitude, sep = "_"),
#          Year = as.numeric(as.character(Year))) %>%
#   # compute lm per location
#   split(.$id) %>%
#   purrr::map(~lm(Richness ~ Year, data = .x)) %>%
#   purrr::map_df(broom::tidy, .id = 'id') %>%
#   # Extract slope over time
#   filter(term == 'Year') %>%
#   # Recover longitude/latitude (now as unique identifier, i.e, id)
#   tidyr::separate(id, c("longitude", "latitude"), sep = "_") %>%
#   mutate(longitude = as.numeric(longitude),
#          latitude = as.numeric(latitude),
#          # Estimate confidence intervals
#          lowerCI = estimate - 1.96 * std.error,
#          upperCI = estimate + 1.96 * std.error)
# 
traits <- read_rds("./boot/data/iceland/traits.rds")
write.csv(traits, "data/iceland_fish_traits.csv")

write.csv(diagnostics, "data/iceland_fish_diagnostics.csv")




# Make zipfile for github release
zip(zipfile = "data/b_useful_data_bundle_nea", 
    files = c("Disclaimer_B-USEFUL.txt", 
              "data/iceland_fish_diagnostics.csv",
              "boot/data/iceland/traits.csv",
              "data/iceland_species_p_occurrence.parquet",
              "data/iceland_species_biomass.parquet",
              "data/iceland_fish_diversity.parquet",
              "data/iceland_fish_div_spatial.parquet")
)