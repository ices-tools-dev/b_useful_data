library(tidyverse)
library(vegan)
library(mFD)
library(RColorBrewer)
library(knitr)
library(sf)
library(tidyr)
source("R/fcts.R")

path <- "./boot/data/west_med/west_med/"
wm_p.occurrence <- read_rds(paste0(path, "westernmed_demersal_p_occurrence.rds"))
wm_abundance <- read_rds(paste0(path, "westernmed_demersal_abundance.rds"))
wm_alpha_fd_indices <- readRDS(paste0(path, "westernmed_demersal_alpha_fd_indicators.rds"))
wm_traits <- read_rds(paste0(path, "westernmed_demersal_traits.rds"))
wm_functional_richness <- readRDS(paste0(path, "westernmed_demersal_f_ric_pa.rds"))
ices_ecoregions <- st_read("./boot/data/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp") %>% st_make_valid()

#make *diversity_dataframe

species <- names(wm_p.occurrence)[!names(wm_p.occurrence) %in%
                                 c("Year", "Ecoregion", "Cell", "longitude", "latitude", "Lon_c", "Lat_c")]

# Taxonomic diversity
threshold <- 0.5
realized_occurrence <- wm_p.occurrence %>%
  mutate_at(.vars = species, # Apply transformation only to species
            .funs = function(x) ifelse(x >= threshold, 1, 0))

richness <- cbind(realized_occurrence %>% select(-all_of(species)), # Keep time and location variables
                  Richness = rowSums(realized_occurrence %>% select(all_of(species)))) # Sum of species occurrences




#Shannon diversity
corrected_abundance <- wm_abundance
zero_abundance <- corrected_abundance[,species]
zero_abundance[zero_abundance <0] <- 0
corrected_abundance[,species] <- zero_abundance

shannon <- diversity(corrected_abundance[, species], index = "shannon")
nSp <- rowSums(ifelse(corrected_abundance[, species] > 0, 1, 0))
evenness <- shannon/log(nSp)

fish_diversity <- cbind(richness,
                        potentialRichness = nSp,
                        shannon = shannon,
                        evenness = evenness) %>%
  mutate(Year = as.numeric(as.character(Year)))



  
# Prepare functional diversity indices

# fish_diversity <- cbind(fish_diversity,
#                         fric_pa = wm_functional_richness$functional_diversity_indices$fric,
#                         wm_alpha_fd_indices$functional_diversity_indices
# )

fric_pa <- wm_functional_richness$functional_diversity_indices
fric_pa$community <- rownames(fric_pa)

fd_indices <- wm_alpha_fd_indices$functional_diversity_indices
fd_indices$community <- rownames(fd_indices)


fish_diversity <- fish_diversity |> mutate(community = paste0("community_", row_number())) |> 
  left_join(fric_pa, by = "community") |> 
  left_join(fd_indices, by = "community") |> 
  select(-c(community, sp_richn.x, sp_richn.y, fric.y)) |> 
  rename(fric = fric.x)

fish_diversity <- mutate(fish_diversity, 
                         longitude = Lon_c,
                         latitude = Lat_c,
                         row_id = row_number())

arrow::write_parquet(fish_diversity,"data/wmed_demersal_diversity.parquet")

fish_div_spatial <- fish_diversity %>% st_as_sf(coords = c("Lon_c", "Lat_c"), crs = st_crs(ices_ecoregions))
fish_div_spatial <-  fish_div_spatial %>%  st_join(ices_ecoregions)
sfarrow::st_write_parquet(fish_div_spatial,"data/wmed_demersal_div_spatial.parquet")


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


#calculate trends
diversity_trends <- calculate_diversity_trend_new(fish_diversity, metrics = c("Richness", "shannon", "evenness", "fric", "feve", "fdis", "fdiv"))
saveRDS(diversity_trends, file = "data/wmed_demersal_diversity_trends.rds")


# Prepare grids
grid_low_res <- fish_div_spatial %>% select(Cell, longitude, latitude) %>% unique()
saveRDS(grid_low_res, file = "data/wmed_grid_low_res.rds")



cols <- colnames(wm_abundance)
traits <- readRDS("boot/data/west_med/west_med/westernmed_demersal_traits.rds")
traits <- cbind("code" = row.names(traits), traits)
species <- cols[!cols %in% c("Year", "Ecoregion", "Cell", "longitude", "latitude",  "Lon_c", "Lat_c")]

# common_names <- wm_common_id_(name = traits$taxon)
# common_names <- common_names |> filter(language_code == "eng") |> slice(.by = id, 1)

binomial <- rownames(traits)
names(binomial) <- traits$taxon

wm_abundance <- rename(wm_abundance, all_of(binomial)) |> 
  mutate(longitude = Lon_c,
         latitude = Lat_c)
arrow::write_parquet(wm_abundance,"data/wmed_species_abundance.parquet")


wm_p.occurrence <- rename(wm_p.occurrence, all_of(binomial))|> 
  mutate(longitude = Lon_c,
         latitude = Lat_c)
arrow::write_parquet(wm_p.occurrence,"data/wmed_species_p_occurrence.parquet")


wmed_demersal_diagnostics <- readRDS("boot/data/west_med/west_med/westernmed_demersal_diagnostics.rds")
wmed_demersal_diagnostics <- wmed_demersal_diagnostics |> left_join(traits[c("code", "taxon")], by = c("species" = "code"))
wmed_demersal_diagnostics <- rename(wmed_demersal_diagnostics, c(code = species, species = taxon))
saveRDS(wmed_demersal_diagnostics, "data/wmed_demersal_diagnostics.rds")

write.csv(wmed_demersal_diagnostics,"data/wmed_demersal_diagnostics.csv")
write.csv(traits,"data/wmed_demersal_traits.csv")


# Make zipfile for github release
zip(zipfile = "data/b_useful_data_bundle_wmed", 
    files = c("Disclaimer_B-USEFUL.txt", 
              "data/wmed_demersal_diagnostics.csv",
              "data/wmed_demersal_traits.csv",
              "data/wmed_species_p_occurrence.parquet",
              "data/wmed_species_abundance.parquet",
              "data/wmed_demersal_diversity.parquet",
              "data/wmed_demersal_div_spatial.parquet")
)

# Save these in a different format?
# saveRDS(fish_diversity, file = "data/wmed_demersal_diversity.rds")
# saveRDS(fish_div_spatial, file = "data/wmed_demersal_div_spatial.rds")
# saveRDS(wm_abundance, "data/wmed_species_biomass.rds")
# saveRDS(wm_p.occurrence, "data/wmed_species_p_occurrence.rds")





#rm(list = ls())


