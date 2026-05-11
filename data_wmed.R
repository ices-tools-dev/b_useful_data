library(tidyverse)
library(vegan)
library(mFD)
library(RColorBrewer)
library(knitr)
library(sf)
library(tidyr)
source("R/fcts.R")

path <- "./boot/initial/data/west_med/west_med/"
wm_p.occurrence <- read_rds(paste0(path, "westernmed_demersal_p_occurrence.rds"))
wm_biomass <- read_rds(paste0(path, "westernmed_demersal_biomass.rds"))
wm_alpha_fd_indices <- readRDS(paste0(path, "westernmed_demersal_alpha_fd_indices.rds"))
wm_traits <- read_rds(paste0(path, "westernmed_demersal_traits.rds"))
wm_functional_richness <- readRDS(paste0(path, "westernmed_demersal_f_ric.rds"))
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
shannon <- diversity(exp(wm_biomass[, species]) - 1, index = "shannon")
nSp <- rowSums(ifelse(wm_biomass[, species] > 0, 1, 0))
evenness <- shannon/log(nSp)

fish_diversity <- cbind(richness,
                        potentialRichness = nSp,
                        shannon = shannon,
                        evenness = evenness) %>%
  mutate(Year = as.numeric(as.character(Year)))


# Prepare functional diversity indices

fish_diversity <- cbind(fish_diversity,
                        fric_pa = wm_functional_richness$functional_diversity_indices$fric,
                        wm_alpha_fd_indices$functional_diversity_indices
)

fish_diversity <- mutate(fish_diversity, 
                         longitude = Lon_c,
                         latitude = Lat_c
                         )

fish_div_spatial <- fish_diversity %>% st_as_sf(coords = c("Lon_c", "Lat_c"), crs = st_crs(ices_ecoregions))
fish_div_spatial <-  fish_div_spatial %>%  st_join(ices_ecoregions)


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
diversity_trends <- calculate_diversity_trend(fish_diversity, metrics = c("Richness", "shannon", "evenness", "fric", "feve", "fdis", "fdiv"))

# Prepare grids
grid_low_res <- fish_div_spatial %>% select(Cell, longitude, latitude) %>% unique()

# Save these in a different format?
saveRDS(fish_diversity, file = "data/wmed_demersal_diversity.rds")
saveRDS(fish_div_spatial, file = "data/wmed_demersal_div_spatial.rds")
saveRDS(diversity_trends, file = "data/wmed_demersal_diversity_trends.rds")
saveRDS(grid_low_res, file = "data/wmed_grid_low_res.rds")

rm(list = ls())