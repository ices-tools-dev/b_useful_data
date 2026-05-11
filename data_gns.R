library(tidyverse)
library(vegan)
library(mFD)
library(RColorBrewer)
library(knitr)
library(sf)
library(tidyr)
source("R/fcts.R")

p.occurrence <- read_rds("./boot/data/greater_north_sea/fish/p_occurrence.rds")
biomass <- read_rds("./boot/data/greater_north_sea/fish/biomass.rds")
alpha_fd_indices <- readRDS("./boot/data/greater_north_sea/fish/alpha_fd_indices.rds")
traits <- read_rds("./boot/data/greater_north_sea/fish/traits.rds")
functional_richness <- readRDS("./boot/data/greater_north_sea/fish/f_ric.rds")
ices_ecoregions <- st_read("./boot/data/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp") %>% st_make_valid()



#table(names(p.occurrence) == names(biomass))

species <- names(p.occurrence)[!names(p.occurrence) %in%
                                 c("Year", "Ecoregion", "Cell", "longitude", "latitude", "Lon_c", "Lat_c")]

# Taxonomic diversity
threshold <- 0.5
realized_occurrence <- p.occurrence %>%
  mutate_at(.vars = species, # Apply transformation only to species
            .funs = function(x) ifelse(x >= threshold, 1, 0))

richness <- cbind(realized_occurrence %>% select(-all_of(species)), # Keep time and location variables
                  Richness = rowSums(realized_occurrence %>% select(all_of(species)))) # Sum of species occurrences




#Shannon diversity
shannon <- diversity(exp(biomass[, species]) - 1, index = "shannon")
nSp <- rowSums(ifelse(biomass[, species] > 0, 1, 0))
evenness <- shannon/log(nSp)

fish_diversity <- cbind(richness,
                        potentialRichness = nSp,
                        shannon = shannon,
                        evenness = evenness) %>%
  mutate(Year = as.numeric(as.character(Year)))


# Prepare functional diversity indices
#alpha_fd_indices$functional_diversity_indices$sp_richn
#functional_richness$functional_diversity_indices

fish_diversity <- cbind(fish_diversity,
                        fric_pa = functional_richness$functional_diversity_indices$fric,
                        alpha_fd_indices$functional_diversity_indices
)

fish_div_spatial <- fish_diversity %>% st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ices_ecoregions))
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

diversity_trends <- calculate_diversity_trend(fish_diversity, metrics = c("Richness", "shannon", "evenness", "fric", "feve", "fdis", "fdiv"))

# Prepare grids
grid_low_res <- fish_div_spatial %>% select(Cell, Lon_c, Lat_c) %>% unique()

# Save these in a different format?
saveRDS(fish_diversity, file = "data/gns_fish_diversity.rds")
saveRDS(fish_div_spatial, file = "data/gns_fish_div_spatial.rds")
saveRDS(diversity_trends, file = "data/gns_fish_diversity_trends.rds")
saveRDS(grid_low_res, file = "data/gns_grid_low_res.rds")
rm(list = ls())