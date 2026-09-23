library(tidyverse)
library(vegan)
library(mFD)
library(RColorBrewer)
library(knitr)
library(sf)
library(tidyr)
source("R/fcts.R")

path <- "./boot/data/east_med/east_med/"
em_p.occurrence <- read_rds(paste0(path, "easternmed_demersal_p_occurrence.rds"))
em_abundance <- read_rds(paste0(path, "easternmed_demersal_p_abundance.rds"))
em_alpha_fd_indices <- readRDS(paste0(path, "easternmed_demersal_alpha_fd_indices.rds"))
em_traits <- read_rds(paste0(path, "easternmed_demersal_traits.rds"))
em_functional_richness <- readRDS(paste0(path, "easternmed_demersal_fric_pa.rds"))
ices_ecoregions <- st_read("./boot/data/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp") %>% st_make_valid()

#make *diversity_dataframe

species <- names(em_p.occurrence)[!names(em_p.occurrence) %in%
                                 c("Year", "Ecoregion", "Cell", "longitude", "latitude", "Lon_c", "Lat_c")]

# Taxonomic diversity
threshold <- 0.5
realized_occurrence <- em_p.occurrence %>%
  mutate_at(.vars = species, # Apply transformation only to species
            .funs = function(x) ifelse(x >= threshold, 1, 0))

richness <- cbind(realized_occurrence %>% select(-all_of(species)), # Keep time and location variables
                  Richness = rowSums(realized_occurrence %>% select(all_of(species)))) # Sum of species occurrences




#Shannon diversity
#shannon <- diversity(exp(em_biomass[, species]) - 1, index = "shannon")
shannon <- diversity(em_abundance[, species], index = "shannon")
nSp <- rowSums(ifelse(em_abundance[, species] > 0, 1, 0))
evenness <- shannon/log(nSp)

fish_diversity <- cbind(richness,
                        potentialRichness = nSp,
                        shannon = shannon,
                        evenness = evenness) %>%
  mutate(Year = as.numeric(as.character(Year)),
         row_id = row_number())


# Prepare functional diversity indices

fish_diversity <- cbind(fish_diversity,
                        fric_pa = em_functional_richness$functional_diversity_indices$fric,
                        em_alpha_fd_indices$functional_diversity_indices
)

fish_diversity <- mutate(fish_diversity, 
                         longitude = Lon_c,
                         latitude = Lat_c,
                         row_id = row_number()
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

diversity_trends <- calculate_diversity_trend_new(fish_diversity, metrics = c("Richness", "shannon", "evenness", "fric", "feve", "fdis", "fdiv"))

# Prepare grids
grid_low_res <- fish_div_spatial %>% select(Cell, longitude, latitude) %>% unique()



###
library(worrms)


cols <- colnames(em_abundance)
traits <- readRDS("boot/data/east_med/east_med/easternmed_demersal_traits.rds")
species <- cols[!cols %in% c("Year", "Ecoregion", "Cell", "longitude", "latitude",  "Lon_c", "Lat_c")]

common_names <- wm_common_id_(name = traits$taxon)
common_names <- common_names |> filter(language_code == "eng") |> slice(.by = id, 1)

binomial <- traits$code
names(binomial) <- traits$taxon
em_abundance <- rename(em_abundance, all_of(binomial))
em_p.occurrence <- rename(em_p.occurrence, all_of(binomial))

#traits |> select(-c(length,lifespan,DepthPref,troph,TempPref)) |> cbind(common_names$vernacular)
easternmed_demersal_diagnostics <- readRDS(paste0(path, "easternmed_demersal_diagnostics.rds"))
easternmed_demersal_diagnostics <- easternmed_demersal_diagnostics |> left_join(traits[c("code", "taxon")], by = c("species" = "code"))
easternmed_demersal_diagnostics <- rename(easternmed_demersal_diagnostics, c(code = species, species = taxon))

# Save these in a different format?
# saveRDS(fish_diversity, file = "data/emed_demersal_diversity.rds")
# saveRDS(fish_div_spatial, file = "data/emed_demersal_div_spatial.rds")
# saveRDS(em_p.occurrence, file = "data/emed_species_p_occurrence.rds")
# saveRDS(em_abundance, "data/emed_species_abundance.rds")

arrow::write_parquet(fish_diversity,"data/emed_demersal_diversity.parquet")
sfarrow::st_write_parquet(fish_div_spatial,"data/emed_demersal_div_spatial.parquet")
arrow::write_parquet(em_p.occurrence,"data/emed_species_p_occurrence.parquet")
arrow::write_parquet(em_abundance,"data/emed_species_abundance.parquet")


saveRDS(diversity_trends, file = "data/emed_demersal_diversity_trends.rds")
saveRDS(grid_low_res, file = "data/emed_grid_low_res.rds")
saveRDS(easternmed_demersal_diagnostics, "data/emed_demersal_diagnostics.rds")

write.csv(easternmed_demersal_diagnostics,"data/emed_demersal_diagnostics.csv")
write.csv(traits,"data/emed_demersal_traits.csv")


# Make zipfile for github release
zip(zipfile = "data/b_useful_data_bundle_ce_med", 
    files = c("Disclaimer_B-USEFUL.txt", 
              "data/emed_demersal_diagnostics.csv",
              "data/emed_demersal_traits.csv",
              "data/emed_species_p_occurrence.parquet",
              "data/emed_species_abundance.parquet",
              "data/emed_demersal_diversity.parquet",
              "data/emed_demersal_div_spatial.parquet")
)
