# Extract results of interest, write CSV output tables

# Before:
# After:

mkdir("output")

metric_label <- c("Species Richness" = "richness", 
                  "Species Richness" = "Richness",
                  "Evenness" = "evenness",
                  "Shannon Index" = "shannon",
                  "Functional Richness" = "fric",
                  "Functional Evenness" = "feve",
                  "Functional Divergence" = "fdiv",
                  "Functional Dispersion" = "fdis",
                  "Species Richness Trend" = "richness_trend", 
                  "Species Richness Trend" = "Richness_trend",
                  "Evenness Trend" = "evenness_trend",
                  "Shannon Index Trend" = "shannon_trend",
                  "Functional Richness Trend" = "fric_trend",
                  "Functional Evenness Trend" = "feve_trend",
                  "Functional Divergence Trend" = "fdiv_trend",
                  "Functional Dispersion Trend" = "fdis_trend")


load("data/map_shape.rda")
source("R/fcts.R")
source("output_w_med.R")
source("output_e_med.R")
source("output_nea.R")

# source("output_gns.R")
#source("output_pngs_gifs.R")

#source("output_species.R")


