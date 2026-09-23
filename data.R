# Prepare data, write CSV data tables

# Before:
# After:

library(icesTAF)

mkdir("data")

#MPAs
cp(from = "boot/data/mpas/", to = "data/")

source("data_ecoregion_shape.R")
source("data_gns.R")
source("data_wmed.R")
source("data_emed.R")
source("data_nea.R")
source("data_iceland.R")
source("data_trawls.R")

source("data_years_species.R")

#source("data_assembly.R")

