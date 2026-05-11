# Output gifs and pngs

ecoregions <- c("greater_north_sea", "west_med")
metrics <- c("Richness", "shannon", "evenness", "fric_pa", "fric", "feve", "fdis", "fdiv")

taxon <- c("w_med" = "demersal")
dat <- readRDS("data/wmed_demersal_diversity.rds")

yr <- unique(dat$Year)
plot_width <- 1300
plot_height <- 1000
dpi <- 144


for(i in 1:length(ecoregions)){
  #ecoregion_code <- eco_code_by_name[ecoregions[i]]
  ecoregion_code <- "w_med"
  # zip_sf(dat = diversity[[ecoregions[i]]],
  #        directory = "data",
  #        zip_name = paste0("b_useful_data", ecoregion_code),
  #        fname = paste("b_useful_biodiversity", ecoregion_code, Sys.Date(), sep = "_")
  # )
  for(j in 1:length(metrics)) {
    #dat <- diversity[[ecoregions[i]]]
    metric_name  <- tolower(metrics[j])
    metric_name  <- str_replace_all(metric_name, " ", "_")
    map_parameters <- calculate_map_parameters(dat)
    colour_limits <- range(dat[,metrics[j]])
    name_of_file <- paste0("output/", paste(ecoregion_code, taxon, metric_name, "status", sep = "_"), ".gif")
    # create plot safely; skip saving plot if warnings as "skip"
    result <-  tryCatch({
      anim <- make_biodiversity_gif(dat,
                                    metric = metrics[j],
                                    #ecoregion_shape = ecoregion_shp[ecoregion_shp$Ecoregion == ecoregions[i],],
                                    #land_shape = atlantic_land_shp,
                                    #crs = CRS_LAEA_EUROPE,
                                    map_parameters = map_parameters,
                                    colour_limits = colour_limits)
    },
    warning = function(w) {
      warning(sprintf("Plot creation for %s produced a warning: %s", name_of_file, conditionMessage(w)))
      NULL
    },
    error = function(e) {
      warning(sprintf("Plot creation for %s failed: %s", name_of_file, conditionMessage(e)))
      NULL
    })
    if (!is.null(result) && inherits(result, "ggplot")) {
      animate(anim, nframes = length(yr), fps = length(yr)/10)
      anim_save(filename = name_of_file, nframes = length(yr))
      try(grDevices::dev.off())
    } else {
      message("Not saving ", name_of_file, " — plot creation returned NULL or not a ggplot")
    }
    for (k in 1:length(yr)){

      name_of_file <- paste0("output/", paste(ecoregion_code, taxon, metric_name, "status", yr[k],sep = "_"), ".png")

      # create plot safely; skip saving plot if warnings as "skip"
      result <-  tryCatch({

        p <- plot_biodiversity_metric(dat,
                                 metric = metrics[j],
                                 #ecoregion_shape = ecoregion_shp[ecoregion_shp$Ecoregion == ecoregions[i],],
                                 #land_shape = atlantic_land_shp,
                                 #crs = CRS_LAEA_EUROPE,
                                 map_parameters = map_parameters,
                                 colour_limits = colour_limits,
                                 year = yr[k])


        ggplot2::ggplot_build(p)
        p

      },
      warning = function(w) {
        warning(sprintf("Plot creation for %s produced a warning: %s", name_of_file, conditionMessage(w)))
        NULL
      },
      error = function(e) {
        warning(sprintf("Plot creation for %s failed: %s", name_of_file, conditionMessage(e)))
        NULL
      })

      if (!is.null(result) && inherits(result, "ggplot")) {
        ragg::agg_png(filename = name_of_file, units = "px",width = plot_width, height = plot_height, res = dpi)
        print(result)
        grDevices::dev.off()

      } else {
        message("Not saving ", name_of_file, " — plot creation returned NULL or not a ggplot")
      }
    }

  }
}