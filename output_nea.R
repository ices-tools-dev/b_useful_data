# Output gifs and pngs

#ecoregions <- c("greater_north_sea", "west_med")
metrics <- c("Richness", "shannon", "evenness", "fric", "feve", "fdis", "fdiv")

taxon <- c("north_east_atlantic" = "fish")

dat <- readRDS("data/nea_fish_diversity.rds")

yr <- unique(dat$Year)
plot_width <- 1300
plot_height <- 1000
dpi <- 144


#for(i in 1:length(ecoregions)){
  #ecoregion_code <- eco_code_by_name[ecoregions[i]]
  ecoregion_code <- "nea"
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
                                    metric_label = names(metric_label[metric_label==metrics[j]]),
                                    map_shape = map_shape,
                                    map_parameters = map_parameters,
                                    colour_limits = colour_limits,
                                    pts_size = 1)
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
      rendered <- animate(
        anim,
        nframes = length(yr),
        fps = length(yr) / 10,
        width = plot_width,
        height = plot_height,
        units = "px",
        res = dpi,
        device = "ragg_png",
        renderer = gifski_renderer()
      )
      
      anim_save(
        filename = name_of_file,
        animation = rendered
      )
      
      try(grDevices::dev.off())
    } else {
      message("Not saving ", name_of_file, " — plot creation returned NULL or not a ggplot")
    }
    # for (k in 1:length(yr)){
    #   
    #   name_of_file <- paste0("output/", paste(ecoregion_code, taxon, metric_name, "status", yr[k],sep = "_"), ".png")
    #   
    #   # create plot safely; skip saving plot if warnings as "skip"
    #   result <-  tryCatch({
    #     
    #     p <- plot_biodiversity_metric(dat,
    #                                   metric = metrics[j],
    #                                   #ecoregion_shape = ecoregion_shp[ecoregion_shp$Ecoregion == ecoregions[i],],
    #                                   #land_shape = atlantic_land_shp,
    #                                   #crs = CRS_LAEA_EUROPE,
    #                                   map_parameters = map_parameters,
    #                                   colour_limits = colour_limits,
    #                                   year = yr[k])
    #     
    #     
    #     ggplot2::ggplot_build(p)
    #     p
    #     
    #   },
    #   warning = function(w) {
    #     warning(sprintf("Plot creation for %s produced a warning: %s", name_of_file, conditionMessage(w)))
    #     NULL
    #   },
    #   error = function(e) {
    #     warning(sprintf("Plot creation for %s failed: %s", name_of_file, conditionMessage(e)))
    #     NULL
    #   })
    #   
    #   if (!is.null(result) && inherits(result, "ggplot")) {
    #     ragg::agg_png(filename = name_of_file, units = "px",width = plot_width, height = plot_height, res = dpi)
    #     print(result)
    #     grDevices::dev.off()
    #     
    #   } else {
    #     message("Not saving ", name_of_file, " — plot creation returned NULL or not a ggplot")
    #   }
    # }
    
  }
#}
  
# SDM maps
nea_p_occurrence <- readRDS("data/nea_fish_p_occurrence.rds")
#nea_p_occurrence <- rename(nea_p_occurrence, longitude = Lon_c, latitude = Lat_c)
species <- colnames(nea_p_occurrence)[!colnames(nea_p_occurrence) %in% c("Year", "Ecoregion", "Ecoregion.x", "Cell", "longitude", "latitude", "Lon_c", "Lat_c")]
load("data/map_shape.rda")
yr <- unique(nea_p_occurrence$Year)
yr <- yr[as.numeric(as.character(yr)) %% 5 == 0]
map_params_list <- list()
regions <- unique(nea_p_occurrence$ecoregion)

for(i in 1:length(regions)) {
  if(!regions[i] %in% ices_ecoregions$Ecoregion){
    params <-   calculate_map_parameters(dplyr::filter(nea_p_occurrence, ecoregion == regions[i]))
  } else {
    bbox <- sf::st_bbox(ices_ecoregions[ices_ecoregions$Ecoregion == regions[i],])
    tmp_dat <- data.frame(longitude = bbox[c(1,3)],
                          latitude = bbox[c(2,4)])
    params <- calculate_map_parameters(tmp_dat)
  }
  map_params_list[[i]] <- params
  names(map_params_list)[i] <- regions[i]
}

for(i in 3:50){#seq_along(species)){
  
  spec_name <- stringr::str_replace_all(species[i], pattern = " ", replacement = "_")
  map_parameters <- calculate_map_parameters(nea_p_occurrence)
  ecoregions <- unique(nea_p_occurrence$ecoregion)
  
  for(k in 1:length(yr)) {
    
      name_of_file <- paste0("output/", paste("nea", spec_name, yr[k],sep = "_"), ".png")
      # create plot safely; skip saving plot if warnings as "skip"
      result <-  tryCatch({
        
        p <- plot_sdm(dat = nea_p_occurrence,
                      species = species[i],
                      map_parameters = map_parameters, 
                      map_shape = map_shape,
                      colour_limits = colour_limits,
                      year = yr[k],
                      model_type = "presence_absence"
                      )
        
        
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
    for (j in seq_along(ecoregions)){
    
      ecoregion <- ecoregions[j]
      name_of_regional_file <- paste0("output/", paste("nea",ecoregion, spec_name, yr[k],sep = "_"), ".png")
      filtered_data <- dplyr::filter(nea_p_occurrence, ecoregion == .env$ecoregion)
      
      # create plot safely; skip saving plot if warnings as "skip"
      result <-  tryCatch({
        
        p <- plot_sdm(dat = filtered_data,
                      species = species[i],
                      map_parameters = map_params_list[[j]], 
                      map_shape = map_shape,
                      colour_limits = colour_limits,
                      year = yr[k],
                      model_type = "presence_absence"
                      )
        
        
        ggplot2::ggplot_build(p)
        print(paste0("i = ", i))
        print(paste0("j = ", j))
        print(paste0("k = ", k))
        p
      },
      warning = function(w) {
        warning(sprintf("Plot creation for %s produced a warning: %s", name_of_regional_file, conditionMessage(w)))
        NULL
      },
      error = function(e) {
        warning(sprintf("Plot creation for %s failed: %s", name_of_regional_file, conditionMessage(e)))
        NULL
      })
      
      if (!is.null(result) && inherits(result, "ggplot")) {
        ragg::agg_png(filename = name_of_regional_file, units = "px",width = plot_width, height = plot_height, res = dpi)
        print(result)
        grDevices::dev.off()
        
      } else {
        message("Not saving ", name_of_file, " — plot creation returned NULL or not a ggplot")
      }
    
  }
  }
}
