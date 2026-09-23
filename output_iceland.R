# Output gifs and pngs

#ecoregions <- c("greater_north_sea", "west_med")
metrics <- c("Richness", "shannon", "evenness", "fric", "feve", "fdis", "fdiv")

taxon <- c("north_east_atlantic" = "fish")

#dat <- readRDS("data/nea_fish_diversity.rds")
dat <- arrow::read_parquet("data/iceland_fish_diversity.parquet")

yr <- unique(dat$Year)
plot_width <- 1300
plot_height <- 1000
dpi <- 144
ecoregion_code <- "iceland"

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
}
