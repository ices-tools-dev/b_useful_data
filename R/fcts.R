library(dplyr)
library(gganimate)

ecoregion_lookup <- tibble::tribble(
  ~code, ~name,
  "BtS", "Baltic Sea",
  "BI",  "Bay of Biscay and the Iberian Coast",
  "BoB", "Bay of Biscay",
  "IW",  "Iberian Waters",
  "CS",  "Celtic Seas",
  "CSx", "Celtic Sea",
  "IrS", "Irish Sea",
  "NrS", "Greater North Sea",
  "NwS", "Norwegian Sea",
  "IS",  "Icelandic Waters",
  "BrS", "Barents Sea",
  "GS",  "Greenland Sea",
  "FO",  "Faroes",
  "ONA", "Oceanic Northeast Atlantic",
  "AZ",  "Azores"
)

# Derive the two lookups from the table (useful in multiple places)
eco_name_by_code <- setNames(ecoregion_lookup$name, ecoregion_lookup$code)  # code -> name
eco_code_by_name <- setNames(ecoregion_lookup$code, ecoregion_lookup$name)  # name -> code


calculate_diversity_trend <- function(biodiversity_data, metrics){
  coord_digits <- 4 
  biodiversity_data <- biodiversity_data %>%
    mutate(
      longitude = round(longitude, coord_digits),
      latitude  = round(latitude,  coord_digits),
      Year = as.numeric(as.character(Year)),
      id = paste(longitude, latitude, sep = "_")
    )
  out <- biodiversity_data[,c("Ecoregion", "Cell", "longitude", "latitude", "Lon_c", "Lat_c", "id")]
  
  for(i in 1:length(metrics)) {
  
  metric_name <- metrics[i]
  slopes <- biodiversity_data %>%
    # compute lm per location
    split(.$id) %>%
    map(~lm(as.formula(paste(metrics[i], "~ Year")), data = .x)) %>% 
    purrr::map_df(broom::tidy, .id = 'id') %>%
    # Extract slope over time
    filter(term == 'Year') %>%
    # Recover longitude/latitude (now as unique identifier, i.e, id)
    mutate(!!paste0(metric_name,"_trend") := estimate) %>% 
    select(-c("estimate", "std.error", "statistic", "p.value", "term"))
    out <- dplyr::left_join(slopes, out, by = "id", multiple = "first") %>% 
      relocate(c("Ecoregion", "Cell", "longitude", "latitude", "Lon_c", "Lat_c"),.before = "id")

  }
  out
}




#' Title
#'
#' @param biodiversity_data 
#' @param metric 
#' @param year 
#' @param map_parameters 
#' @param colour_limits 
#'
#' @returns
#' @import ggplot2
#' @export
#'  
#'
#' @examples
plot_biodiversity_metric <- function(biodiversity_data, metric, map_parameters, colour_limits, year = NULL) {

  if(!is.null(year)){
    
    dat <- biodiversity_data %>% 
      filter(Year %in% year)
  } else{
    dat <- biodiversity_data
  }
  
  p <- ggplot() +
    geom_point(data = dat, aes(x = longitude, y = latitude, color = !!rlang::sym(metric)), size = 2) +
    borders(fill = "grey", colour = "grey") + 
    coord_quickmap(xlim = range(dat$longitude),
                   ylim = range(dat$latitude))+
    scale_color_gradientn(colours = rev(brewer.pal(11, "RdYlBu")), 
                          limits = colour_limits)+
    #geom_sf(data = map_shape, fill = "grey")+
    # scale_x_continuous(breaks= map_parameters$coordxmap)+
    # scale_y_continuous(breaks= map_parameters$coordymap,expand=c(0,0))+
    # coord_sf(xlim=c(map_parameters$coordslim[1], map_parameters$coordslim[2]), ylim=c(map_parameters$coordslim[3],map_parameters$coordslim[4]))+
    ylab("Latitude")+
    xlab("Longitude") 
  
  if(!is.null(year)){
    p <- p + facet_wrap(~ Year)
  }
  return(p)
    
    facet_wrap(~Year)
  p

}

#' Title
#'
#' @param biodiversity_data 
#' @param metric 
#' @param year 
#' @param map_parameters 
#' @param colour_limits 
#' @import ggplot gganimate
#' @returns
#' @export
#'
#' @examples
make_biodiversity_gif <- function(biodiversity_data, metric, year, map_parameters, colour_limits) {

  p <- ggplot() +
    geom_point(data = dat, aes(x = longitude, y = latitude, color = !!rlang::sym(metric)), size = 2) +
    borders(fill = "grey", colour = "grey") + 
    coord_quickmap(xlim = range(dat$longitude),
                   ylim = range(dat$latitude))+
    scale_color_gradientn(colours = rev(brewer.pal(11, "RdYlBu")), 
                          limits = colour_limits)+
    #geom_sf(data = map_shape, fill = "grey")+
    # scale_x_continuous(breaks= map_parameters$coordxmap)+
    # scale_y_continuous(breaks= map_parameters$coordymap,expand=c(0,0))+
    # coord_sf(xlim=c(map_parameters$coordslim[1], map_parameters$coordslim[2]), ylim=c(map_parameters$coordslim[3],map_parameters$coordslim[4]))+
    ylab("Latitude")+
    xlab("Longitude") 
  
  anim <- p + transition_time(Year)+
    ggtitle("Year: {round(frame_time, digits=0)}")

}



plotFun <- function(data, variable, years = NULL, size = 0.5){
  if(!is.null(years)){
    data <- data %>%
      filter(Year %in% years)
  }
  
  p <- ggplot(data,
              aes(x = longitude, y = latitude, color = .data[[variable]])) +
    borders(fill = "grey", colour = "grey") + 
    coord_quickmap(xlim = range(data$longitude),
                   ylim = range(data$latitude)) +
    geom_point(size = size) +
    scale_color_gradientn(colours = rev(brewer.pal(11, "RdYlBu")))
  
  if(!is.null(years)){
    p <- p + facet_wrap(~ Year)
  }
  return(p)
}

calculate_map_parameters <- function(dat) {
  
  minlong <- min(dat$longitude) 
  maxlong <- max(dat$longitude)
  long_range <- maxlong-minlong
  minlong <- minlong -0.05*long_range
  maxlong <- maxlong +0.05*long_range
  
  minlat <- min(dat$latitude)
  maxlat <- max(dat$latitude)
  lat_range <- maxlat-minlat
  minlat <- minlat -0.05*lat_range
  maxlat <- maxlat +0.05*lat_range
  
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 5))
  coordymap <- round(seq(minlat,maxlat,length.out = 5))
  
  list(coordslim = coordslim,
                         coordxmap = coordxmap,
                         coordymap = coordymap)
}
