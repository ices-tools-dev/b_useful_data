library(rnaturalearth)
library(sf)

# 
# trawls <- data.frame()
# survey <- "NS-IBTS"
# trawl <- icesDatras::getHHdata(survey = "NS-IBTS", year = 2020, quarter = 3) %>%
#   select(lon = HaulLong, lat = HaulLat) %>%
#   as.matrix %>%
#   st_multipoint %>%
#   st_sfc(crs = 4945) %>%
#     st_sf(data.frame(survey = survey))
# 
# trawls <- rbind(trawls, trawl)


# trawl <- read.csv2("boot/data/MEDITS_stations.csv")
# trawl <- trawl |> mutate(lon = as.numeric(Longitude), lat = as.numeric(Latitude)) %>%
#   select(lon, lat) |>
#   as.matrix() %>%
#   st_multipoint %>%
#   st_sfc(crs = 4945) %>%
#   st_sf(data.frame(survey = "MEDITS"))
# trawls <- rbind(trawls, trawl)

preparedData  <- readRDS("boot/data/nea_haul_points.RDS")


trawl <- read.csv2("boot/data/MEDITS_stations.csv")

trawl <- trawl |> mutate(lon = as.numeric(Longitude), lat = as.numeric(Latitude),
                         region = "MEDITS",
                         haulid = paste(COUNTRY, AREA, YEAR, MONTH, DAY, HAUL_NUMBER),
                        Area = AREA,
                        Ecoregion = "Mediterranean", 
                        Year = NA, 
                        cell = NA,
                        Season = NA,
                        Month = NA) |> 
  select(colnames(preparedData))

preparedData <- rbind(preparedData, test
      )

set.seed(456)
dRegion <- preparedData %>%
  select(haulid, lon, lat, region) %>%
  unique() %>%
  group_by(region) %>%
  summarise(lon = median(lon),
            lat = median(lat)) %>%
  mutate(ranNum = as.factor(sample(1:length(unique(region)), replace = FALSE)))

mapData <- merge(preparedData,
                 dRegion %>% select(region, ranNum),
                 by = "region")

# coast <- ne_coastline(scale = "medium", returnclass = "sf")
# land <- ne_countries(scale = "medium", returnclass = "sf")

# daMap <- ggplot() +
#   geom_point(data = mapData,
#              aes(x = lon, y = lat, color = ranNum),
#              alpha = 0.25, shape = 1, size = 0.5) + # Transparency and small points to see overlapping surveys
#   geom_sf(data = land,
#           fill = "antiquewhite",
#           colour = NA) +
#   geom_sf(data = coast,
#           colour = "burlywood4",
#           linewidth = 0.3) +
#   annotate("rect",
#            xmin = -65, xmax = -40,
#            ymin = 80, ymax = 85,
#            fill = "antiquewhite",
#            colour = NA) +
#   coord_sf(
#     xlim = range(mapData$lon) + c(-3, 3),
#     ylim = range(mapData$lat) + c(-1, 1),
#     expand = FALSE
#   ) +
#   theme_bw() + labs(x = "Longitude", y = "Latitude") +
#   theme(legend.position = "none",
#         panel.grid = element_blank()
#   )
# 
# dSegment <- dRegion%>%
#   mutate( # LONGITUDE
#     lon = ifelse(region == "SWC-IBTS", -5, lon),
#     lon = ifelse(region == "Iceland", -13, lon),
#     lon = ifelse(region == "NS-IBTS", 0, lon),
#     lon = ifelse(region == "NorBTS", 18, lon),
#     # LATITUDE
#     lat = ifelse(region == "Greenland", 60, lat),
#     lat = ifelse(region == "IE-IGFS", 51, lat),
#     lat = ifelse(region == "Iceland", 66, lat),
#     lat = ifelse(region == "NS-IBTS", 61, lat),
#     lat = ifelse(region == "PT-IBTS", 40, lat),
#     lat = ifelse(region == "SWC-IBTS", 59, lat))%>%
#   mutate(
#     lonend = case_when(
#       region %in% c("IE-IGFS", "NIGFS") ~ lon - 15,
#       region %in% c("NorBTS") ~ lon - 12,
#       region %in% c("EVHOE", "PT-IBTS", "ROCKALL", "SP-NORTH", "SP-ARSA", "SWC-IBTS") ~ lon - 10,
#       region %in% c("NS-IBTS") ~ lon - 2,
#       ################################################!
#       region %in% c("FR-CGFS") ~ lon + 5,
#       TRUE ~ lon),  # Default value
#     
#     latend = case_when(
#       region %in% c("FR-CGFS", "Greenland") ~ lat - 3,
#       ################################################!
#       region %in% c("SWC-IBTS") ~ lat + 2,
#       region %in% c("Iceland", "NS-IBTS") ~ lat + 3,
#       TRUE ~ lat))
# 
# daMap +
#   geom_segment(data = dSegment, aes(x = lon, y = lat, xend = lonend, yend = latend), color = "white", linewidth = 1.25) + # Background line
#   geom_segment(data = dSegment, aes(x = lon, y = lat, xend = lonend, yend = latend), color = "black", linewidth = 0.75) +
#   geom_label(data = dSegment, aes(x = lonend, y = latend, label = region, color = ranNum), fontface = "bold", size = 5, label.size = 1.75) +
#   geom_label(data = dSegment, aes(x = lonend, y = latend, label = region), color = "black", fontface = "bold", size = 5, label.size = NA)

trawls <- st_as_sf(
  mapData,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
  )

trawls<- trawls |>
  group_by(region) |>
  summarise(
    geometry = st_combine(geometry),
    .groups = "drop"
  ) |>
  st_cast("MULTIPOINT")
save(trawls, file = "data/trawls.rda")
