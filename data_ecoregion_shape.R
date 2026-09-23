library(sf)

map_shape <- sf::st_read(dsn = "boot/data/shape_worldmap",
                         layer = "ne_50m_land")

save(map_shape, file = "data/map_shape.rda")
