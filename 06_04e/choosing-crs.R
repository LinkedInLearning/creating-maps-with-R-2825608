library(tidyverse)
library(rnaturalearthdata)
library(sf)

oceania_sf <- countries110 %>% 
  st_as_sf() %>% 
  filter(name %in% c("Australia", "New Zealand"))

quakes_4326 <- quakes %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)


quakes_4326 %>% 
  st_transform(crs = 4959)

crs_quakes <- 'PROJCS["ProjWiz_Custom_Albers",
                     GEOGCS["GCS_WGS_1984",
                            DATUM["D_WGS_1984",
                                  SPHEROID["WGS_1984",6378137.0,298.257223563]],
                            PRIMEM["Greenwich",0.0],
                            UNIT["Degree",0.0174532925199433]],
                     PROJECTION["Albers"],
                     PARAMETER["False_Easting",0.0],
                     PARAMETER["False_Northing",0.0],
                     PARAMETER["Central_Meridian",153.3691406],
                     PARAMETER["Standard_Parallel_1",-40.9315112],
                     PARAMETER["Standard_Parallel_2",-8.0456775],
                     PARAMETER["Latitude_Of_Origin",-24.4885943],
                     UNIT["Meter",1.0]]'

quakes_4326 %>% 
  st_transform(crs = crs_quakes)

ggplot() +
  geom_sf(data = oceania_sf) +
  geom_sf(data = quakes_4326)
