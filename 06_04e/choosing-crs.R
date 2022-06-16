library(tidyverse)
library(rnaturalearthdata)
library(sf)

oceania_sf <- countries110 %>% 
  st_as_sf() %>% 
  filter(name %in% c("Australia", "New Zealand"))

quakes_4326 <- quakes %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)

ggplot() +
  geom_sf(data = oceania_sf) +
  geom_sf(data = quakes_4326)

quakes_4326 %>% 
  st_transform(crs = 7844)

crs_quakes <- 'PROJCS["quales-dataset_Equidistant_Conic",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Equidistant_Conic"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",146.25],
 PARAMETER["Standard_Parallel_1",-40.2354813],
 PARAMETER["Standard_Parallel_2",-7.3439988],
 PARAMETER["Latitude_Of_Origin",-23.78974],
 UNIT["Meter",1.0]]'

quakes_4326 %>% 
  st_transform(crs = crs_quakes)

