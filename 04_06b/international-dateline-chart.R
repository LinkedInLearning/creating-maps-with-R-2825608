library(tidyverse)
library(sf)
library(rnaturalearthdata)

countries_sf <- countries110 %>% 
  st_as_sf()

international_date_line_sf <- read_sf("data/ne_10m_geographic_lines") %>% 
  filter(name == "International Date Line")


gg_date_line <- ggplot() +
  geom_sf(data = countries_sf,
          fill = "darkolivegreen3",
          color = "transparent") +
  geom_sf(data = international_date_line_sf) +
  geom_vline(xintercept = 180,
             linetype = "dotted") +
  theme_void() +
  scale_y_continuous(limits = c(-80, 80)) +
  theme(plot.background = element_rect(fill = "white",
                                       color = "white"))


gg_date_line +
  theme(panel.background = element_rect(fill = "pink"), background)


gg_date_line %>% 
  ggsave("gg_date_line.png",
         .,
         width = 12,
         height = 6)
