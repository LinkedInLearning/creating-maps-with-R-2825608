library(sf)
library(tictoc)
library(tidyverse)

london_boroughs_sf <- read_sf("data/london_boroughs")

london_age_finished_edu <- read_csv("data/london_age_finished_edu.csv")

london_education_sf <- london_boroughs_sf %>%
  left_join(london_age_finished_edu,
            by = c("lad11nm" = "area")) 

tic()
london_dot_density_100 <- london_education_sf %>%
  st_transform(4326) %>%
  sf_dot_density(age_group, scale = 100, parallel = TRUE)
toc("scale = 100")

gg_london_dot_density_school_leavers <- ggplot() +
  geom_sf(data = london_education_sf,
          fill = "gray90",
          color = "transparent") +
  geom_sf(data = london_dot_density_100,
          aes(fill = age_group,
              color = age_group),
          size = 0.05,
          shape = 21,
          alpha = 0.8) +
  geom_sf(data = london_education_sf,
          fill = "transparent") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(override.aes = list(size = 4,
                                                 color = "black",
                                                 shape = 21),
                             title = ""),
         color = guide_none()) +
  labs(title = "When do people in London leave school?",
       subtitle = "1 dot represents 100 people") +
  theme_void() +
  theme_ipsum(base_size = 20) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "top")

gg_london_dot_density_school_leavers %>% 
  ggsave("gg_london_dot_density_school_leavers.png",
         .,
         width = 4.46 * 2,
         height = 4 * 2)
