library(tidyverse)
library(sf)

# Development version of colorspace is needed. Install via:
# install.packages("colorspace", repos = "http://R-Forge.R-project.org")
library(colorspace)

crs_goode <- "+proj=igh"

world_goode <- st_as_sf(rworldmap::getMap(resolution = "low")) %>%
  st_transform(crs = crs_goode) %>%
  mutate(
    area = st_area(geometry), # area in m^2
    popdens = POP_EST/area,
    logdens = log(as.numeric(popdens)*1e6)
  )


# projection outline in long-lat coordinates
lats <- c(
  90:-90, # right side down
  -90:0, 0:-90, # third cut bottom
  -90:0, 0:-90, # second cut bottom
  -90:0, 0:-90, # first cut bottom
  -90:90, # left side up
  90:0, 0:90, # cut top
  90 # close
)
longs <- c(
  rep(180, 181), # right side down
  rep(c(80.01, 79.99), each = 91), # third cut bottom
  rep(c(-19.99, -20.01), each = 91), # second cut bottom
  rep(c(-99.99, -100.01), each = 91), # first cut bottom
  rep(-180, 181), # left side up
  rep(c(-40.01, -39.99), each = 91), # cut top
  180 # close
)

goode_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc(
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_transform(crs = crs_goode)

# bounding box in transformed coordinates
xlim <- c(-21945470, 21963330)
ylim <- c(-9538022, 9266738)
goode_bbox <- 
  list(
    cbind(
      c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
      c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
    )
  ) %>%
  st_polygon() %>%
  st_sfc(crs = crs_goode)

# area outside the earth outline
goode_without <- st_difference(goode_bbox, goode_outline)

limit_range <- function(x, range = c(0, 1)) {
  x[x < range[1]] <- range[1]
  x[x > range[2]] <- range[2]
  x
}

ggplot(world_goode, aes(fill = logdens)) + 
  geom_sf(color = "black", size = 0.5/.pt) +
  geom_sf(data = goode_without, fill = "white", color = NA) +
  geom_sf(data = goode_outline, fill = NA, color = "grey30", size = 0.5/.pt) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL) +
  scale_fill_viridis_c(option = "B", oob = limit_range, limits = c(-1, 10)) +
  scale_fill_continuous_sequential(
    palette = "Lajolla", rev = TRUE, p1 = 2, p2 = 1.3,
    name = "population density\n(persons / square km)",
    limits = c(-1, 8),
    breaks = log(c(1, 10, 100, 1000, 10000)),
    labels = parse(text = c("10^0", "10^1", "10^2", "10^3", "10^4")),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      frame.colour = "black",
      ticks.colour = "white",
      barwidth = grid::unit(120, "pt"),
      barheight = grid::unit(20, "pt")
    )
  ) +
  coord_sf(xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE, ndiscr = 1000) + 
  ggtitle("World-wide population density") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#56B4E950", color = "white", size = 1),
    panel.grid.major = element_line(color = "gray30", size = 0.25),
    legend.position = "bottom"
  )








~