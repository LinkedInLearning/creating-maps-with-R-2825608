## from https://rpubs.com/dieghernan/beautifulmaps_I

make_hex_bins <- function(sf,
                         to = "fishnet",
                         gridsize = as.integer(
                           min(
                             diff(st_bbox(sf)[c(1, 3)]),
                             diff(st_bbox(sf)[c(2, 4)])
                           ) / 40
                         ),
                         sliver = 0.5) {
  if (!unique(st_geometry_type(sf)) %in% c("POLYGON", "MULTIPOLYGON")) {
    stop("Input should be  MULTIPOLYGON or POLYGON")
  }
  if (!to %in% c("fishnet", "puzzle", "honeycomb", "hexbin", "pixel")) {
    stop("'to' should be 'fishnet','puzzle','honeycomb','hexbin' or 'pixel'")
  }
  
  if (class(sf)[1] == "sf") {
    initial <- sf
    initial$index_target <- 1:nrow(initial)
  } else {
    initial <- st_sf(index_target = 1:length(sf), geom = sf)
  }
  
  target <- st_geometry(initial)
  
  if (to %in% c("fishnet", "puzzle")) {
    sq <- T
  } else {
    sq <- F
  }
  if (to == "pixel") {
    grid <- st_make_grid(target,
                         gridsize,
                         crs = st_crs(initial),
                         what = "centers"
    )
  } else {
    grid <- st_make_grid(
      target,
      gridsize,
      crs = st_crs(initial),
      what = "polygons",
      square = sq
    )
  }
  grid <- st_sf(index = 1:length(lengths(grid)), grid) # Add index
  if (to == "pixel") {
    cent_merge <- st_join(grid, initial["index_target"], left = F)
    grid_new <- st_buffer(cent_merge, gridsize / 2)
  } else {
    cent_grid <- st_centroid(grid)
    cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
    grid_new <- inner_join(grid, st_drop_geometry(cent_merge))
  }
  if (to %in% c("fishnet", "honeycomb", "pixel")) {
    geom <- aggregate(
      grid_new,
      by = list(grid_new$index_target),
      FUN = min,
      do_union = FALSE
    )
  } else {
    geom <- aggregate(
      st_buffer(grid_new, sliver),
      by = list(grid_new$index_target),
      FUN = min,
      do_union = TRUE
    )
  }
  if (class(initial)[1] == "sf") {
    fin <- left_join(
      geom %>% select(index_target),
      st_drop_geometry(initial)
    ) %>%
      select(-index_target)
    fin <- st_cast(fin, "MULTIPOLYGON")
    return(fin)
  } else {
    fin <- st_cast(geom, "MULTIPOLYGON")
    return(st_geometry(fin))
  }
}