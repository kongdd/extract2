library(raster)
library(stars)
library(magrittr)
library(sf)

files = dir("N:/Research/hydro/globalRunoff/data-raw/PMLV2/lastest-v016", "*.tif$", full.names = TRUE)
r <- brick(files[1]) %>% readAll()

basins  <- read_sf("N:/Research/hydro/globalRunoff/data-raw/shp/GlobalCatchments_9559basins_(2000-2017).shp")[,1]
shp_wkb <- sf::st_as_binary(sf::st_geometry(basins), EWKB = TRUE)
# library(foreach)
t_new = system.time({
  # system.time(geoms <- overlap(r, shp_wkb))
  for (i in 1:10) {
    runningId(i)
    res <- extract2(r, geoms)
  }
})

# t_old = system.time({
#   d = exact_extract(r, basins)
# })

# ret = geoms[[1]]
# res = .exact_extract(r, y, "mean")
# ggplot(basins[1, ]) +
#   geom_sf(color = "red", fill = "transparent") +
#   coord_sf(xlim = c(142.9, 143.3), ylim = c(-13.1, -12.7))

# plot(basins[1, ], axes = TRUE)
# abline(v = seq(142.9, 143.3, 0.1)[1])
