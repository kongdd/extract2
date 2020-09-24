library(raster)
library(stars)
library(magrittr)
library(sf)
library(stringr)
library(foreach)
library(iterators)
library(purrr)
library(tidyverse)
library(data.table)

str_year <- function(files) {str_extract(basename(files), "\\d{4}")}

ggplot_trend <- function(df, variableName = "GPP", color = "green"){
    lab_et = expression("ET (mm"^-1~y^-1*")")
    lab_gpp = expression("GPP (gC"~m^-2~y^-1*")")
    lab_y = ifelse(variableName == "GPP", lab_gpp, lab_et)
  # stat_format <- "b[0]~`=`~%.3g*\", \"*b[1]~`=`~%.3g*\", pvalue\"~`=`~\"%.3f\""
  stat_format <- "Slope~`=`~\"%.2f\"*\", pvalue\"~`=`~\"%.3f\""
  p <- ggplot(df, aes_string("year", variableName, group = "continent")) +
    geom_line(color = color, linetype = 2) + geom_point() +
    facet_wrap(~continent, scales = "free_y") +
    scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2019)) +
    labs(y = lab_y, x = "Year") +
    geom_smooth(method = "lm", formula = y~x, color = color, size = 0.5) +
    stat_poly_eq(
      # mapping = aes(label = sprintf(stat_format,
      #                               map(stat(coef.ls), ~.[[1, "Estimate"]]),
      #                               map(stat(coef.ls), ~.[[2, "Estimate"]]),
      #                               stat(p.value))),
      mapping = aes(label = sprintf(stat_format,
                                    map(stat(coef.ls), ~.[[2, "Estimate"]]),
                                    stat(p.value))),
      formula = y~x,
      label.x = 0.03,
      label.y = 0.95,
      output.type = "numeric",
      parse = TRUE
    )
    # stat_fit_glance(aes(label = sprintf('R^2~"="~%.3f*","~italic(P)~"="~%.3f',
    #                                     stat(r.squared), stat(p.value))),
    #                 method.args = list(formula = y ~ x),
    #                 parse = TRUE,
    #                 label.x = "left", label.y = "bottom")
  p
}

## -----------------------------------------------------------------------------

indir = "C:/Users/kongdd/Google 云端硬盘（kongdd.sysu@gmail.com）/PMLV2_yearly"
files = dir(indir, "*.tif$", full.names = TRUE) %>% set_names(., str_year(.))

polys <- read_sf("data-raw/shp/Continents.shp")[-8, 1]
polys <- rbind(polys, st_sf(CONTINENT = "Global", geometry = st_combine(polys[1:7, ])))

# add the polygon union, global
shp_wkb <- sf::st_as_binary(sf::st_geometry(polys), EWKB = TRUE)

# 1. get the overlap grids fraction and area
r = raster(files[1])
geoms <- overlap(r, shp_wkb)

res = foreach(file = files, i = icount()) %do% {
    runningId(i)
    r <- brick(file) %>% readAll()
    r$ET = rowSums(as.matrix(r)[, 2:4], na.rm = TRUE) # not include ET_water
    r2 = r[[c("GPP", "ET")]]
    # exact_extract(r2[[1]], polys, "mean")
    extract2(r2, geoms)
}

Continents = c("Africa", "Asia", "Australia", "Europe", "North America", "South America", "Global")
levels = paste0("(", letters[1:length(Continents)], ") ", Continents)
df = map(res, ~cbind(continent = polys$CONTINENT, .)) %>% melt_list("year") %>%
  data.table() %>%
  mutate(year = as.numeric(year), continent = factor(continent, Continents, levels)) %>%
  .[!is.na(continent), ]

{
  theme_set( theme_bw(base_size = 14) +
               theme(
                 strip.text = element_text(face = 2),
                 panel.grid.major = element_line(size = 0.2),
                 panel.grid.minor = element_blank()
               ))
    p_gpp = ggplot_trend(df, "GPP", "green")
    p_et = ggplot_trend(df, "ET", "blue")
    write_fig(p_gpp, "Figure1_GPP_annual_variation.pdf", 10, 6)
    write_fig(p_et, "Figure1_ET_annual_variation.pdf", 10, 6)
}
