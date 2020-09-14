#' @name sf_func
#' @title functions of zonal statistics
#' @importFrom plyr llply
#' @import exactextractr


#' @rdname sf_func
#' @export 
sf_mean <- function(vals, cov_fracs, .fun_sum = colSums, ...) {
  # n = nrow(vals)
  # mean(vals*cov_fracs, na.rm = TRUE)
  .fun_sum(vals*cov_fracs, na.rm = TRUE)/sum(cov_fracs)
}

#' @rdname sf_func
#' @export 
sf_sum <- function(vals, cov_fracs, .fun_sum = colSums, ...) {
  # n = nrow(vals)
  # mean(vals*cov_fracs, na.rm = TRUE)
  .fun_sum(vals * cov_fracs, na.rm = TRUE)
}

#' @rdname sf_func
#' @export 
sf_mean_weighted <- function(vals, cov_fracs, weights, .fun_sum = colSums, ...) {
  # n = nrow(vals)
  # e.g. vals is runoff (mm/y), weights is area (km^2)
  # mean(vals*cov_fracs, na.rm = TRUE)
  weights = weights * cov_fracs
  .fun_sum(vals * weights, na.rm = TRUE) / sum(weights)
}

#' @rdname sf_func
#' @export 
sf_sum_weighted <- function(vals, cov_fracs, weights, .fun_sum = colSums, ...) {
  # n = nrow(vals)
  # e.g. vals is runoff (mm/y), weights is area (km^2)
  # mean(vals*cov_fracs, na.rm = TRUE)
  weights <- weights * cov_fracs
  .fun_sum(vals * weights, na.rm = TRUE)
}

#' Get the detailed information of overlapped grids 
#' 
#' @param r Raster or RasterBrick object
#' @param shp WKB object, e.g. `sf::st_as_binary(sf::st_geometry(basins), EWKB=TRUE)`
#' 
#' @return geoms
#' - `weights`: fraction percentage
#' - `area`: area in km^2
#' - `row`,`col`: begining `row` and `col` of the overlapped region
#' - `nrow`,`ncol`: number of the rows and columns of the overlapped region
#' 
#' @importFrom plyr llply
#' @export
overlap <- function(r, shp) {
  area = raster::area(r)
  geoms <- llply(shp, function(wkb) {
    ret <- exactextractr:::CPP_exact_extract(r, wkb)
    dim = dim(ret$weights)
    ret$nrow = dim[1]
    ret$ncol = dim[2]
    ret$weights = as.vector(t(ret$weights))
    ret$area <- raster::getValuesBlock(area,
      row = ret$row,
      col = ret$col,
      nrow = ret$nrow, ncol = ret$ncol)
    ret
  }, .progress = "text")
  geoms
}

#' @name extract2
#' @title Get zonal statistics from raster
#' 
#' @param r Raster or RasterBrick object
#' @param shp WKB object, e.g. `sf::st_as_binary(sf::st_geometry(basins), EWKB=TRUE)`
#' 
#' @export 
NULL

.extract2 <- function(r, geoms, fun = sf_mean_weighted, ...) {
  .fun_sum = if (nbands(r) > 1) colSums else sum
  n = length(geoms)
  res = list()
  for (i in 1:n) {
    ret <- geoms[[i]]
    vals <- raster::getValuesBlock(r,
      row = ret$row,
      col = ret$col,
      nrow = ret$nrow, ncol = ret$ncol
    ) #%>% as.matrix()
    res[[i]] <- fun(vals, ret$weights, ret$area, .fun_sum = .fun_sum)
  }
  do.call(rbind, res) %>% as.data.frame()
}

setGeneric("extract2", function(r, geoms, ...) standardGeneric("extract2"))

#' @rdname extract2
#' @export
setMethod("extract2", signature(r = "Raster"), .extract2)

#' @export
setMethod("extract2", signature(r = "list"), function(r, geoms, ...) {
  res = list()
  for(i in 1:length(r)) {
    runningId(i)
    res[[i]] <- extract2(r[[i]], geoms, ...)
  }
  setNames(res, names(r))
})

runningId <- function(i, step = 1, N, prefix = "") {
  perc <- ifelse(missing(N), "", sprintf(", %.1f%% ", i/N*100))
  if (mod(i, step) == 0) cat(sprintf("[%s] running%s %d ...\n", prefix, perc, i))    
}
