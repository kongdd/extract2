#' @name sf_func
#' @title functions of zonal statistics
#' @importFrom plyr llply
#' @import exactextractr


#' @rdname sf_func
#' @export
sf_mean <- function(vals, fraction, .fun_sum = colSums, ...) {
  # mean(vals*fraction, na.rm = TRUE)
  ## whether to rm na first
  # ind = which(!is.na(vals))
  # .fun_sum(vals[ind]*fraction[ind], na.rm = TRUE)/sum(fraction[ind])
  .fun_sum(vals*fraction, na.rm = TRUE)/sum(fraction)
}

#' @rdname sf_func
#' @export
sf_sum <- function(vals, fraction, .fun_sum = colSums, ...) {
  .fun_sum(vals * fraction, na.rm = TRUE)
}

#' @param weights The default is grid area.
#'
#' @rdname sf_func
#' @export
sf_mean_weighted <- function(vals, fraction, weights, .fun_sum = colSums, ...) {
  # e.g. vals is runoff (mm/y), weights is area (km^2)
  weights = weights * fraction
  .fun_sum(vals * weights, na.rm = TRUE) / sum(weights)
}

#' @rdname sf_func
#' @export
sf_sum_weighted <- function(vals, fraction, weights, .fun_sum = colSums, ...) {
  weights <- weights * fraction
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
    names(ret)[3] = "fraction"
    dim = dim(ret$fraction)
    ret$nrow = dim[1]
    ret$ncol = dim[2]
    ret$fraction = as.vector(t(ret$fraction))
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
  nbands = length(names(r))
  .fun_sum = if (nbands > 1) colSums else sum
  n = length(geoms)
  # res = list()
  # for (i in 1:n) {
  res = llply(1:n, function(i){
    ret <- geoms[[i]]
    vals <- raster::getValuesBlock(r,
      row = ret$row,
      col = ret$col,
      nrow = ret$nrow, ncol = ret$ncol
    ) #%>% as.matrix()
    # # res[[i]] <-
    fun(vals, ret$fraction, weights = ret$area, .fun_sum = .fun_sum)
  }, .progress = "text")
  do.call(rbind, res) %>% as.data.frame()
}

setGeneric("extract2", function(r, geoms, ...) standardGeneric("extract2"))

#' @rdname extract2
#' @export
setMethod("extract2", signature(r = "Raster"), .extract2)

#' @export
setMethod("extract2", signature(r = "RasterStack"), .extract2)

#' @export
setMethod("extract2", signature(r = "list"), function(r, geoms, ...) {
  res = list()
  for(i in 1:length(r)) {
    runningId(i)
    res[[i]] <- extract2(r[[i]], geoms, ...)
  }
  setNames(res, names(r))
})

#' @export
setMethod("extract2", signature(r = "character"), function(r, geoms, ...) {
  if ("sf" %in% class(geoms)) {
    # if geoms is sf
    geoms <- sf::st_as_binary(sf::st_geometry(geoms), EWKB = TRUE)
  }
  if ("WKB" %in% class(geoms)) {
    b <- raster(r[[1]]) %>% readAll()
    geoms <- overlap(b, shp_wkb)
  }

  res = list()
  for(i in 1:length(r)) {
    runningId(i)
    b <- brick(r[[i]]) %>% readAll()
    res[[i]] <- extract2(b, geoms, ...)
  }
  setNames(res, names(r))
})

runningId <- function(i, step = 1, N, prefix = "") {
  perc <- ifelse(missing(N), "", sprintf(", %.1f%% ", i/N*100))
  if (mod(i, step) == 0) cat(sprintf("[%s] running%s %d ...\n", prefix, perc, i))
}
