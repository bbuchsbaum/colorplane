#'
#' # ColorScale
#' #
#' # ColorScale constructor
#' #
#' # export
#' # param irange the intensity range of the scale
#' # param threshold the alpha thresholding range
#' # param clr a vector of hex colors
#' # import methods
#' ColorScale <- function(irange=c(-3, 3), threshold=c(0,0), clr=rainbow(12)) {
#'   assertthat::assert_that(irange[2] >= irange[1])
#'   assertthat::assert_that(threshold[2] >= threshold[1])
#'
#'   new("ColorScale", irange=irange, threshold=threshold, clr=clr)
#' }
#'
#'
#' # export
#' # rdname get_color-methods
#' setMethod("get_color", signature(x="ColorScale", v="numeric"),
#'           function(x,v,...) {
#'             delta <- x@irange[2] - x@irange[1]
#'             frac <- (v - x@irange[1])/delta
#'             frac <- ifelse(frac < 0, 0, frac)
#'             frac <- ifelse(frac > 1, 1, frac)
#'             bin <- frac * (length(x@clr)-1) + 1
#'
#'             sapply(seq_along(v), function(i) {
#'               if (v[i] <= x@threshold[1] || v[i] >= x@threshold[2]) {
#'                 x@clr[bin[i]]
#'               } else {
#'                 "#00000000"
#'               }
#'             })
#'
#'             #thr <- ifelse(v < x@threshold[1] & v > x@threshold[2], )
#'             #clr[bin]
#'           })
