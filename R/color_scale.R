
#' @export
ColorScale <- function(irange=c(-3, 3), threshold=c(0,0), clrs=rainbow(12)) {
  assertthat::assert_that(irange[2] >= irange[1])
  assertthat::assert_that(threshold[2] >= threshold[1])
  
  new("ColorScale", irange=irange, threshold=threshold, clrs=clrs)
}


#' @export
setMethod("get_color", signature(x="ColorScale", v="numeric"),
          function(x,v,...) {
            delta <- irange[2] - irange[1]
            frac <- (v - irange[1])/delta
            frac <- ifelse(frac < 0, 0, frac)
            frac <- ifelse(frac > 1, 1, frac)
            bin <- frac * (length(x@clrs)-1) + 1
            
            sapply(seq_along(v), function(i) {
              if (v[i] <= x@threshold[1] || v[i] >= x@threshold[2]) {
                x@clrs[bin[i]]
              } else {
                "#00000000"
              }
            })
            
            #thr <- ifelse(v < x@threshold[1] & v > x@threshold[2], ) 
            #clrs[bin]
          })