#' @include all_class.R
#' @include all_generic.R
NULL



#' IntensityColorPlane
#'
#' IntensityColorPlane constructor
#'
#' @param intensity a numeric vector of intensity values
#' @param cols a vector of hex character codes
#' @param alpha a vector of alpha values ranging from 0 to 1
#' @export
#' @rdname IntensityColorPlane-class
#' @importFrom grDevices rainbow rgb
#'
#' @return a new \code{\linkS4class{IntensityColorPlane}} instance
IntensityColorPlane <- function(intensity, cols=rainbow(255), alpha=1) {
  new("IntensityColorPlane", intensity=intensity, colmap=cols, alpha=alpha)
}


#' DiscreteColorPlane
#'
#' DiscreteColorPlane constructor taking list with names mapping to color values in hex representation.
#' This object is used when one has a one to one mapping between discrete set of strings/values to discrete set of colors.
#'
#' @param lookup a "lookup table", which is a named list mapping discrete values to hex colors
#' @importFrom methods new
#' @export
#' @rdname DiscreteColorPlane-class
#' @return a new \code{\linkS4class{DiscreteColorPlane}} instance
#'
#' @examples
#'
#' lookup <- as.list(col2hex(c("red", "blue", "green")))
#' names(lookup) <- c("a", "b", "c")
#' cp <- DiscreteColorPlane(lookup)
#'
#' values <- c("a", "b", "c", "a", "c")
DiscreteColorPlane <- function(lookup) {
  if (!is.list(lookup)) {
    stop("`lookup` must be a list from keys (as list names) --> colors (as list elements) ")
  }
  assert_that(all(!is.null(names(lookup))), msg="`lookup` must be a named list, where the names are lookup keys")

  new("DiscreteColorPlane", lookup=lookup)
}

#' RGBColorPlane
#'
#' RGBColorPlane constructor taking a 3- or 4-column numeric \code{matrix} of RGB(A) colors in the 0-255 range.
#'
#' @param clr a matrix of colors where the first column is red, second column is green, third column is blue, and optional fourth column is alpha.
#' @export
#' @rdname RGBColorPlane-class
#' @return a new \code{\linkS4class{RGBColorPlane}} instance
#' @examples
#'
#' rgba_cmat <- rbind(c(255,0,0,255),
#'               c(0, 255, 0, 255),
#'               c(0, 0, 255, 0))
#'
#' cp <- RGBColorPlane(rgba_cmat)
#' stopifnot(all(cp@clr[1,] == c(255,0,0,255)))
#'
RGBColorPlane <- function(clr) {
  stopifnot(is.matrix(clr))
  if (ncol(clr) == 3) {
    clr <- cbind(clr, 255)
  }

  if (ncol(clr) != 4) {
    stop("'ncol' of 'clr' must be 4 (r,g,b,a)")
  }
  new("RGBColorPlane", clr=clr)
}


#' ConstantColorPlane
#'
#' ConstantColorPlane constructor taking a single hex `character` vector defining a constant color plane.
#'
#' @param clr a single hex color as a `character` vector of length 1 defining the constant color.
#' @export
#' @rdname ConstantColorPlane-class
#' @return a new \code{\linkS4class{ConstantColorPlane}} instance
ConstantColorPlane <- function(clr) {
  stopifnot(is.character(clr))
  stopifnot(length(clr) == 1)
  new("ConstantColorPlane", clr=clr)
}

#' HexColorPlane
#'
#' HexColorPlane constructor taking a `character` vector of colors to define a color plane.
#'
#' @param clr a vector of hex colors
#' @export
#' @rdname HexColorPlane-class
#' @return a new \code{\linkS4class{HexColorPlane}} instance
HexColorPlane <- function(clr) {
  stopifnot(is.character(clr))
  new("HexColorPlane", clr=clr)
}

#' convert rgb colors to hex colors
#'
#' @param r the red color component
#' @param g the green color component
#' @param b the blue color component
#' @param alpha the alpha component
#' @return a hex color represenation as `character` vector
rgb2hex <- function(r,g,b, alpha) rgb(r, g, b, alpha, maxColorValue = 255)


#' convert color name to hex character string
#'
#' @param cname one or more color names, e.g. "red"
#' @param alpha the value of the alpha channel, ranging from 0 to 1 (default is 1)
#' @return a vector of hex color values, one per color name
#' @export
col2hex <- function(cname, alpha=1) {
  cmat <- col2rgb(cname)
  rgb(red = cmat[1,]/255, blue=cmat[2,]/255, green=cmat[3,]/255, alpha=rep(alpha, ncol(cmat)))
}


#' multiply rgb matrix with alpha channel

#' @keywords internal
#' @param rgb matrix of colors
#' @param alpha channel
multiply_alpha <- function(rgb, alpha) {
  sweep(rgb, 1, alpha, "*")
}



#' @export
#' @rdname blend_colors-methods
#' @return a new \code{\linkS4class{ColorPlane}} instance with `top` and `bottom` alpha-blended.
setMethod("blend_colors", signature(bottom="ColorPlane", top="ColorPlane", alpha="numeric"),
          def=function(bottom, top, alpha=1) {
            assert_that(alpha >= 0 && alpha <= 1)

            bchan <- alpha_channel(bottom)
            achan <- alpha_channel(top) * alpha

            rgb1 <- as_rgb(bottom)
            rgb2 <- as_rgb(top)


            ao <- achan + bchan*(1-achan)

            clr <- multiply_alpha(rgb2[,1:3,drop=FALSE], achan) + multiply_alpha(rgb1[,1:3,drop=FALSE], bchan * (1-achan))
            clr <- sweep(clr, 1, ao, "/")
            #clr <- (1-alpha)*rgb1[,1:3,drop=FALSE] + alpha*rgb2[,1:3,drop=FALSE]
            RGBColorPlane(clr)

          })


#' @export
#' @rdname blend_colors-methods
#' @importFrom methods callGeneric
setMethod("blend_colors", signature(bottom="ColorPlane", top="ColorPlane", alpha="missing"),
          def=function(bottom, top) {
            callGeneric(bottom, top, alpha=1)
          })

#' @export
#' @rdname blend_colors-methods
setMethod("blend_colors", signature(bottom="HexColorPlane", top="RGBColorPlane", alpha="numeric"),
          def=function(bottom, top, alpha) {
            bottom <- RGBColorPlane(as_rgb(bottom))
            callGeneric(bottom, top, alpha)
            #alpha <- alpha_channel(top) * alpha
            ## multiple constant alpha with alpha channel of top level
            #clr <- (1-alpha)*bottom[,1:3,drop=FALSE] + alpha*top@clr[,1:3,drop=FALSE]
            #RGBColorPlane(clr)
          })

#' @export
#' @rdname blend_colors-methods
setMethod("blend_colors", signature(bottom="HexColorPlane", top="ConstantColorPlane", alpha="numeric"),
          def=function(bottom, top, alpha=1) {

            rgbtop <- t(replicate(length(bottom@clr), as_rgb(top), simplify=TRUE))

            callGeneric(RGBColorPlane(as_rgb(bottom)), RGBColorPlane(rgbtop), alpha)
            #alpha <- alpha_channel(top) * alpha
            ## multiple constant alpha with alpha channel of top level
            #topclr <- as_rgb(top)
            #clr <- (1-alpha)*bottom[,1:3,drop=FALSE] + matrix(rep(alpha,nrow(bottom)), nrow(bottom)) %*% topclr[,1:3,drop=FALSE]
            #RGBColorPlane(clr)
          })


#' @export
#' @rdname as_rgb-methods
#' @return a numeric matrix of rgb components
setMethod("as_rgb", signature(x="RGBColorPlane"),
          def=function(x) x@clr)

#' @export
#' @rdname as_rgb-methods
setMethod("as_rgb", signature(x="HexColorPlane"),
          def=function(x) t(col2rgb(x@clr, alpha=TRUE)))

#' @export
#' @rdname as_rgb-methods
setMethod("as_rgb", signature(x="ConstantColorPlane"),
          def=function(x) t(col2rgb(x@clr, alpha=TRUE)))


#' @export
#' @rdname as_hexcol-methods
setMethod("as_hexcol", signature(x="RGBColorPlane"),
          def=function(x) {
            if (ncol(x@clr) == 4) {
              rgb2hex(x@clr[,1], x@clr[,2], x@clr[,3], x@clr[,4])
            } else if (ncol(x@clr) == 3) {
              rgb2hex(x@clr[,1], x@clr[,2], x@clr[,3])
            } else {
              stop("as_hexcol: 'x' must have 3 or 4 columns")
            }
          })

#' @export
#' @rdname as_hexcol-methods
setMethod("as_hexcol", signature(x="HexColorPlane"),
          def=function(x) x@clr)

#' @export
#' @param normalize divide by 255
#' @rdname alpha_channel-methods
#' @return a numeric vector of alpha channel values
setMethod("alpha_channel", signature(x="HexColorPlane"),
          def=function(x, normalize=TRUE) {
            if (normalize) {
              col2rgb(x@clr, alpha=TRUE)[4,]/255
            } else {
              col2rgb(x@clr, alpha=TRUE)[4,]
            }
          })


#' @export
#' @param normalize divide by 255
#' @rdname alpha_channel-methods
setMethod("alpha_channel", signature(x="ConstantColorPlane"),
          def=function(x, normalize=TRUE) {
            if (normalize) {
              col2rgb(x@clr, alpha=TRUE)[4,]/255
            } else {
              col2rgb(x@clr, alpha=TRUE)[4,]
            }
          })


#' @export
#' @param normalize divide by 255
#' @rdname alpha_channel-methods
setMethod("alpha_channel", signature(x="RGBColorPlane"),
          def=function(x, normalize=TRUE) {
            if (normalize) {
              x@clr[,4]/255
            } else {
              x@clr[,4]
            }
          })


#' @export
#' @rdname map_colors-methods
setMethod("map_colors", signature=c("ConstantColorPlane"),
          def=function(x) {
            new("HexColorPlane", clr=x@clr)
          })

#' @export
#' @rdname map_colors-methods
setMethod("map_colors", signature=c("HexColorPlane"),
          def=function(x) {
            x
          })

#' @export
#' @param values the values to map to colors via the discrete lookup table
#' @rdname map_colors-methods
setMethod("map_colors", signature=c("DiscreteColorPlane"),
          def=function(x, values, ...) {
            clrs <- as.vector(x@lookup[values])

            wh <- which(sapply(clrs, is.null))
            if (length(wh) > 0) {
              clrs[wh] <- "000000FF"
            }
            new("HexColorPlane", clr=unlist(clrs))
          })


#' @export
#' @importFrom grDevices col2rgb
#' @import assertthat
#' @rdname map_colors-methods
#' @param alpha alpha multiplier from 0 to 1.
#' @param threshold two-sided threshold as a 2-element vector, e.g. `threshold=c(-3,3)` indicating two-sided transparency thresholds.
#' @param irange the intensity range defining min and max of scale.
setMethod("map_colors", signature=c("IntensityColorPlane"),
          def=function(x, alpha=1, threshold=NULL, irange=NULL) {
            assertthat::assert_that(alpha >=0 && alpha <= 1)
            if (is.null(irange)) {
              irange <- range(x@intensity, na.rm=TRUE)
              clr <- x@colmap[as.integer((x@intensity - irange[1])/ diff(irange) * (length(x@colmap) -1) + 1)]
              clr[is.na(clr)] <- "#00000000"
            } else {
              assertthat::assert_that(irange[2] >= irange[1])
              full_range <- range(x@intensity, na.rm=TRUE)
              #irange <- c(max(irange[1], full_range[1]), min(irange[2], full_range[2]))
              icol <- as.integer((x@intensity - irange[1])/diff(irange) * (length(x@colmap) -1) + 1)
              icol[icol < 1] <- 1
              icol[icol > length(x@colmap)] <- length(x@colmap)
              clr <- x@colmap[icol]
              clr[is.na(icol)] <- "#00000000"
              clr
            }



            if (!is.null(threshold)) {
              clr <- col2rgb(clr, alpha=TRUE)
              if (alpha < 1) {
                clr[4,] <- clr[4,] * alpha
              }
              if (length(threshold) == 1) {
                trans <- x@intensity < threshold
                clr[4,trans] <- 0

              } else if (length(threshold) == 2) {
                #cat("thresholding ", threshold)
                trans <- x@intensity > threshold[1] & x@intensity < threshold[2]
                clr[4,trans] <- 0
              } else {
                stop("threshold must be a numeric vector with 1 or 2 elements")
              }

              new("RGBColorPlane", clr=t(clr))

            } else {
              new("HexColorPlane", clr=clr)
            }


          })
