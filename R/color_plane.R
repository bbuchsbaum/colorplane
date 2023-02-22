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
IntensityColorPlane <- function(intensity, cols=rainbow(255), alpha=1) {
  new("IntensityColorPlane", intensity=intensity, colmap=cols, alpha=alpha)
}


#' DiscreteColorPlane
#'
#' DiscreteColorPlane constructor
#'
#' @param values the discrete values of the color map
#' @param lookup a "lookup table", which is a named list mapping discrete values to hex colors
#' @param alpha a value from 0 to 1 indicating transparency level
#' @param cmap a hex color vector
#' @importFrom methods new
#' @export
DiscreteColorPlane <- function(values, lookup=NULL, alpha=1, cmap=rainbow(length(unique(values[!is.na(values)])))) {
  values <- as.integer(values)
  if (is.null(lookup)) {
    valid_vals <- values[!is.na(values)]

    nuniq <- length(unique(valid_vals))
    uvals <- sort(unique(valid_vals))

    if (is.null(cmap)) {
      cmap <- rainbow(nuniq)
    }

    names(cmap) <- as.character(uvals)
    lookup <- as.list(cmap)
  }

  cols <- lookup[as.character(values)]
  cols[sapply(cols, is.null)] <- "#00000000"

  new("DiscreteColorPlane", values=values, cols=cols, alpha=alpha)
}

#' RGBColorPlane
#'
#' RGBColorPlane constructor
#' @param clr a field of colors
#' @export
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
#' ConstantColorPlane constructor
#'
#' @param clr a single hex color as a `character` vector
#' @export
#' @rdname ConstColorPlane-class
ConstantColorPlane <- function(clr) {
  stopifnot(is.character(clr))
  new("ConstantColorPlane", clr=clr)
}

#' HexColorPlane
#'
#' HexColorPlane constructor
#' @param clr a field of colors
#' @export
#' @rdname HexColorPlane-class
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
rgb2hex <- function(r,g,b, alpha) rgb(r, g, b, alpha, maxColorValue = 255)

#' @export
#' @rdname blend_colors-methods
setMethod("blend_colors", signature(bottom="ColorPlane", top="ColorPlane", alpha="numeric"),
          def=function(bottom, top, alpha=1) {

            rgb1 <- as_rgb(bottom)
            rgb2 <- as_rgb(top)

            achan <- alpha_channel(top)
            alpha <- achan * alpha

            clr <- (1-alpha)*rgb1[,1:3,drop=FALSE] + alpha*rgb2[,1:3,drop=FALSE]
            RGBColorPlane(clr)

          })


#' @export
#' @rdname blend_colors-methods
setMethod("blend_colors", signature(bottom="ColorPlane", top="ColorPlane", alpha="missing"),
          def=function(bottom, top) {

            rgb1 <- as_rgb(bottom)
            rgb2 <- as_rgb(top)

            alpha <- alpha_channel(top)
            clr <- (1-alpha)*rgb1[,1:3,drop=FALSE] + alpha*rgb2[,1:3,drop=FALSE]
            RGBColorPlane(clr)

          })

#' @export
#' @rdname blend_colors-methods
setMethod("blend_colors", signature(bottom="HexColorPlane", top="RGBColorPlane", alpha="numeric"),
          def=function(bottom, top, alpha) {
            bottom <- as_rgb(bottom)

            alpha <- alpha_channel(top) * alpha
            ## multiple constant alpha with alpha channel of top level
            clr <- (1-alpha)*bottom[,1:3,drop=FALSE] + alpha*top@clr[,1:3,drop=FALSE]
            RGBColorPlane(clr)
          })


#' @export
#' @rdname color-conversion
setMethod("as_rgb", signature(x="RGBColorPlane"),
          def=function(x) x@clr)

#' @export
#' @rdname color-conversion
setMethod("as_rgb", signature(x="HexColorPlane"),
          def=function(x) t(col2rgb(x@clr, alpha=TRUE)))

#' @export
#' @rdname color-conversion
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
#' @rdname color-conversion
setMethod("as_hexcol", signature(x="HexColorPlane"),
          def=function(x) x@clr)

#' @export
#' @param normalize divide by 255
#' @rdname alpha_channel-methods
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
setMethod("map_colors", signature=c("DiscreteColorPlane"),
          def=function(x, ...) {
            x@cols
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
