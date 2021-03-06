#' @include all_class.R
#' @include all_generic.R
NULL


#' IntensityColorPlane constructor
#' 
#' @export
IntensityColorPlane <- function(intensity, cols=rainbow(255), alpha=1) {
  new("IntensityColorPlane", intensity=intensity, colmap=cols, alpha=alpha)
}

#' DiscreteColorPlane constructor
#' 
#' @param the discrete values of the color map
#' @param lookup a "lookup table", which is a named list mapping discrete values to hex colors
#' @param alpha a value from 0 to 1 indicating transparency level
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


#' RGBColorPlane constructor
#' @export
RGBColorPlane <- function(clrs) {
  stopifnot(is.matrix(clrs))
  if (ncol(clrs) == 3) {
    clrs <- cbind(clrs, 255)
  }
  
  if (ncol(clrs) != 4) {
    stop("'ncol' of 'clrs' must be 4 (r,g,b,a)")
  }
  new("RGBColorPlane", clrs=clrs)
}

#' ConstantColorPlane constructor
#' @export
ConstantColorPlane <- function(clr) {
  stopifnot(is.character(clr))
  new("ConstantColorPlane", clrs=clr)
}

#' HexColorPlane constructor
#' @export
HexColorPlane <- function(clr) {
  stopifnot(is.character(clr))
  new("HexColorPlane", clrs=clr)
}

#' convert rgb colors to hex colors
#' @export
rgb2hex <- function(r,g,b, alpha) rgb(r, g, b, alpha, maxColorValue = 255)

#' @export
setMethod("blend_colors", signature(bottom="ColorPlane", top="ColorPlane", alpha="numeric"),
          def=function(bottom, top, alpha=1) {
            
            rgb1 <- as_rgb(bottom)
            rgb2 <- as_rgb(top)
            
            achan <- alpha_channel(top)
            alpha <- achan * alpha
            
            clrs <- (1-alpha)*rgb1[,1:3,drop=FALSE] + alpha*rgb2[,1:3,drop=FALSE]
            RGBColorPlane(clrs)
            
          })


#' @export
setMethod("blend_colors", signature(bottom="ColorPlane", top="ColorPlane", alpha="missing"),
          def=function(bottom, top) {
            
            rgb1 <- as_rgb(bottom)
            rgb2 <- as_rgb(top)
            
            alpha <- alpha_channel(top)
            clrs <- (1-alpha)*rgb1[,1:3,drop=FALSE] + alpha*rgb2[,1:3,drop=FALSE]
            RGBColorPlane(clrs)
            
          })

#' @export
setMethod("blend_colors", signature(bottom="HexColorPlane", top="RGBColorPlane", alpha="numeric"),
          def=function(bottom, top, alpha) {
            bottom <- as_rgb(bottom)
            
            alpha <- alpha_channel(top) * alpha
            ## multiple constant alpha with alpha channel of top level
            clrs <- (1-alpha)*bottom[,1:3,drop=FALSE] + alpha*top@clrs[,1:3,drop=FALSE]
            RGBColorPlane(clrs)
          })


#' @export
setMethod("as_rgb", signature(x="RGBColorPlane"),
          def=function(x) x@clrs)

#' @export
setMethod("as_rgb", signature(x="HexColorPlane"),
          def=function(x) t(col2rgb(x@clrs, alpha=TRUE)))

#' @export
setMethod("as_hexcol", signature(x="RGBColorPlane"),
          def=function(x) {
            if (ncol(x@clrs) == 4) {
              rgb2hex(x@clrs[,1], x@clrs[,2], x@clrs[,3], x@clrs[,4])
            } else if (ncol(x@clrs) == 3) {
              rgb2hex(x@clrs[,1], x@clrs[,2], x@clrs[,3])
            } else {
              stop("as_hexcol: 'x' must have 3 or 4 columns")
            }
          })

#' @export
setMethod("as_hexcol", signature(x="HexColorPlane"),
          def=function(x) x@clrs)

#' @export
setMethod("alpha_channel", signature(x="HexColorPlane"),
          def=function(x, normalize=TRUE) {
            if (normalize) {
              col2rgb(x@clrs, alpha=TRUE)[4,]/255
            } else {
              col2rgb(x@clrs, alpha=TRUE)[4,]
            }
          })


#' @export
setMethod("alpha_channel", signature(x="RGBColorPlane"),
          def=function(x, normalize=TRUE) {
            if (normalize) {
              x@clrs[,4]/255
            } else {
              x@clrs[,4]
            }
          })


#' @export
setMethod("map_colors", signature=c("ConstantColorPlane"),
          def=function(x) {
            new("HexColorPlane", clrs=x@clrs)
          })

setMethod("map_colors", signature=c("DiscreteColorPlane"),
          def=function(x, ...) {
            x@cols
          })
          
          
#' @export
setMethod("map_colors", signature=c("IntensityColorPlane"),
          def=function(x, alpha=1, threshold=NULL, irange=NULL) {
            if (is.null(irange)) {
              irange <- range(x@intensity, na.rm=TRUE)
              clrs <- x@colmap[as.integer((x@intensity - irange[1])/ diff(irange) * (length(x@colmap) -1) + 1)]
              clrs[is.na(clrs)] <- "#00000000"
            } else {
              assertthat::assert_that(irange[2] >= irange[1])
              full_range <- range(x@intensity, na.rm=TRUE)
              #irange <- c(max(irange[1], full_range[1]), min(irange[2], full_range[2]))
              icol <- as.integer((x@intensity - irange[1])/diff(irange) * (length(x@colmap) -1) + 1)
              icol[icol < 1] <- 1
              icol[icol > length(x@colmap)] <- length(x@colmap)
              clrs <- x@colmap[icol]
              clrs[is.na(icol)] <- "#00000000"
              clrs
            }
            
            if (!is.null(threshold)) {
              clrs <- col2rgb(clrs, alpha=TRUE)
              if (length(threshold) == 1) {
                trans <- x@intensity < threshold
                clrs[4,trans] <- 0
              } else if (length(threshold) == 2) {
                #cat("thresholding ", threshold)
                trans <- x@intensity > threshold[1] & x@intensity < threshold[2]
                clrs[4,trans] <- 0
                
              } else {
                stop("threshold must be a numeric vector with 1 or 2 elements")
              }
              
              new("RGBColorPlane", clrs=t(clrs))
              
            } else {
              new("HexColorPlane", clrs=clrs)
            }
            
            
          })
