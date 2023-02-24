


#' blend two color planes
#'
#' given two color planes, generate a new color plane by blending the colors using the supplied alpha multiplier.
#'
#' @param bottom the bottom color plane
#' @param top the top color plane
#' @param alpha the alpha overlay value.
#' @export
#' @rdname blend_colors-methods
#' @examples
#'
#' top <- IntensityColorPlane(1:5, cols=rainbow(5))
#' bottom <- IntensityColorPlane(1:5, cols=rev(rainbow(5)))
#'
#' top <- map_colors(top)
#' bottom <- map_colors(bottom)
#' bc <- blend_colors(bottom, top, .5)
#'
setGeneric(name="blend_colors", def=function(bottom, top, alpha) standardGeneric("blend_colors"))


#' map data values to a set of colors
#'
#' instantiate a vector of colors from a ColorPlane specification.
#'
#' @param x the object to map over
#' @param ... extra args
#' @export
#' @rdname map_colors-methods
#'
#'
#' @examples
#'
#' cp <- IntensityColorPlane(seq(1,100), cols=rainbow(25))
#' cl <- map_colors(cp, irange=c(0,50))
#' stopifnot(cl@clr[50] == rainbow(25)[25])
#'
#' @return a \code{HexColorPlane} instance containing the mapped colors
setGeneric(name="map_colors", def=function(x, ...) standardGeneric("map_colors"))


#' convert to rgb colors
#'
#' @param x the object to convert
#' @param ... extra args
#' @rdname as_rgb-methods
#' @export
#' @examples
#' cp <- IntensityColorPlane(seq(1,100), cols=rainbow(25))
#' cl <- map_colors(cp, irange=c(0,50))
#' rgbcols <- as_rgb(cl)
setGeneric(name="as_rgb", def=function(x, ...) standardGeneric("as_rgb"))

#' convert to hex colors
#'
#' @param x the object to convert
#' @param ... extra args
#' @rdname as_hexcol-methods
#' @export
setGeneric(name="as_hexcol", def=function(x, ...) standardGeneric("as_hexcol"))

#' alpha_channel
#'
#' extract the alpha channel
#'
#' @param x the object to extract alpha channel from
#' @param ... extra args
#' @export
#' @rdname alpha_channel-methods
#' @examples
#' cp <- IntensityColorPlane(seq(1,5), cols=rainbow(25))
#' cl <- map_colors(cp, irange=c(0,50))
#' stopifnot(length(alpha_channel(cl)) == 5)
setGeneric(name="alpha_channel", def=function(x, ...) standardGeneric("alpha_channel"))


#' get_color
#'
#' get the color associated with one or more values
#'
#' @param x the color lookup table
#' @param v the intensity value(s)
#' @param ... extra args
#' @export
#' @rdname get_color-methods
#' @return a color value
setGeneric(name="get_color", def=function(x, v, ...) standardGeneric("get_color"))

