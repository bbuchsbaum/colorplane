


#' blend two color planes
#' 
#' @param bottom the bottom color plane
#' @param top the top color plane
#' @param alpha the alpha blending value
#' @export
setGeneric(name="blend_colors", def=function(bottom, top, alpha) standardGeneric("blend_colors"))


#' map data values to a set of colors
#'
#' @param x the object to map over
#' @export
setGeneric(name="map_colors", def=function(x, ...) standardGeneric("map_colors"))


#' convert to rgb colors
#'
#' @param x the object to convert
#' @param ... extra args
#' @rdname color-conversion
#' @export
setGeneric(name="as_rgb", def=function(x, ...) standardGeneric("as_rgb"))

#' convert to hex colors
#'
#' @param x the object to convert
#' @param ... extra args
#' @rdname color-conversion
#' @export
setGeneric(name="as_hexcol", def=function(x, ...) standardGeneric("as_hexcol"))

#' alpha_channel
#'
#' extract the alpha channel
#'
#' @param x the object to extract alpha channel from
#' @param ... extra args
#' @export
setGeneric(name="alpha_channel", def=function(x, ...) standardGeneric("alpha_channel"))


#' get_color
#' 
#' get_color associated with one or more values
#'
#' @param x the color lookup table
#' @param v the intensity value(s)
#' @param ... extra args
#' @export
setGeneric(name="get_color", def=function(x, v, ...) standardGeneric("get_color"))

