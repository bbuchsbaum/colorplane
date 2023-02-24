setClass("BaseColorPlane")


#' IntensityColorPlane
#'
#' An association of intensities and colors
#'
#' @slot intensity a vector of intensity values
#' @slot alpha a vector of alpha values
#' @slot colmap a color map containing a vector of hex character codes
#' @export
#' @rdname IntensityColorPlane-class
setClass("IntensityColorPlane",
         representation(intensity="numeric", alpha="numeric", colmap="character"), contains="BaseColorPlane")



#' DiscreteColorPlane
#'
#' @slot lookup a lookup table mapping values to hex colors
#' @export
#' @rdname DiscreteColorPlane-class
setClass("DiscreteColorPlane",
         representation(lookup="list"), contains="BaseColorPlane")



#' ColorPlane
#'
#' @slot clr a field of colors
#' @rdname ColorPlane-class
setClass("ColorPlane", representation(clr="ANY"), contains="BaseColorPlane")


#' HexColorPlane
#'
#' @inheritParams ColorPlane
#' @export
#' @rdname HexColorPlane-class
setClass("HexColorPlane", representation(clr="character"), contains="ColorPlane")


#' RGBColorPlane
#'
#' @inheritParams ColorPlane
#' @export
#' @rdname RGBColorPlane-class
setClass("RGBColorPlane", representation(clr="matrix"), contains="ColorPlane")

#' ConstantColorPlane
#'
#' @inheritParams ColorPlane
#' @slot clr the constant color as hex value
#' @export
#' @rdname ConstantColorPlane-class
#' @examples
#'
#' cp <- ConstantColorPlane(clr="#FF0000")
setClass("ConstantColorPlane", representation(clr="character"), contains="ColorPlane")


#' ColorScale
#'
#' @slot irange the intensity range of the scale
#' @slot threshold the alpha thresholding range
#' @slot clr a vector of hex colors
#' @export
#' @rdname ColorScale-class
setClass("ColorScale", representation(irange="numeric", threshold="numeric", clr="character"))



