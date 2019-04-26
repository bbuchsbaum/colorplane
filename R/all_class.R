setClass("BaseColorPlane")

#' @slot intensity a vector of intensity values
#' @slot alpha a vector of alpha values 
#' @slot colmap a color map containing a vector of hex character codes
#' @export
setClass("IntensityColorPlane",
         representation(intensity="numeric", alpha="numeric", colmap="character"), contains="BaseColorPlane")


#' @slot values a vector of discrete integer values
#' @slot lookup a lookup table mapping values to hex colors
#' @slot alpha a vector of alpha values 
#' @export
setClass("DiscreteColorPlane",
         representation(values="integer", lookup="list", alpha="numeric"), contains="BaseColorPlane")




#' @slot clrs a field of colors
#' @export
setClass("ColorPlane", representation(clrs="ANY"), contains="BaseColorPlane")

#' @inheritParams ColorPlane
#' @export
setClass("HexColorPlane", representation(clrs="character"), contains="ColorPlane")

#' @inheritParams ColorPlane
#' @export
setClass("RGBColorPlane", representation(clrs="matrix"), contains="ColorPlane")

#' @inheritParams ColorPlane
#' @export
setClass("ConstantColorPlane", representation(clrs="character"), contains="ColorPlane")
