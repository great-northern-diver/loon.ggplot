#' @title Position scales for continuous data (x, y & z)
#' @description Scaling the coordinates for 3D visualization
#' @param trans For continuous scales, the name of a transformation object or the object itself.
#' Built-in transformations include "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p",
#' "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt" and "time".
#' A transformation object bundles together a transform, its inverse, and methods for generating breaks and labels.
#' Transformation objects are defined in the scales package, and are called  <name>_trans (e.g., \code{scales::boxcox_trans()}).
#' You can create your own transformation with \code{scales::trans_new()}.
#' @param ... Other arguments passed on to \code{scale_(x|y)_continuous()}. To set the position scales,
#' three scales (x, y, z) has to be set simultaneously.
#' @details In 3D rotation, different scales of variables x, y and z may cause an issue that the points appear to be
#' off the window even with a minor tweak. Additionally, if one variable is in a large scale,
#' the shape of the 3D plot may be dominated. Setting \code{scale_multi} can ensure the scales in the same measurement, as
#' we rotate the plot, most points will stay inside the current view.
#' @return a list of the \code{ggproto} objects
#' @export
#' @import scales
#' @examples
#' if(interactive()) {
#'
#' dsamp <- dplyr::sample_n(diamonds, 100)
#'
#' \dontrun{
#' # press `R`, then rotate with a minor tweak,
#' # Issues:
#' #   1: Points are displayed outside the window
#' #   2: Always in a line shape
#' l_ggplot(dsamp, aes(x = carat, y = price,
#'                     z = depth, colour = color)) +
#'   geom_point()}
#'
#' # set scales
#' l_ggplot(dsamp, aes(x = carat, y  =price,
#'                     z = depth, colour = color)) +
#'   geom_point() +
#'   scale_multi()
#'
#' # customized `trans`
#' logp1_base10_trans <- scales::trans_new(
#'   name = "logp",
#'   trans = function(x) log(x + 1, base = 10),
#'   inverse = function(x) 10**x - 1,
#'   breaks = scales::log_breaks())
#'
#' l_ggplot(dsamp, aes(x = carat, y = price,
#'                     z = depth, colour = color)) +
#'   geom_point() +
#'   scale_multi(trans = logp1_base10_trans)
#' }
scale_multi <- function(trans = scaleBox(center = TRUE), ...) {

  dotArgs <- list(...)
  if(!is.null(dotArgs$position)) {
    if(length(dotArgs$position) != 3) {
      warning("The scales of x, y, z are set simultaneously. ",
              "Position should be a vector to determine the x, y, z in order",
              call. = FALSE)
      dotArgs$position <- NULL

      dotArgs_x <- dotArgs
      dotArgs_y <- dotArgs
      dotArgs_z <- dotArgs

    } else {

      dotArgs_x <- dotArgs
      dotArgs_x$position <- dotArgs$position[1]

      dotArgs_y <- dotArgs
      dotArgs_y$position <- dotArgs$position[2]

      dotArgs_z <- dotArgs
      dotArgs_z$position <- dotArgs$position[3]

    }
  } else {
    dotArgs_x <- dotArgs
    dotArgs_y <- dotArgs
    dotArgs_z <- dotArgs
  }

  dotArgs_x$trans <- trans
  dotArgs_y$trans <- trans
  dotArgs_z$trans <- trans

  list(
    do.call(ggplot2::scale_x_continuous, dotArgs_x),
    do.call(ggplot2::scale_y_continuous, dotArgs_y),
    do.call(scale_z_continuous, dotArgs_z)
  )
}

#' @title Box scaling in 3D rotation
#' @description the variable is scaled to have equal ranges and,
#' when \code{center = TRUE}, to be centred by the average of the min and max.
#' @param center either a logical value or numeric-alike vector of length equal to the number of columns of
#' x, where 'numeric-alike' means that \code{as.numeric(.)} will be applied successfully
#' if \code{is.numeric(.)} is not true.
#' @return A \code{trans} object
#' @seealso \code{\link{l_scale3D}}
#' @export
scaleBox <- function(center = TRUE) {
  scales::trans_new(
    name = "scaleBox",
    trans = function(x) {
      suppressWarnings(
        unname(
          unlist(
            loon::l_scale3D(as.data.frame(x),
                            method = "box",
                            center = center)
          )
        )
      )
    },
    inverse = function(x) x
  )
}

scale_z_continuous <- function(name = ggplot2::waiver(), breaks = ggplot2::waiver(),
                               minor_breaks = ggplot2::waiver(),
                               n.breaks = NULL, labels = ggplot2::waiver(),
                               limits = NULL, expand = ggplot2::waiver(),
                               oob = scales::censor, na.value = NA_real_, trans = "identity",
                               guide = ggplot2::waiver(), position = "right",
                               sec.axis = ggplot2::waiver()) {

  sc <- ggplot2::continuous_scale(c("z", "zmin", "zmax",
                                    "zend", "zintercept", "zmin_final",
                                    "zmax_final", "zlower", "zmiddle",
                                    "zupper", "z0"), "position_c", identity,
                                  name = name, breaks = breaks, n.breaks = n.breaks, minor_breaks = minor_breaks,
                                  labels = labels, limits = limits, expand = expand, oob = oob,
                                  na.value = na.value, trans = trans, guide = guide, position = position,
                                  super = ggplot2::ScaleContinuousPosition)
  set_sec_axis(sec.axis, sc)
}

