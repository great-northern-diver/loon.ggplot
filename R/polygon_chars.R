#' @title Polygon glyph coordinates
#' @description Some useful polygon coordinates
#' @name polygonGlyph
#' @seealso \link{geom_polygonGlyph}
#' @export
#' @format An object of class \code{character}.
#' @examples
#' if(requireNamespace("grid")) {
#'   library(grid)
#'   grid.newpage()
#'   grid.polygon(x=(x_star + 1)/2,
#'                y=(1 - y_star)/2)
#'   grid.newpage()
#'   grid.polygon(x=(x_cross + 1)/2,
#'                y=(y_cross + 1)/2)
#'   grid.newpage()
#'   grid.polygon(x=(x_hexagon + 1)/2,
#'                y=(y_hexagon + 1)/2)
#'   grid.newpage()
#'   grid.polygon(x=(-x_airplane + 4)/10,
#'                y=(-y_airplane + 4)/10)
#' }
x_star <-
  c(-0.000864304235090734, 0.292999135695765, 0.949870354364736,
    0.474503025064823, 0.586862575626621, -0.000864304235090734,
    -0.586430423509075, -0.474070872947277, -0.949438202247191, -0.29256698357822)

#' @rdname polygonGlyph
#' @export
y_star <-
  c(-1, -0.403630077787381, -0.308556611927398, 0.153846153846154,
    0.808556611927398, 0.499567847882455, 0.808556611927398,
    0.153846153846154, -0.308556611927398, -0.403630077787381)

#' @rdname polygonGlyph
#' @export
x_cross <-
  c(-0.258931143762604, -0.258931143762604, -0.950374531835206,
    -0.950374531835206, -0.258931143762604, -0.258931143762604,
    0.259651397291847, 0.259651397291847, 0.948934024776722,
    0.948934024776722, 0.259651397291847, 0.259651397291847)

#' @rdname polygonGlyph
#' @export
y_cross <-
  c(-0.950374531835206, -0.258931143762604, -0.258931143762604,
    0.259651397291847, 0.259651397291847, 0.948934024776722,
    0.948934024776722, 0.259651397291847, 0.259651397291847,
    -0.258931143762604, -0.258931143762604, -0.950374531835206)

#' @rdname polygonGlyph

#' @export
x_hexagon <-
  c(0.773552290406223, 0, -0.773552290406223, -0.773552290406223,
    0, 0.773552290406223)

#' @rdname polygonGlyph

#' @export
y_hexagon <-
  c(0.446917314894843, 0.894194756554307, 0.446917314894843,
    -0.447637568424085, -0.892754249495822, -0.447637568424085)



airplane_coords <- c(30.8,0.5,57.4,27.1,85.6,16.5,89.9,17,78.7,30.9,183.5,27.7,
                     223.5,6.4,234.6,7.4,222.9,22.3,240,21.8,253.8,26.1,264.5,
                     33.5,276.2,39.4,283.1,42,286.5,50.6,282,57.5,273.5,63.9,
                     260.2,69.7,246.9,72.4,217.1,76.1,176.6,78.8,151.6,78.8,
                     88.8,105.9,62.7,95.8,117,70.8,87.7,70.8,73.9,68.1,56.3,
                     63.3,44.6,53.2,20.7,61.2,11.6,57.5,34,44.2)
x_ap <- airplane_coords[seq(1, length(airplane_coords), by=2)]
y_ap <- airplane_coords[seq(2, length(airplane_coords), by=2)]
d_ap <- diff(range(x_ap, y_ap))/3.5
#' @rdname polygonGlyph

#' @export
x_airplane <- (x_ap-mean(x_ap))/d_ap
#' @rdname polygonGlyph

#' @export
y_airplane <- (y_ap-mean(y_ap))/d_ap
