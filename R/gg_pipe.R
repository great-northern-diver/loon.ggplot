#' @title Pipe ggplot object
#'
#' @description Pack a \code{ggplot} object forward to \code{ggplot2loon} expressions
#' via a pipe-operator "\%>\%".
#'
#' @details When "+" and "\%>\%" both appear in pipe operations, "\%>\%" takes the priority of "+",e.g:
#'
#' \code{mtcars \%>\%
#'    ggplot(aes(mpg, wt, colour = cyl)) +
#'    geom_point() \%>\%
#'    ggplot2loon()},
#'
#' error would occur. The reason is
#'
#' \code{geom_point() \%>\% ggplot2loon()}
#'
#' would run before
#'
#' \code{ggplot(aes(mpg, wt, colour = cyl)) + geom_point()}.
#'
#' Hence, we need a function \code{gg_pipe()} to pack the \code{ggplot} object and force operations happen in order.
#'
#' @param data a data frame to use for ggplot
#' @param ggObj a ggplot object to be passed though
#'
#' @return a \code{ggplot} evaluate object
#'
#' @export
#'
#' @examples
#' if(requireNamespace("magrittr") && interactive()) {
#' \dontrun{
#'  # Error
#'  g <- mtcars %>%
#'     ggplot(aes(mpg, wt, colour = cyl)) +
#'     geom_point() %>%
#'     ggplot2loon()
#' }
#'  g <- mtcars %>%
#'    gg_pipe(
#'      ggplot(aes(mpg, wt, colour = cyl)) + geom_point()
#'    ) %>%
#'    ggplot2loon()
#' }

gg_pipe <- function(data, ggObj) {

  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  subs <- substitute(ggObj)
  # convert a call object to a character
  dep <- deparse(subs)
  # stringr::str_detect(dep, "ggplot")
  if (any(grepl("ggplot", dep))) {
    ggplot_id <- which(grepl("ggplot", dep))
    if (length(ggplot_id) > 1) {
      stop("only one ggplot model can be handled", call. = FALSE)
    } else {
      dep1 <- dep[ggplot_id]
      # construct a new ggplot character with input data
      dep1_split <- strsplit(dep1, split = "")[[1]]
      len.dep1_split <- length(dep1_split)
      first_left_bracket <- which(dep1_split %in% "(" == TRUE)[1]
      new_dep1 <- paste0(c(dep1_split[1:first_left_bracket], "data, ",
                           dep1_split[(first_left_bracket + 1):len.dep1_split]),
                         collapse =  "")
      dep[ggplot_id] <- new_dep1
      parse_dep <- parse(text = paste0(dep, collapse = ""))
      eval(parse_dep)
    }
  } else stop("ggplot object cannot be found", call. = FALSE)
}
