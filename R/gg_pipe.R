#' @title Create a ggplot object function for piping
#'
#' @description This function is used to pack and pass ggplot object through a pipe model (because of the
#' precedence of + and %>%, ggplot object will break the pipe model)
#'
#' @param data a data frame to use for ggplot
#' @param ggplotObject a ggplot object to be passed though
#'
#' @return a ggplot function, not used for plot, just for passing through
#'
#'
#' @export
#'
#' @examples
#'
#' library(magrittr)
#' g <- mtcars %>%
#'    gg_pipe(ggplot(aes(mpg, wt, colour = cyl)) + geom_point()) %>%
#'    loon.ggplot()

gg_pipe <- function(data, ggplotObject){
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  subs <- substitute(ggplotObject)
  # convert a call object to a character
  dep <- deparse(subs)
  if (any(str_detect(dep, "ggplot"))) {
    ggplot_id <- which(str_detect(dep, "ggplot") == TRUE)
    if (length(ggplot_id) > 1) {
      stop("only one ggplot model is required")
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
  } else stop("ggplot object cannot be found")
}
