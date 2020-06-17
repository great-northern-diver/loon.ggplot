set_data_group <- function(data = NULL,
                           mapping = ggplot2::aes(),
                           showArea = FALSE,
                           color = NULL,
                           lineWidth = 0.5,
                           axesLayout = "parallel",
                           originalData = NULL) {

  if(is.null(data)) stop("No data found", call. = FALSE)

  dimD <- dim(data)
  n <- dimD[1]
  p <- dimD[2]

  stopifnot(
    exprs = {
      length(color) == 0 || length(color) == 1 || length(color) == n
      length(lineWidth) == 1 || length(lineWidth) == n
    }
  )

  if(length(color) == 0) color <- rep(NA, n)
  if(length(color) == 1) color <- rep(color, n)
  if(length(lineWidth) == 1) lineWidth <- rep(lineWidth, n)

  grouped_data <- switch(
    axesLayout,
    "parallel" = {
      xaxis <- seq(0, 1, length.out =  p)
      do.call(
        rbind,
        lapply(1:n,
               function(i) {
                 if(showArea) {
                   data.frame(
                     x = c(xaxis, rev(xaxis)),
                     y = as.numeric(c(data[i, ], rep(0, p))),
                     group = rep(i, 2 * p),
                     color = rep(color[i], 2 * p),
                     stringsAsFactors = FALSE
                   )
                 } else {
                   data.frame(
                     x = xaxis,
                     y = as.numeric(data[i, ]),
                     group = rep(i, p),
                     size = rep(lineWidth[i], p),
                     color = rep(color[i], p),
                     stringsAsFactors = FALSE
                   )
                 }
               })
      )
    },
    "radial" = {
      radius <- loon_default_setting("radius")
      xpos <- 0.5
      ypos <- 0.5
      angle <- seq(0, 2 * base::pi, length.out = p + 1)[1:p]
      do.call(
        rbind,
        lapply(1:n,
               function(i) {

                 radialxais <- radius * data[i,] * cos(angle)
                 radialyais <- radius * data[i,] * sin(angle)

                 if(showArea) {
                   data.frame(
                     x = xpos + c(radialxais, radialxais[1]),
                     y = ypos + c(radialyais, radialyais[1]),
                     group = rep(i, p + 1),
                     color = rep(color[i], p + 1),
                     stringsAsFactors = FALSE
                   )
                 } else {
                   data.frame(
                     x = xpos + c(radialxais, radialxais[1]),
                     y = ypos + c(radialyais, radialyais[1]),
                     group = rep(i, p + 1),
                     color = rep(color[i], p + 1),
                     size = rep(lineWidth[i], p + 1),
                     stringsAsFactors = FALSE
                   )
                 }
               })
      )
    }
  )

  # remove NA color
  if(any(is.na(grouped_data$color))) grouped_data$color <- NULL
  quo_color <- mapping$colour

  if(!rlang::is_empty(quo_color) && !is.null(originalData)) {
    grouped_data <- cbind(
      grouped_data,
      color = rep(
        rlang::eval_tidy(rlang::quo(!!quo_color),  originalData),
        each = switch(axesLayout, "parallel" = p, "radial" = p + 1)
      )
    )
  }

  if(!is.null(grouped_data$color)) {
    if(is.numeric(grouped_data$color)) {
      warning("Color can only be discrete", call. = FALSE)
      grouped_data$color <- as.character(grouped_data$color)
    }
  }

  return(grouped_data)
}
