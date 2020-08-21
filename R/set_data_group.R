set_data_group <- function(data = NULL,
                           mapping = ggplot2::aes(),
                           showArea = FALSE,
                           ymin = NULL,
                           color = NULL,
                           lineWidth = NULL,
                           axesLayout = "parallel",
                           originalData = NULL) {

  if(is.null(data)) stop("No data found", call. = FALSE)

  dimD <- dim(data)
  n <- dimD[1]
  p <- dimD[2]

  stopifnot(
    exprs = {
      length(color) == 0 || length(color) == 1 || length(color) == n
      length(lineWidth) == 0 || length(lineWidth) == 1 || length(lineWidth) == n
    }
  )

  if(length(color) == 0) color <- rep(NA, n)
  if(length(color) == 1) color <- rep(color, n)
  if(length(lineWidth) == 0) lineWidth <- rep(NA, n)
  if(length(lineWidth) == 1) lineWidth <- rep(lineWidth, n)

  ymin <- set_ymin(ymin, n, p)

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
                     y = as.numeric(c(data[i, ], ymin[i, ])),
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

  # color includes NA; input color is NULL
  # replace the NA color column
  # to data mapping variable
  if(any(is.na(grouped_data$color))) {

    grouped_data$color <- NULL
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

  }

  if(any(is.na(grouped_data$size))) {

    grouped_data$size <- NULL
    quo_size <- mapping$size

    if(!rlang::is_empty(quo_size) && !is.null(originalData)) {
      grouped_data <- cbind(
        grouped_data,
        size = rep(
          rlang::eval_tidy(rlang::quo(!!quo_size),  originalData),
          each = switch(axesLayout, "parallel" = p, "radial" = p + 1)
        )
      )
    }
  }

  # merge original data
  switch(
    axesLayout,
    "parallel" = {
      cbind(
        grouped_data,
        originalData[rep(seq(n), each = p), ],
        row.names = NULL
      )
    },
    "radial" = {
      cbind(
        grouped_data,
        originalData[rep(seq(n), each = p + 1), ],
        row.names = NULL
      )
    }
  )
}

set_ymin <- function(ymin, n, p) {

  ymin <- ymin %||% 0

  if(is.atomic(ymin)) {
    # a vector
    if(!is.numeric(ymin))
      stop("ymin must be numerical.")
    ymin <- rep_len(ymin, p)

    as.data.frame(matrix(rep(ymin, each = n), nrow = n))
  } else {
    # a data.frame or a list
    as.data.frame(ymin, stringsAsFactors = FALSE)
  }
}
