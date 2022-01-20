loonDefaultLinkingKey <- function(data) {
  paste0(seq(0, length(row.names(data)) - 1))
}

loonLinkingKey <- function(data, args) {
  if (is.data.frame(data) & !is.waive(data)) {
    # check linkingKey
    if (is.null(args[['linkingKey']])) {
      # default linkingKey
      loonDefaultLinkingKey(data)
    } else {
      args[['linkingKey']]
    }
  } else {
    if (is.null(args[['linkingKey']])) {
      NULL
    } else {
      args[['linkingKey']]
    }
  }
}

loonItemLabel <- function(data, args) {
  if (is.data.frame(data) & !is.waive(data)) {
    # check itemLabel
    if (is.null(args[['itemLabel']])) {
      # default itemLabel
      gsub(" ", "-", row.names(data))
    } else {
      as.character(args[['itemLabel']])
    }
  } else NULL
}
