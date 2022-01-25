random_tip <- function() {
  tips <- c(
    "See helpful articles at l_web(package = \"loon.ggplot\", directory = \"articles\")",
    "See \"A Grammar Of Interactive Graphics\" at l_web(package = \"loon.ggplot\", page = \"grammarOfInteractiveGraphics\", directory = \"articles\")",
    "For more on interactive plots, see l_web(package = \"loon\", directory = \"articles\")",
    "Interactive plots via l_web(package = \"loon.shiny\")",
    "Multivariate extension to \"ggplot2\" via l_web(package = \"ggmulti\")",
    "The \"diveR\" package suite bundles \"loon.ggplot\" with other loon related packages."
  )
  sample(tips, 1)
}

.onAttach <- function(libname, pkgname) {

  if(utils::packageVersion("loon") < "1.3.9") {
    msg <- paste0("the current `loon` package version is ",
                  utils::packageVersion("loon"),
                  ". Please update it to 1.3.9 or above.")
    initial <- "Warning:"
    indent <- 1
    exdent <- 1
    prefix <- paste0(rep(" ", nchar(initial)), collapse = "")
    msg <-  paste(strwrap(msg,
                          initial = initial,
                          indent = indent,
                          prefix = prefix,
                          exdent = exdent),
                  collapse = "\n")
    packageStartupMessage(msg)
  }

  if (stats::runif(1) > 0.5) {
    tip <- random_tip()
    initial <- "Random tip:"
    indent <- 1
    exdent <- 1
    prefix <- paste0(rep(" ", nchar(initial)), collapse = "")
    msg <-  paste(strwrap(tip,
                          initial = initial,
                          indent = indent,
                          prefix = prefix,
                          exdent = exdent),
                  collapse = "\n")
    packageStartupMessage(msg)
    }
}
