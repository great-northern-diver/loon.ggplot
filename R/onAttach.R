random_tip <- function() {
  tips <- c(
    "For more info: l_web(package = \"loon.ggplot\")"
  )

  sample(tips, 1)
}

.onAttach <- function(libname, pkgname) {
  tip <- random_tip()
  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}
