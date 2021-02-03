random_tip <- function() {
  tips <- c(
    "Need help? Check https://great-northern-diver.github.io/loon.ggplot/ for more details",
    "Welcome to 'great-northern-diver' community https://github.com/great-northern-diver"
  )

  sample(tips, 1)
}

.onAttach <- function(libname, pkgname) {
  tip <- random_tip()
  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}
