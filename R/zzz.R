.onLoad <- function(libname, pkgname) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  # beim Laden von mvwizrAWEL mvwizr immer auf neueste Version updaten
  try({
    remotes::install_github("ror-at-ebp/mvwizr", upgrade = "always")
  }, silent = TRUE)
}
