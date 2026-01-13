.onAttach <- function(libname, pkgname) {

  # prüft installierte mvwizr version
  installed <- utils::packageVersion("mvwizr")

  # github version abrufen
  url <- "https://api.github.com/repos/ror-at-ebp/mvwizr/releases/latest"
  res <- try(httr::GET(url), silent = TRUE)

  if (inherits(res, "try-error")||res$status_code != 200){ # warnmeldung falls github nicht erreicht
    warning("Github konnte nicht erreicht werden. Bitte überprüfe deine Internetverbindung")
  }

  latest_tag <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))$tag_name # sucht nach neuster version auf github
  latest <- numeric_version(gsub("v\\s*", "", latest_tag)) # sauberes Versionsobjekt erzeugen

  # vergleicht github version mit installierter Version
  if (installed < latest){
    warning(sprintf("Deine mvwizr Version (%s) stimmt nicht mit der neusten GitHub Version (%s) überein. Bitte aktualisiere mvwizr.",
                    installed, latest))
  } else {
    packageStartupMessage("Du hast die neuste Version von mvwizr (%s)", installed)
  }
}
