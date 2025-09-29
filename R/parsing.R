# AWEL-spezifische Erweiterung von mvwizr
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
# fuer R CMD check NOTES Variablen definieren
utils::globalVariables(c(
  "WERT_NUM", "BG", "NAME", "BEGINNPROBENAHME_DATUM",
  "BEGINNPROBENAHME_ZEIT", "ENDEPROBENAHME_DATUM",
  "ENDEPROBENAHME_ZEIT", "BEZEICHNUNG_BAFU", "ID_Substanz",
  "PARAMETERID_BAFU", "PARAMETER"
))

#' MV-Daten des AWEL einlesen
#'
#' Liest MV-Daten (LIMS: Exportfilter_DB_Daten_mvwizr) des AWEL ein, formatiert sie um und uebergibt sie fuer die weitere Verarbeitung direkt and die mvwizr Funktion "einlesen_mv_gbl" (wrapper-Funktion). Die Funktion ersetzt also "einlesen_mv_gbl" aus mvwizr, damit der AWEL LIMS-Export direkt verarbeitet werden kann.
#' Sollen berechnete Mischproben verwendet werden (bSaP = TRUE), werden Konzentrationen aus 3-/4-Tagesmischproben zeitproportional zu 2-Wochenmischproben verrechnet. Das jaehrliche Startdatum der verdichteten Probenahme kann optional als Vektor eingegeben werden.
#'
#' @param mv_daten_pfad Pfad zur kommaseparierten Datei mit den MV-Daten (fuer mehrere Dateien als Vektor von Dateipfaden)
#' @param vsa_lookup_pfad Pfad zur VSA-Tabelle `Tab_Substanzen.xlsx` (optional)
#' @param bafu_lookup_pfad Pfad zum BAFU-Namen-Lookup (optional)
#' @param bSaP Logisch (Vorgabe `FALSE`). Sollen berechnete Mischproben verwendet werden?
#' @param startdatum_bSaP Optionaler Vektor mit den Startdaten der 3-/4-Tages-Mischproben, getrennt nach den im Datensatz enthaltenen Jahren. Format: startdatum <- c("dd.mm.yyyy","dd.mm.yyyy")
#'
#' @importFrom mvwizr einlesen_mv_gbl
#'
#' @return Dataframe mit MV-Daten
#' @export
#'
#' @examples
#' mv_daten_pfad <- system.file("extdata", "LIMSexport.txt", package = "mvwizrAWEL")
#' v.startdatum <- c("27.03.2023", "25.03.2024")
#'
#' mvdaten <- einlesen_mv_awel(mv_daten_pfad = mv_daten_pfad,
#'                             bSaP = FALSE,
#'                             startdatum_bSaP = v.startdatum)
#'
einlesen_mv_awel <- function(mv_daten_pfad, vsa_lookup_pfad = NULL, bafu_lookup_pfad = NULL, bSaP = FALSE, startdatum_bSaP = NULL){

  # Loop ueber alle angegebenen MV-Dateien zum Einlesen
  mv_daten_list <- purrr::map(mv_daten_pfad, function(mv_datei) {
    mv_df <- readr::read_delim(
      mv_datei,
      delim = ",")
  })

  # Erstellen des Datatables aus AWEL input Daten
  mv_daten_df <- mv_daten_list %>%
    purrr::list_rbind() %>%
    dplyr::mutate(
      BEGINNPROBENAHME = lubridate::dmy_hms(paste(.data$BEGINNPROBENAHME_DATUM, .data$BEGINNPROBENAHME_ZEIT)), #Korrigiertes Zeitformat
      ENDEPROBENAHME   = lubridate::dmy_hms(paste(.data$ENDEPROBENAHME_DATUM, .data$ENDEPROBENAHME_ZEIT)), #Korrigiertes Zeitformat
      PROBENAHMEDAUER_TAGE = round(as.numeric(difftime(.data$ENDEPROBENAHME, .data$BEGINNPROBENAHME, units = 'days'))),
      JAHR = lubridate::year(.data$BEGINNPROBENAHME),
      PROBEARTID = dplyr::recode(.data$PROBEARTID,
                                 "2-Wochenmischprobe" = "SaP",
                                 "Wochenmischprobe"   = "SaP",
                                 "Tagesmischprobe"    = "SaP",
                                 "Stichprobe"         = "S")) %>% # Ersetzt LIMS Syntax mit mvwizr Syntax
    dplyr::select(
      -.data$BEGINNPROBENAHME_DATUM,
      -.data$BEGINNPROBENAHME_ZEIT,
      -.data$ENDEPROBENAHME_DATUM,
      -.data$ENDEPROBENAHME_ZEIT) # drop Hilfsspalten fuer Zeitformat

  # Optionale 2-Wochenmischprobenberechnung auf Basis von Tagesmischproben
  berechnete_misch <- berechnung_mischproben(mv_daten_df = mv_daten_df, bSaP = bSaP, startdatum_bSaP = startdatum_bSaP)

  # Tibble der berechneten Mischproben hinzufuegen und finale Tabellenbereinigung
  mv_daten_df <- dplyr::bind_rows(mv_daten_df, berechnete_misch) %>%
    dplyr::mutate(
      UID = dplyr::row_number(), # Zuweisung einer eindeutigen ID
      OPERATOR = dplyr::if_else(WERT_NUM < BG, "<", ""), # Gibt fuer alle Proben an, ob die Konzentration ueber oder unter BG liegt
      STANDORT = NAME, # Standort ist nicht in LIMS hinterlegt, deshalb identisch mit Stationsname
      BEZEICHNUNG_BAFU = .data$PARAMETER, # wird verwendet fuer Plot Namen, dafuer Parameter Namen aus LIMS verwenden/zuweisen
      PARAMETERGRUPPEID = 0, # Pseudospalte fuer ID des Substanztyps da diese Information nicht in LIMS hinterlegt ist
      BEGINNPROBENAHME = format(.data$BEGINNPROBENAHME, "%d.%m.%Y %H:%M:%S"), # Datumsformate muessen wieder als character gespeichert werden da speichern in temp Datei sonst das Zeitformat aendert
      ENDEPROBENAHME   = format(.data$ENDEPROBENAHME, "%d.%m.%Y %H:%M:%S"),
      MSTLTYP = "Nicht hinterlegt") %>% # Pseudospalte fuer Messstellentyp, diese Information ist nicht in LIMS hinterlegt
    dplyr::select(-.data$PROBENAHMEDAUER_TAGE,-.data$JAHR) # Hilfszeilen loeschen

  # temporaere txt Datei generieren, da einlesen_mv_gbl nur Dateipfade zu txt-files akzeptiert
  temp_txt <- tempfile(fileext = ".txt")
  readr::write_delim(mv_daten_df, temp_txt, delim = ",", na = "", escape = "double")

  # Uebergabe des aufbereiteten Datensatzes an mvwizr und Zwischenspeichern des Resultats
  result <- mvwizr::einlesen_mv_gbl(mv_daten_pfad = temp_txt, vsa_lookup_pfad = vsa_lookup_pfad, bafu_lookup_pfad = bafu_lookup_pfad, bSaP = bSaP)

  # temporaere txt-Datei loeschen
  on.exit(file.remove(temp_txt))

  # Ergebnis zurueckgeben
  return(result)
}


#' 2-Wochenmischproben (bSaP) berechnen aus < 8 Tagesmischproben
#'
#' Wird innerhalb der Funktion "einlesen_mv_awel" aufgerufen und berechnet (falls bSaP = TRUE) auf Basis der verdichteten 3- oder 4-Tagesmischproben zeitproportional
#' die Konzentrationen der entsprechenden theoretischen 2-Wochenmischproben. Der jaehrliche Beginn der verdichteten Probenahme kann entweder manuell eingegeben werden,
#' oder wird automatisch aufgrund des Startdatums der ersten Mischprobe, die kuerzer als 8 Tage gesammelt wurde, gesetzt. Faellt das pro Jahr geschaetzte Startdatum
#' der Verdichtung auf einen Tag vor dem 15.03.20XX, wird ein Hinweis auf einen moeglichen Fehler in der Intervallberechnung ausgegeben.
#'
#' @param mv_daten_df Tibble mit eingelesenen und vorprozessierten mv Daten
#' @param startdatum_bSaP optionaler Vektor mit den Startdaten der 3-/4-Tages-Mischproben, getrennt nach den im Datensatz enthaltenen Jahren. Format: startdatum <- c("dd.mm.yyyy","dd.mm.yyyy")
#' @param bSaP Logisch (Vorgabe `FALSE`). Sollen berechnete Mischproben verwendet werden?
#'
#' @return Tibble mit den zeitproportional berechneten Konzentrationen der 2-Wochenmischproben, basierend auf den Tagesmischproben innerhalb des jeweiligen Intervalls.
#'
berechnung_mischproben <- function(mv_daten_df, bSaP = FALSE, startdatum_bSaP = NULL) {

  # Mischproben nur berechnen, wenn Mischproben verwendet werden sollen (bSaP = TRUE), sonst ueberspringen
  if (!bSaP) {
    return(NULL)
  }

  # Die Berechnung kann abgebrochen werden, wenn im Datensatz keine Tagesmischproben (Probenahmedauer < 8 Tage) vorhanden sind
  tagesmischproben <- mv_daten_df %>% dplyr::filter(.data$PROBENAHMEDAUER_TAGE < 8, .data$PROBEARTID == "SaP") # tibble mit allen Proben < 8 Tage
  if (nrow(tagesmischproben) == 0){ # pruefe ob es Proben < 8 Tage hat
    message("Keine Proben < 8 Tage. Keine Mischproben-Berechnung noetig.") # Statusmeldung
    return(NULL)
  }

  # Falls ein Referenzdatumsvektor uebergeben wurde -> Vektor formatieren
  if (!is.null(startdatum_bSaP)){
    startdatum_bSaP <- as.Date(startdatum_bSaP, format = "%d.%m.%Y")
    names(startdatum_bSaP) <- lubridate::year(startdatum_bSaP) # Daten nach Jahr benennen
  }
  # Startdatum des Intervalls pro Standort und Jahr (Beginn Verdichtung) bestimmen und 2-Wochenintervalle definieren
  tagesmischproben <- tagesmischproben %>%
    dplyr::group_by(.data$NAME, .data$JAHR) %>%
    dplyr::mutate(
      # Festlegung Startdatum Verdichtung
      startdatum_use = dplyr::coalesce( # wurde fuer ein bestimmtes Jahr ein Startdatum im Vektor gefunden?
        startdatum_bSaP[as.character(.data$JAHR)], # Ja -> nimmt fuer das jeweilige Jahr das manuell eingegebene
        min(.data$BEGINNPROBENAHME, na.rm = TRUE)), # Nein -> nimmt das Startdatum der ersten Tagesmischprobe
      # Berechnung Intervalle
      intervall_nr = floor(as.numeric(difftime(.data$BEGINNPROBENAHME, .data$startdatum_use, units = "days")) / 14) + 1) %>%
    # alle Proben entfernen, deren bSaP aus nur einer Probe berechnet werden wuerde
    dplyr::group_by(.data$NAME, .data$JAHR, .data$PARAMETER,.data$intervall_nr, .data$Methode) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()
  # Warnmeldung falls das Referenzdatum zur Berechnung der Intervalle vor dem 15. Maerz liegt
  if (any(lubridate::month(tagesmischproben$startdatum_use) == 3 &
           lubridate::day(tagesmischproben$startdatum_use) <= 15, na.rm = TRUE)) {
    warning("ACHTUNG: Mindestens ein verwendetes Startdatum der Probenverdichtung liegt vor dem 15. Maerz - Bitte ueberpruefen. ")
    }

  # Zeitgewichtete Mischprobe berechnen
  berechnete_mischproben <- tagesmischproben %>%
    dplyr::group_by(.data$NAME, .data$intervall_nr, .data$PARAMETER, .data$JAHR, .data$Methode) %>%
    dplyr::summarise(
      WERT_NUM = sum(.data$WERT_NUM * .data$PROBENAHMEDAUER_TAGE) / sum(.data$PROBENAHMEDAUER_TAGE), # Mischprobenkonzentration berechnen
      BEGINNPROBENAHME = min(.data$BEGINNPROBENAHME), # Startdatum berechnete Mischprobe
      ENDEPROBENAHME  = max(.data$ENDEPROBENAHME), # Enddatum berechnete Mischprobe
      dplyr::across(c(.data$CODE, .data$EINHEIT, .data$PARAMETERGRUPPE, .data$PARAMETERID_BAFU, .data$PROBENAHMEDAUER_TAGE), # restliche Spalten uebernehmen sofern innerhalb Group identisch, sonst NA
                    ~ ifelse(length(unique(.x)) == 1, unique(.x), NA),
                    .names = "{.col}"),
      BG = min(.data$BG), # als BG der berechneten Konzentration den tiefsten LOQ nehmen
      NG = min(.data$NG),
      .groups = "drop") %>%
    dplyr::select(-.data$intervall_nr) %>%
    dplyr::mutate(PROBEARTID = "bSaP")

  return(berechnete_mischproben)
}


