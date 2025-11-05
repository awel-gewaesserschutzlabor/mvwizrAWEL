# mvwizrAWEL
## Übersicht
Das Paket mvwizrAWEL enthält AWEL-spezifische Erweiterungsfunktionen zu "mvwizr" (github.com/ror-at-ebp/mvwizr), welches zur visuellen Auswertung von Mikroverunreinigungen in Schweizer Gewässern von der EBP im Auftrag des Gewässer- und Bodenschutzlabor des Kantons Bern (GBL) erstellt wurde. 
Bei den Zusatzfunktionen handelt es sich in erster Linie um eine Datenaufbereitung des LIMS-Gewässerschutzdatenexport für eine erfolgreiche Weiterverarbeitung durch mvwizr.  

## Installation und Updates
mvwizrAWEL wird direkt von github installiert und benötigt mindestens R 4.1. Für die Installation auf einem lokalen Rechner müssen folgende Kommandos ausgefürt werden: 
```r
install.packages("remotes") # falls noch nicht installiert
remotes::install_github("awel-gewaesserschutzlabor/mvwizrAWEL", build = TRUE, build_vignettes = TRUE, dependencies = TRUE)
```
Bei der Installation von mvwizrAWEL wird mvwizr automatisch mitinstalliert und geladen. Bei jedem Laden von mvwizrAWEL wird das zugrundeliegende Paket mvwizr automatisch auf github auf Updates kontrolliert und bei Bedarf neu installiert, d.h. mvwizr sollte automatisch immer auf dem neusten Stand sein.
Dadurch wird auch garantiert, dass die hinterlegten Daten des VSA, BAFU und Oekotoxzentrums immer auf dem neusten Stand sind. Um mvwizrAWEL zu aktualisieren, kann der gleiche Befehl wie bei der Installation verwendet werden. 

## Verwendung
Nach der Installation kann mvwizrAWEL bzw. mvwizr in Scripts oder in der Konsole verwendet werden, indem das Paket geladen wird:
```r
library(mvwizrAWEL)
```
Durch Eingabe von ```?funktion``` in der Konsole kann die Hilfeseite einer bestimmten Funktion aufgerufen werden, also z.B. ```?einlesen_mv_awel```.

## Funktionen
Alle verfügbaren Funktionen sind in der Datei R/parsing.R enthalten. 

### einlesen_mv_awel()
Dabei handelt es sich in erster Linie um eine wrapper-Funktion von einlesen_mv_gbl aus mvwizr. Die Funktion liest die im LIMS generierte .txt Datei (Exportfilter_DB_Daten_mvwizr) mit den MV-Daten ein, bereinigt die Tabelle, berechnet bei bedarf aus Tagesmischproben 2-Wochenmischproben und verarbeitet sie 
innerhalb der Funktion mit ```einlesen_mv_gbl()```. Für eine detaillierte Funktionsbeschreibung, Spezifikation der Parameter und Beispiele, rufe in RStudio nach Laden des Pakets in der Konsole die Hilfeseite ```?einlesen_mv_awel``` auf. 

### berechnung_mischproben()
Dabei handelt es sich um eine interne Funktion zur Berechnung von 2-Wochenmischproben auf Basis der 3-/4-Tagesmischproben wärend der sommerlichen Probenintervallverdichtung, damit in diesem Zeitraum ebenfalls chronische Toxizitäten beurteilt werden können.
Sie wird innerhalb von ```einlesen_mv_awel()``` verwendet und kann nicht explizit aufgerufen werden. Falls in der Funktion ```einlesen_mv_awel()``` das Argument ```bSaP = TRUE``` ist, also berechnete Mischproben verwendet werden sollen, 
verrechnet die Funktion Proben, welche kürzer als 8 Tage sind, zeitproportional zu 2-Wochenmischproben. Um zu vermeiden, dass einzelne Probenausfälle zu Verschiebungen der hypothetischen 2-Wochenintervalle während der Verdichtung führen, werden die 2-Wochenintervalle aufgrund eines Referenzdatums (pro Jahr und Station)
festgelegt. Dies geschieht entweder automatisch durch Auswahl des Startdatums der ersten Probe pro Jahr, die weniger als acht Tage lang gesammelt wurde, oder manuell durch Übergabe eines Vektors an die Funktion, der die jeweiligen Startdaten der Probenverdichtung pro Jahr enthält, z.B. ```startdatum_bSaP = "25.03.2024"```. 
Sollte die automatische Schätzung des Beginn der Probenahmeverdichtung ein Datum ergeben, dass vor dem 15. März des jeweiligen Jahres liegt, wird als Hinweis auf einen möglichen Fehler in der Referenzdatenschätzung eine Warnmeldung ausgegeben. 
