
# GErapcg: R-Package für die parallele Berechnung des Risikoausgleichs

[![R-CMD-check](https://github.com/gekvgra/GErapcg_public/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gekvgra/GErapcg_public/actions/workflows/R-CMD-check.yaml)

## Voraussetzungen

Die Parallelberechnung ist in der Programmiersprache R geschrieben und
eine Installation von R, RStudio sowie der benötigten R Pakete ist somit
notwendig. Ansonsten sind keine weiteren Kenntnisse notwendig.

## Installation

<div class="GErapcg-release">

``` r
# Produktive Version
remotes::install_github("https://github.com/gekvgra/GErapcg_public.git")
```

</div>

## Verwendung

Die Implementierung der Parallelberechnung umfasst 6 Rscripts:

- Setup und Laden der Individualdaten
- Laden der PCG-Liste
- Zuordnung der PCG zu den Individualdaten und Hierarchisierung
- Preprocessing
- Berechnung
- PCG Nachweis

Um die Berechnung durchzuführen gibt es zwei Möglichkeiten. Die
schnellere davon ist die Hilfsfunktion `compute_ra`. Alternativ liegt
dem Package eine RMarkdown-Vorlage mit dem Namen *GE KVG
Parallelberechnung Risikoausgleich* bei. Ein neues, auf dieser Vorlage
basierendes Dokument kann über den folgenden Ablauf erzeugt werden:

> File \> New File \> RMarkdown… \> From Template \> Parallelberechnung

Um die zugehörigen Rscripts genauer zu inspizieren können die
auskommentierten Zeilen beginnend mit `file.edit` ausgeführt werden. Die
im Code verwendeten Variablennamen folgen dabei im wesentlichen der
Nomenklatur, die in der Publikation [Berechnungsformel für den
Risikoausgleich mit PCG ab
2020](https://www.bag.admin.ch/bag/de/home/versicherungen/krankenversicherung/krankenversicherung-versicherer-aufsicht/risikoausgleich.html)
verwendet wird.

## WICHTIG

Die im Package enthaltenen Daten enthalten zufällig generierte Daten mit
1000 Zeilen für jeden Kanton für drei Versicherer. Die Ausgabeobjekte
aus diesen zufällig generierten Daten sind also eindeutig nicht
identisch mit den Ausgabeobjekten von SORA PCG. Wenn die GE KVG jedoch
die Originaldaten des Risikoausgleichs mit diesem Package verwendet,
sind die Ergebnisse identisch mit den Ergebnissen von SORA PCG. Für
Dritte gibt es keine Möglichkeit diese Ergebnisse zu reproduzieren, da
die Originaldaten per Gesetz nur der GE KVG und dem BAG zugänglich sind
(siehe VORA Art. 26).

Bei Fragen oder Unklarheiten stehen wir Ihnen gerne zur Verfügung.

<sora@kvg.org>
