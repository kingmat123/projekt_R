Projekt zaliczeniowy: pakiet R do analizy szeregów czasowych sprzedaży.

## Co robi projekt?

Pakiet wykonuje cały proces analizy danych sprzedażowych:

1. Wczytuje dane sprzedażowe.
2. Sprawdza jakość danych.
3. Czyści dane.
4. Liczy metryki biznesowe.
5. Tworzy wykresy trendów.
6. Generuje podsumowanie dla menedżera.
7. Tworzy prognozę sprzedaży metodami ARIMA i Prophet.
8. Generuje raport PDF.
9. Ma prosty panel Shiny.

## Struktura projektu

```text
salesTSKinga_FINAL/
├── R/funkcje.R
├── data/train.csv
├── data/stores.csv
├── data/holidays_events.csv
├── raport/raport.Rmd
├── app.R
├── workflow.R
├── github_setup.R
├── DESCRIPTION
├── NAMESPACE
├── README.md
└── salesTSKinga.Rproj
```

## Instalacja pakietów

```r
install.packages(c("devtools", "roxygen2", "dplyr", "tidyr", "readr", "lubridate", "ggplot2", "slider", "forecast", "prophet", "tibble", "knitr", "rmarkdown", "tinytex", "shiny", "DT", "scales"))
```

Do generowania PDF potrzebny jest LaTeX. Najprościej zainstalować TinyTeX:

```r
tinytex::install_tinytex()
```


## Uruchomienie projektu

```r
devtools::document()
devtools::load_all()
source("workflow.R")
```

## Generowanie raportu PDF

```r
rmarkdown::render("raport/raport.Rmd")
```

Raport PDF używa szybkiej prognozy ARIMA, żeby dokument generował się sprawnie. Pełna prognoza ARIMA + Prophet jest dostępna w funkcji `create_prognosis()` i w pliku `workflow.R`.

## Panel Shiny

```r
shiny::runApp("app.R")
```

## GitHub

```r
source("github_setup.R")
```
