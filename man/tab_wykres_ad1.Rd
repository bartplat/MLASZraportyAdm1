% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabele_do_wykresow.R
\name{tab_wykres_ad1}
\alias{tab_wykres_ad1}
\title{Konwersja wskaźników w formie list na tabele do wsadu do wykresu}
\usage{
tab_wykres_ad1(dane_szk, dane_woj, dane_kraj, wojewodztwo_dop, etykiety)
}
\arguments{
\item{dane_szk}{lista zawierająca wskaźniki na poziomie agregacji: szkoła
(lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)}

\item{dane_woj}{lista zawierająca wskaźniki na poziomie agregacji:
województwo (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)}

\item{dane_kraj}{lista zawierająca wskaźniki na poziomie agregacji: cały kraj
(lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)}

\item{wojewodztwo_dop}{nazwa województwa, w którym znajduje się dana szkoła w
formie dopełniacza (lub wartość NULL jeśli brak takiej potrzeby)}

\item{etykiety}{lista etykiet wierszy tabeli, która ma być użyta potem jako
etykiety wartości na wykresie. Nazwy etykiet nie powinny zawierać nazwy
kolumny "n".}
}
\value{
ramka danych w formacie tibble
}
\description{
Na potrzeby raportu funkcja tworzy z 3 list zawierających
wskaźniki na różnych poziomach agregacji: szkoła, województwo i cały kraj,
ramkę danych, która następnie jest używana jako wsad do funkcji rysujących
wyresy.
}
