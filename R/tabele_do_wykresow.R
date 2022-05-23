#' @title Konwersja wskaźników w formie list na tabele do wsadu do wykresu
#' @description Na potrzeby raportu funkcja tworzy z 3 list zawierających
#' wskaźniki na różnych poziomach agregacji: szkoła, województwo i cały kraj,
#' ramkę danych, która następnie jest używana jako wsad do funkcji rysujących
#' wyresy.
#' @param dane_szk lista zawierająca wskaźniki na poziomie agregacji: szkoła
#' (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param dane_woj lista zawierająca wskaźniki na poziomie agregacji:
#' województwo (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param dane_kraj lista zawierająca wskaźniki na poziomie agregacji: cały kraj
#' (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param wojewodztwo_dop nazwa województwa, w którym znajduje się dana szkoła w
#' formie dopełniacza (lub wartość NULL jeśli brak takiej potrzeby)
#' @param etykiety lista etykiet wierszy tabeli, która ma być użyta potem jako
#' etykiety wartości na wykresie. Nazwy etykiet nie powinny zawierać nazwy
#' kolumny "n".
#' @export
#' @return ramka danych w formacie tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr select everything mutate
#' @importFrom tidyr pivot_longer
tab_wykres_ad1 = function(dane_szk, dane_woj, dane_kraj, wojewodztwo_dop, etykiety) {
  stopifnot(is.list(dane_szk) | is.null(dane_szk),
            is.list(dane_woj) | is.null(dane_woj),
            is.list(dane_kraj) | is.null(dane_kraj))
  if (!is.null(wojewodztwo_dop)) {
    stopifnot(is.character(wojewodztwo_dop),
              length(wojewodztwo_dop) %in% 1,
              nchar(wojewodztwo_dop) > 1)
  }

  # etykiety = ordered(etykiety, etykiety)

  if (!is.null(dane_szk)) {
    tab_szk = dane_szk %>%
      as_tibble() %>%
      select(-n) %>%
      pivot_longer(everything()) %>%
      mutate(lab = ifelse(round(value, 2) >= 0.06,
                          paste0(round(value * 100), "%"),
                          ""),
             typ = "Państwa szkoła")
    tab_szk$name = etykiety
  } else {
    tab_szk = NULL
  }

  if (!is.null(dane_woj)) {
    tab_woj = dane_woj %>%
      as_tibble() %>%
      select(-n) %>%
      pivot_longer(everything()) %>%
      mutate(lab = ifelse(round(value, 2) >= 0.06,
                          paste0(round(value * 100), "%"),
                          ""),
             typ = paste0("Pozostałe branżowe szkoły I stopnia\nz województwa\n", wojewodztwo_dop))
    tab_woj$name = etykiety
  } else {
    tab_woj = NULL
  }

  if (!is.null(dane_kraj)) {
    tab_kraj = dane_kraj %>%
      as_tibble() %>%
      select(-n) %>%
      pivot_longer(everything()) %>%
      mutate(lab = ifelse(round(value, 2) >= 0.06,
                          paste0(round(value * 100), "%"),
                          ""),
             typ = "Branżowe szkoły I stopnia\nw całej Polsce")
    tab_kraj$name = etykiety
  } else {
    tab_kraj = NULL
  }

  tab = rbind(tab_szk, tab_woj, tab_kraj)
  return(tab)
}
#' @title Konwersja wskaźników w formie list na tabele do wsadu do wykresu
#' @description Na potrzeby raportu funkcja tworzy z 3 list zawierających
#' wskaźniki na różnych poziomach agregacji: szkoła, województwo i cały kraj,
#' ramkę danych, która następnie jest używana jako wsad do funkcji rysującej
#' wyres \code{wyk_facet_6_ad1()}.
#' @param dane_szk lista zawierająca wskaźniki na poziomie agregacji: szkoła
#' (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param dane_woj lista zawierająca wskaźniki na poziomie agregacji:
#' województwo (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param dane_kraj lista zawierająca wskaźniki na poziomie agregacji: cały kraj
#' (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param wojewodztwo_dop nazwa województwa, w którym znajduje się dana szkoła w
#' formie dopełniacza (lub wartość NULL jeśli brak takiej potrzeby)
#' @export
#' @return ramka danych w formacie tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr select starts_with mutate arrange
tab_facet_ad1 = function(dane_szk, dane_woj, dane_kraj, wojewodztwo_dop) {
  stopifnot(is.list(dane_szk) | is.null(dane_szk),
            is.list(dane_woj) | is.null(dane_woj),
            is.list(dane_kraj) | is.null(dane_kraj))
  if (!is.null(wojewodztwo_dop)) {
    stopifnot(is.character(wojewodztwo_dop),
              length(wojewodztwo_dop) %in% 1,
              nchar(wojewodztwo_dop) > 1)
  }

  if (!is.null(dane_szk)) {
    tab_szk = dane_szk %>%
      as_tibble() %>%
      select(mies = starts_with("l_mies_"), value, -c(srednia, mediana)) %>%
      mutate(typ = "Państwa szkoła") %>%
      arrange(mies)
    tab_szk$typ = as.factor(tab_szk$typ)
  } else {
    tab_szk = NULL
  }

  if (!is.null(dane_woj)) {
    tab_woj = dane_woj %>%
      as_tibble() %>%
      select(mies =  starts_with("l_mies_"), value, -c(srednia, mediana)) %>%
      mutate(typ = paste0("Pozostałe branżowe szkoły I stopnia\nz województwa\n", wojewodztwo_dop)) %>%
      arrange(mies)
    tab_woj$typ = as.factor(tab_woj$typ)
  } else {
    tab_woj = NULL
  }

  if (!is.null(dane_kraj)) {
    tab_kraj = dane_kraj %>%
      as_tibble() %>%
      select(mies =  starts_with("l_mies_"), value, -c(srednia, mediana)) %>%
      mutate(typ = "Branżowe szkoły I stopnia\nw całej Polsce") %>%
      arrange(mies)
    tab_kraj$typ = as.factor(tab_kraj$typ)
  } else {
    tab_kraj = NULL
  }

  tab = rbind(tab_szk, tab_woj, tab_kraj)
  return(tab)
}
#' @title Konwersja wskaźników w formie list na tabele do wsadu do wykresu
#' @description Na potrzeby raportu funkcja tworzy z 3 list zawierających
#' wskaźniki na różnych poziomach agregacji: szkoła, województwo i cały kraj,
#' ramkę danych, która następnie jest używana jako wsad do funkcji rysującej
#' wyres \code{wyk_facet_6_ad1()}.
#' @param dane_szk lista zawierająca wskaźniki na poziomie agregacji: szkoła
#' (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param dane_woj lista zawierająca wskaźniki na poziomie agregacji:
#' województwo (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param dane_kraj lista zawierająca wskaźniki na poziomie agregacji: cały kraj
#' (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param wojewodztwo_dop nazwa województwa, w którym znajduje się dana szkoła w
#' formie dopełniacza (lub wartość NULL jeśli brak takiej potrzeby)
#' @export
#' @return ramka danych w formacie tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr select starts_with mutate arrange
tab_facet_z4_ad1 = function(dane_szk, dane_woj, dane_kraj) {
  stopifnot(is.list(dane_szk) | is.null(dane_szk),
            is.list(dane_woj) | is.null(dane_woj),
            is.list(dane_kraj) | is.null(dane_kraj))
  if (!is.null(wojewodztwo_dop)) {
    stopifnot(is.character(wojewodztwo_dop),
              length(wojewodztwo_dop) %in% 1,
              nchar(wojewodztwo_dop) > 1)
  }

  if (!is.null(dane_szk)) {
    tab_szk = dane_szk %>%
      as_tibble() %>%
      select(mies = starts_with("l_mies_"), value, -c(srednia, mediana)) %>%
      mutate(typ = "Państwa szkoła") %>%
      arrange(mies)
    tab_szk$typ = as.factor(tab_szk$typ)
  } else {
    tab_szk = NULL
  }

  if (!is.null(dane_woj)) {
    tab_woj = dane_woj %>%
      as_tibble() %>%
      select(mies =  starts_with("l_mies_"), value, -c(srednia, mediana)) %>%
      mutate(typ = paste0("Pozostałe branżowe szkoły I stopnia\nz województwa\n", wojewodztwo_dop)) %>%
      arrange(mies)
    tab_woj$typ = as.factor(tab_woj$typ)
  } else {
    tab_woj = NULL
  }

  if (!is.null(dane_kraj)) {
    tab_kraj = dane_kraj %>%
      as_tibble() %>%
      select(mies =  starts_with("l_mies_"), value, -c(srednia, mediana)) %>%
      mutate(typ = "Branżowe szkoły I stopnia\nw całej Polsce") %>%
      arrange(mies)
    tab_kraj$typ = as.factor(tab_kraj$typ)
  } else {
    tab_kraj = NULL
  }

  tab = rbind(tab_szk, tab_woj, tab_kraj)
  return(tab)
}
