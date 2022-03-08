![KL+RP+IBE+EFS](inst/Belka-Losy-absolwentow-Kolor-PL.png)

# MLASZraportyAdm1

Pakiet został opracowany w ramach projektu *Monitorowanie losów edukacyjno-zawodowych absolwentów i młodych dorosłych* (POWR.02.15.00-IP.02-00-004/16) prowadzonego w Instytucie Badań Edukacyjnych w ramach działania 2.15. Kształcenie i szkolenie zawodowe dostosowane do potrzeb zmieniającej się gospodarki II. osi priorytetowej Efektywne polityki publiczne dla rynku pracy, gospodarki i edukacji Programu Operacyjnego Wiedza, Edukacja, Rozwój.

Pakiet `MLASZraportyAdm1` zawiera szablony raportów napisane w języku `R Markdown` oraz zbiór funkcji używanych w szablonach np. do generowania wykresów. Pakiet zawiera 3 szablony - po jednym dla każdego typu szkoły, dla którego udostępniane są wyniki pilotażu 1. edycji Monitoringu Losów Absolwentów z użyciem danych administracyjnych (resjestry: CIE, ZUS, POLON i CKE). Pakiet `MLASZraportyAdm1` zawiera jedynie szablony i używane przez nie funkcje, natomiast, żeby generować przy jego pomocy raporty wymagany jest drugi pakiet zawierający zbiór funkcji będących silnikiem raportowania - [`MLASZraporty`](https://github.com/bartplat/MLASZraporty). Zbiory danych ze wskaźnikami, których można użyć do generowania raportów pakietem `MLASZraportyAdm1` można stworzyć przy użyciu pakietu [`MLASZdaneAdm1`](https://github.com/bartplat/MLASZdaneAdm1)

# Instalacja / aktualizacja

Pakiet nie jest dostępny na CRAN, więc trzeba instalować go ze źródeł.

Instalację najprościej przeprowadzić wykorzystując pakiet *devtools*:

```r
install.packages('devtools') # potrzebne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('bartplat/MLASZraportyAdm1', build_opts = c("--no-resave-data"))
```
Pakiet `MLASZraportyAdm1` jest zależny od pakietu `MLASZraporty`, ale nie ma potrzeby go dodatkowo instalować, ponieważ dzieje się to podczas instalacji pakietu `MLASZraportyAdm1`.

Dokładnie w ten sam sposób można przeprowadzić aktualizację pakietu do najnowszej wersji.

# Użycie

Do generowania raportów służy funkcja `generuj_raporty()` z pakietu `MLASZraporty`. Jej przykładowe wywołanie dla szablonu raportu dla branżowych szkół 1. stopnia wygląda następująco:

```r
library(MLASZraportyAdm1)
generuj_raporty(
  pakiet = "MLASZraportyAdm1",
  szablon = "admin_1_bs1.Rmd",
  wskazniki = szk,
  wskaznikiGrPor = woj,
  kolumnaNazwaPliku = nazwa_pliku,
  parametry = list(typDokumentu = "pdf",
                   progLiczebnosci = 10,
                   wyrownanieTabWykr = "center",
                   plikZObiektami = "/data/wskazniki/wsk_bs1.RData",
                   obiektWskazniki = "szk",
                   obiektWskaznikiPorownanie = "woj",
                   obiektWskaznikiKraj = "kraj")
)
```
Raporty zostaną utworzone w aktywnym folderze. Jeśli nie jesteś pewien, jaki to folder, użyj funkcji `getwd()` i ew. funkcji `setwd()`, aby go zmienić.

Zbiór wskaźników, oprócz kolumn wygenerowanych za pomocą pakietu `MLASZdaneAdm1`, powinien zawierać także kolumnę z nazwą każdego z generowanych raportów - argument `kolumnaNazwaPliku` w funkcji `generuj_raporty()`. Nie jest ona tworzona w wyniku działania funkcji pakietu `MLASZdaneAdm1`, więc trzeba ją dodać samemu. Przykładowe nazwy plików składające się z ID RSPO szkoły oraz dopisku "raport_v1" można stworzyć w następujący sposób:

```r
szk = szk %>% 
  mutate(nazwa_pliku = paste0(ID_SZK, "raport_v1"))
```
