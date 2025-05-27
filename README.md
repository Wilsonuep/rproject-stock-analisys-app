# Aplikacja Analityczna Instrumentów Finansowych

## Spis treści
1. [Opis projektu](#opis-projektu)
2. [Struktura projektu](#struktura-projektu)
3. [Instalacja](#instalacja)
4. [Używanie renv](#używanie-renv)
5. [Uruchomienie aplikacji](#uruchomienie-aplikacji)
6. [Funkcjonalności](#funkcjonalności)
7. [Lista zadań (TODO)](#lista-zadań-todo)
8. [Autorzy](#autorzy)

## Opis projektu

Aplikacja służy do analizy wybranych instrumentów finansowych, monitorowania ich wartości oraz predykcji przyszłych wartości za pomocą różnych modeli. Aplikacja została stworzona przy użyciu R Shiny i umożliwia użytkownikowi interaktywne przeglądanie danych giełdowych, porównywanie różnych instrumentów oraz budowanie modeli predykcyjnych.

Projekt powstał na zaliczenie przedmiotu: **Podstawy programowania z R**.

## Struktura projektu

Aby zachować porządek i umożliwić współpracę wielu osób przy projekcie, kod aplikacji został podzielony na kilka plików:

```
my_shiny_app/
├── app.R           # Główny plik aplikacji
├── global.R        # Zmienne globalne i funkcje ładowane przed UI i serwerem
├── ui/
│   ├── ui.R        # Główna definicja UI
│   ├── sidebar.R   # Definicja paska bocznego
│   └── tabs/
│       ├── overview_tab.R
│       ├── data_tab.R
│       ├── indices_tab.R
│       ├── stock_tab.R
│       ├── currency_tab.R
│       ├── predictions_tab.R
│       └── about_tab.R
├── server/
│   ├── server.R    # Główna funkcja serwera
│   ├── overview.R  # Logika serwera dla zakładki overview
│   ├── data.R      # Logika serwera dla eksploratora danych
│   ├── indices.R   # Logika serwera dla indeksów
│   ├── stocks.R    # Logika serwera dla akcji
│   ├── currency.R  # Logika serwera dla walut
│   └── predictions.R # Logika serwera dla modeli predykcyjnych
├── helpers/
│   ├── data_functions.R      # Funkcje do przetwarzania danych
│   ├── plotting_functions.R  # Funkcje do tworzenia wykresów
│   └── prediction_models.R   # Funkcje modeli predykcyjnych
└── renv/                     # Katalog zarządzania zależnościami
    └── ...
```

## Instalacja

1. Sklonuj to repozytorium:
```
git clone https://github.com/Wilsonuep/rproject-stock-analisys-app.git
cd rproject-stock-analisys-app
```

2. Upewnij się, że masz zainstalowany pakiet renv:
```R
install.packages("renv")
```

3. Zainicjuj środowisko renv i zainstaluj zależności:
```R
renv::restore()
```

## Używanie renv

Projekt wykorzystuje pakiet `renv` do zarządzania zależnościami, co zapewnia, że wszyscy współpracownicy używają tych samych wersji pakietów.

### Podstawowe komendy renv:

* **Inicjalizacja projektu z renv**:
```R
renv::init()
```
Ta komenda została już wykonana dla tego projektu, więc nie musisz jej uruchamiać ponownie.

* **Instalacja zależności**:
```R
renv::restore()
```
Zainstaluje wszystkie pakiety w wersjach określonych w pliku lock.

* **Aktualizacja pliku lock po dodaniu nowych pakietów**:
```R
renv::snapshot()
```
Zapisuje aktualny stan pakietów do pliku lock.

* **Instalacja nowego pakietu**:
```R
install.packages("nazwa_pakietu")
renv::snapshot()  # aby zaktualizować lock
```

* **Sprawdzenie statusu pakietów**:
```R
renv::status()
```

Więcej informacji: [dokumentacja renv](https://rstudio.github.io/renv/)

## Uruchomienie aplikacji

Aby uruchomić aplikację:

1. Otwórz plik `app.R` w RStudio
2. Kliknij przycisk "Run App" lub wykonaj:
```R
shiny::runApp()
```

Lub uruchom z linii komend:
```
Rscript -e "shiny::runApp('ścieżka/do/aplikacji')"
```

## Funkcjonalności

Aplikacja składa się z kilku modułów (zakładek):

1. **Overview** - Podsumowanie najważniejszych danych i wskaźników
2. **Eksplorator danych** - Przeglądanie i analiza surowych danych finansowych
3. **Giełda indeksów** - Szczegółowa analiza indeksów giełdowych
4. **Giełda akcji** - Szczegółowa analiza akcji
5. **Giełda walut** - Szczegółowa analiza kursów walutowych
6. **Model predykcji** - Budowanie i testowanie modeli predykcyjnych
7. **About** - Informacje o projekcie i autorach

## Lista zadań (TODO)

Poniżej znajduje się podział zadań między członków zespołu:

### Podstrony aplikacji:

1. **Overview** - Mikołaj (@AN0DA)
   - [ ] Implementacja infoBoxów z największymi zmianami
   - [ ] Wykres zbiorczy dla indeksów
   - [ ] Wykres zbiorczy dla akcji
   - [ ] Wykres zbiorczy dla walut

2. **Eksplorator danych** - Marek (@elogawron)
   - [ ] Implementacja ładowania danych
   - [ ] Tabele danych dla indeksów, akcji i walut
   - [ ] Filtrowanie i sortowanie danych

3. **Giełda indeksów** - Łukasz (@L-debski) & Wiktor (@WiktorPietrzynski)
   - [ ] Implementacja porównania wyników indeksów
   - [ ] Obliczanie metryk wydajności
   - [ ] Analiza ryzyka i zwrotu
   - [ ] Wyświetlanie największych zysków i strat

4. **Giełda akcji** - Łukasz (@L-debski) & Wiktor (@WiktorPietrzynski)
   - [ ] Implementacja porównania wyników akcji
   - [ ] Obliczanie metryk wydajności
   - [ ] Analiza ryzyka i zwrotu
   - [ ] Wyświetlanie największych zysków i strat

5. **Giełda walut** - Łukasz (@L-debski) & Wiktor (@WiktorPietrzynski)
   - [ ] Implementacja porównania wyników walut
   - [ ] Obliczanie metryk wydajności
   - [ ] Analiza ryzyka i zwrotu
   - [ ] Wyświetlanie największych zysków i strat

6. **Model predykcji** - Piotr (@Wilsonuep)
   - [ ] Implementacja modelu naiwnego
   - [ ] Implementacja modelu ARIMA
   - [ ] Implementacja modelu las losowy
   - [ ] Implementacja modelu XGBoost
   - [ ] Wizualizacja wyników predykcji
   - [ ] Tabela dokładności modelu

7. **About**
   - [x] Podstawowe informacje o projekcie
   - [x] Lista autorów


## Autorzy

- Piotr Wilma (@Wilsonuep) - 124832
- Mikołaj Kaczmarek (@AN0DA) - 124942
- Wiktor Pietrzyński (@WiktorPietrzynski)
- Łukasz Dębski (@L-debski)
- Marek Gawron (@elogawron)

**Źródło danych**: Yahoo Finance (via pakiet quantmod)