about_tab <- tabItem(tabName = "about",
  h2("O autorach"),
  box(width = 12,
      h3("Aplikacja powstała na zaliczenia przedmiotu: Podstawy programowania z R"),
      p("Aplikacja służy do analizy wybranych insturmentów finansowych, monitorowania ich wartości oraz
      predykcji przyszłych za pomocą różnych modeli predykcji. (\n)
        Modele zostały dobrane na podstawie dokładności w predykcji na podstawie jednej cechy oraz niskich wymagań założeń"),
      p("Autorzy"),
      tags$ul(
        tags$li("Piotr Wilma (124832)"),
        tags$li("Mikołaj Kaczmarek (124942)"),
        tags$li("Wiktor Pietrzyński (125134)"),
        tags$li("Łukasz Dębski (126759)"),
        tags$li("Marek Halber (122814)"),
      ),
      h4("Źródło danych: Yahoo Finance (via quantmod package)"),
  )
)
