#' Wczytuje plik ze zdefiniowanym kluczem
#' 
#' @description
#' Wczytuje klucz zapisany w pliku tekstowym za pomocą funkcji
#' \code{\link[utils]{read.csv2}}.
#' 
#' W pliku, nazwy zadań powinny byc zdefiniowane w kolumnie 'item' i być
#' powtórzone tyle razy, ile wartości zdefiniowano dla danego zadania do
#' zrekodowania. W kolumnie "value" powinny znajdować się wartości zdefiniowane
#' dla zadania w oryginalnej bazie danych. W kolumnie "recode" powinny
#' znajdować się odpowiadające im wartości docelowe.
#' 
#' @param file character wskazujący plik .csv z kluczem.
#' @param ... inne argumenty przekazane do \code{\link[utils]{read.csv2}}.
#' 
#' @return
#' Lista z nazwami odpowiadającymi zadaniom, której elementami są data.frame'y
#' z kolumnami "values" i "recode" z klucza.
#' 
#' @export
#' 
readKey = function(file, ...) {
  # wczytanie pliku
  code_df = read.csv2(file, stringsAsFactors = FALSE, ...)
  # wyłowienie nazw itemów
  items = unique(code_df$item)
  
  # wyłonienie 'values' i 'recodes'
  key = list()
  for (it in items) {
    key[[it]] = code_df[code_df$item == it, c("value", "recode")]
  }
  key
}