# Zmienia format codebooka
# Funkcja pomocnicza do getKey
# Argumenty
# cdbook      data.frame z surowym codebookiem.
# var_names   string pozwalający na zidentyfikowanie kolumny z nazwami
#             zmiennych.
# val_names   string pozwalający na zidentyfikowanie kolumny z etykietami
#             wartości.
# Wartość
# Data.frame z codebookiem z kolumną 'var_id', w której znajdują się nazwy
# zmiennych oraz kolumną 'values', w której znajdują się etykiety wartości.
#
reformat = function(cdbook, var_names = "Nazwa", val_names = "Etykiety") {
  # zapytanie do grepa
  grep_querry = paste(c(var_names, val_names), collapse = "|")
  # wybranie zmiennych z codebooka
  cdbook = cdbook[, grep(grep_querry, names(cdbook))]
  
  # zmiana nazw zmiennych na bardziej przyjazne
  names(cdbook) = c("var_id", "values")
  
  # zwraca data.frame z wybranymi kolumnami
  return(cdbook)
}

# Funkcja pomocnicza do getKey().
# Wyciąga itemy z codebooka. Zwraca wektor tekstowy z nazwami zmiennych.
# Argumenty
# cdbook  data.frame z codebookiem, w którym nazwy zmiennych znajdują się w
#         kolumnie 'var_id'.
# vars    string z zapytaniem do grep() lub wektor tekstowy z nazwami
#         zmiennych.
getItems = function(cdbook, vars) {    
  # stworzenie listy itemów
  grep_querry = paste(vars, collapse = "|")
  items = grep(grep_querry, cdbook$var_id, value = TRUE)
  # zwraca wektor z nazwami itemów    
  return(items)
}

# Wyciąga wartości zmiennych z definicji w codebooku
# Argumenty
# valnames    string z definicją etykiet wartości w codebooku
# Uwagi
# Zakłada, że etykiety dla kolejnych wartości zmiennej opisane są w nowej
# linii, a także, że wartości od swoich etykiet rozdzielone są dwukropkiem
# (':').
# Wartość
# Wektor tekstowy z wartościami, dla których zdefiniowane zostały etykiety.
getValues = function (valnames) {
  # łamie etykiety wartości po znaku nowej linii
  valnames = unlist(strsplit(valnames, "\n"))
  
  # łamie etykiety weartości po dwukropku
  values = unlist(lapply(strsplit(valnames, ":"), "[", 1))
  
  # zwraca wektor tekstowy z kodami
  return(values)
}