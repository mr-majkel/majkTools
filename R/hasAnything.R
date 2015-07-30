#' Znajdź wiersze, które coś mają
#' 
#' @description Zwraca wektor logiczny z informacją, które wiersze z data.frame lub macierzy, które
#' przynajmniej na jednej kolumnie posiadają wartość (tj. nie NA lub NaN).
#' 
#' @param data.frame lub macierz z wierszami do sprawdzenia
#' 
#' @return Wektor logiczny mówiący, które wiersze posiadają wartość w
#' którejkolwiek kolumnie z \code{df}.
#' 
#' @export

hasAnything = function(df) {
  # liczba kolumn
  nc = ncol(df)
  
  # suma braków danych
  sum_na = apply(df, 1, function(row) sum(is.na(row)))
  
  # które wiersze coś mają
  sum_na < nc
}
