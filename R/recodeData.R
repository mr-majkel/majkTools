#' Rekoduje zbiór danych zgodnie z kluczem.
#' 
#' @description
#' Rekoduje zbiór danych zgodnie z kluczem. Klucz powinien być nazwaną listą.
#' Nazwy elementów \code{key_list} powinny odpowiadać nazwom kolumn w
#' \code{raw_df}. Dla każdej kolumny, która ma być zrekodowana, definicja
#' wartości wyjściowych oraz docelowych powinna przyjmować postać w data.frame,
#' z kolumnami o nazwach 'value' (wartości wyjściowe) i 'recode'
#' (wartości docelowe).
#' 
#' @param raw_df data.frame z bazą do zrekodowania.
#' @param key_list lista z kluczem.
#' 
#' @return
#' Data.frame, w którym dla kolumn wymienionymi w
#' \code{key_list} odpowiednie wartości w kolumnie 'value' zostały podmienione
#' na wartości z kolumny 'recode'. Kolumny nie wymienione w \code{key_list} są
#' zwracane bez zmian.
#' 
#' Funkcja zwraca kolumnę o klasie odpowiadającej klasie
#' kolumny 'recode' w kluczu (obsługuje cztery klasy kolumn: character, numeric,
#' integer i logical).
#' 
#' @details
#' Podczas wyszukiwania i nadpisywania wartości z klucza usuwa spacje z początku
#' i końca w przeszukiwanej kolumnie oraz z wartości wyjściowych.
#' 
#' @export
#' 

recodeData = function(raw_df, key_list) {
  recoded_df = raw_df
  items = names(key_list)
  
  for(it in items) {
    it_vals = key_list[[it]]$value
    it_recodes = key_list[[it]]$recode
    recode_type = typeof(it_recodes)
    # pozbywa się spacji na początku i końcu
    it_vals = gsub("^ *| *$", "", it_vals)
    # it_recodes = gsub("^ *| *$", "", it_recodes)                
    for (v in it_vals) {
      dat_vec = gsub("^ *| *$", "", raw_df[, it]) # bez spacji na początku i końcu
      recode_ind = which(dat_vec == v)
      recoded_df[recode_ind, it] = it_recodes[which(it_vals == v)]
    }
    # zwróć pożądaną klasę
    recoded_df[, it] = switch(recode_type,
                              character = as.character(recoded_df[, it]),
                              double = as.numeric(recoded_df[, it]),
                              integer = as.integer(recoded_df[, it]),
                              logical = as.logical(recoded_df[, it]),
                              ... = recoded_df[, it])
  }
  recoded_df
}
