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
#' @export
#' 

recodeData = function(raw_df, key_list) {
    recoded_df = raw_df
    items = names(key_list)
    
    for(it in items) {
        it_vals = key_list[[it]]$value
        it_recodes = key_list[[it]]$recode
        for (v in it_vals) {
            recode_ind = which(raw_df[, it] == v)
            recoded_df[recode_ind, it] = it_recodes[which(it_vals == v)]
        }
    }
    # recoded_df[, items] = data.frame(recoded_df[, items],
    #                                  stringsAsFactors = FALSE)
    recoded_df
}
