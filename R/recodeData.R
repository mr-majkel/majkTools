#' Rekoduje zbiór danych zgodnie z kluczem.
#' 
#' @param raw_df data.frame z bazą do zrekodowania.
#' @param key_list lista zawierająca .
#' 
#' @return
#' Data.frame, w którym dla kolumn wyspecyfikowanych w \code{key_list}
#' odpowiednie wartości w 'values' zostały podmienione na wartości z 'recodes'.
#' Kolumny nie zdefiniowane w \code{key_list} są zwracane bez zmian.
#' 
#' @export
#' 

recodeData = function(raw_df, key_list) {
    recoded_df = raw_df
    items = key_list[["items"]]
    
    for(it in items) {
        it_vals = key_list[["values"]][[it]]
        it_recodes = key_list[["recodes"]][[it]]
        for (v in it_vals) {
            recode_ind = which(raw_df[, it] == v)
            recoded_df[recode_ind, it] = it_recodes[which(it_vals == v)]
        }
    }
    recoded_df[, items] = data.frame(lapply(recoded_df[, items], as.numeric))
    recoded_df
}
