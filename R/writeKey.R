#' Zapisuje listę z kluczem do pliku.
#' 
#' @param key_list lista z kluczem.
#' @param file character z nazwą pliku docelowego.
#' 
#' @export
#' 
writeKey = function(key_list, file) {
    for(it in names(key_list)) {
      key_list[[it]] = data.frame(item = it, key_list[[it]],
                                  stringsAsFactors = FALSE)
    }
    merged = plyr::rbind.fill(key_list)
    write.csv2(merged, file, row.names = FALSE)
}