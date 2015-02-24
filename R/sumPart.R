#' Dzieli pliki z summary dla modeli z TAM
#' 
#' @description Dzieli pliki z summary dla modeli z TAM na części i wyrzuca do
#' konsoli wybrane części.
#' 
#' @param sum_file character określający lokalizację pliku z \code{summary}.
#' @param chunk integer z określeniem, które części zwrócić lub "all"
#'  (domyślnie), żeby zwrócić wszystkie.
#' @param sep character z określeniem separatora. Jeżeli określono kilka
#'  separatorów, dzieli plik po nich wszystkich. Domyślnie "---".
#' 
#' @export
#' 
sumPart = function(sum_file, chunk = "all", sep = "---") {
  # wczytaj kolejne linie z pliku
  x = readLines(con = sum_file)
  
  # podziel plik na części
  if (length(sep) > 1) {
    sep = paste(sep, collapse = "|")
  }
  seps_ind = unique(grep(sep, x))
  if (length(seps_ind) == 0) {
    stop("Plik nie zawiera oczekiwanych separatorów! ",
         "Wskazałeś dobry plik?\n")
  }
  end_line = length(x)
  
  chunk_begin = seps_ind + 1
  chunk_end = c(seps_ind[-1] - 1, end_line)

  chunk_list = lapply(seq_along(chunk_begin), function(i) {
    x[chunk_begin[i]:chunk_end[i]]
  })
  n_chunk = length(chunk_list)

  # sprawdź czy wypluć wszystkie
  if ((length(chunk) == 1) && (chunk == "all")) {
    sel_chunk = 1:n_chunk
    
    # jeśli nie, to czy chunk jest liczbą i choć jedna część wybrana
  } else if (any(chunk %in% 1:n_chunk)) {
    sel_chunk = as.numeric(chunk[which(chunk %in% 1:n_chunk)])
    
    # sprawdź czy żądanie jest w zakresie
    if((max(chunk) > n_chunk) || (min(chunk) <= 0)) {
      warning("Wyświetlam tylko niektóre części. ",
              "Wykryto ", n_chunk, " części w pliku.\n")
    }
  } else {
    stop("Nie wskazano żadnej części! ",
         "Wykryto ", n_chunk, " części w pliku.\n")
  }
  # wypluj pożądane części
  for(i in sel_chunk) {
    cat(chunk_list[[i]], sep = "\n")
    cat("\n")
  }
}

