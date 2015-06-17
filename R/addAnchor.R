#' Dodaje kolumny grupujące wartości dla zadań kotwiczących.
#' 
#' @description
#' Dodaje do zbioru danych kolumny, w których gromadzi wartości dla zadań
#' kotwiczących zdefiniowanych w \code{anchor_mat}.
#' 
#' @param df data.frame, do którego dodane mają być kolumny.
#' @param anchor_mat macierz wskazująca odpowiadające sobie zadania w
#'  poszczególnych wersjach.
#' @param test_ver character z nazwą kolumny w \code{df}, w której znajduje się
#'  informacja o rozwiązywanej wersji testu. Alternatywnie wektor z taką
#'  informacją (o długości równej liczbie wierszy w \code{df}). Domyślnie
#'  "wersja".
#'  
#' @export
#'  
addAnchor = function(df, anchor_mat, test_ver = "wersja") {
  # wersje testu
  if (length(test_ver) == 1) {
    if (test_ver %in% names(df)) {
      test_ver = df[, test_ver]
      vers = sort(unique(test_ver))
    } else {
      stop("Brak kolumny ", test_ver, "w zbiorze danych (df)")
    }
  } else if (length(test_ver) == nrow(df)) {
      vers = sort(unique(test_ver))
  } else if (length(test_ver) != nrow(df)) {
      stop("Wektor z określeniem wersji (test_var) ma inną długość niż ",
           "liczba wierszy w zbiorze danych (odp. ", length(test_ver),
           " i ", nrow(df), ")")
  }

  # liczba wersji w macierzy
  n_ver = ncol(anchor_mat)
  if (n_ver != length(vers)) {
    stop("W macierzy anchor_mat określono inną liczbę wersji testu niż", 
         "występuje w bazie df (odp. ", n_ver, " i ", length(vers), ")")
  }
  
  stud_ind = lapply(seq_len(n_ver), function(ver) which(test_ver == vers[ver]))
  # pętla po wierszach, czyli koljenych parach zadań
  for(i in 1:nrow(anchor_mat)) {
    tasks = anchor_mat[i, ]
    kit = paste0("K", tasks[1])

    # wyciągnięcie odpowiednich wartości
    for(ver in seq_len(n_ver)) {
      df[stud_ind[[ver]], kit] = df[stud_ind[[ver]], tasks[ver]]
    }
  }

  df
}
