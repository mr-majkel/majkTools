#' Zamienia brak odpowiedzi na not-reached.
#' 
#' @param dataframe data.frame z zakodowanym testem. W kolumnach z zadaniami nie
#'  powinno być braków danych.
#' @param tasks character z określeniem nazw kolumn z zadaniami. Może 
#'  stanowić także wyrażenie regularne. Jeżeli nieokreślone (domyślnie),
#'  wszystkie kolumny traktowane są jako zadania.
#' @param missing_code kod brakującej odpowiedzi. Domyślnie 9 (integer).
#' @param replace_with kod, który ma być wprowadzony, gdy odpowiedź zostanie
#'  określona jako not-reached. Domyślnie "N".
#'  
showNotReached = function(dataframe, tasks = NULL, missing_code = 9,
                          replace_with = "N") {
  # tworzy kopię zbioru danych
  dataframe2 = dataframe
  # wybranie zadań
  if(is.null(tasks)) {
    tasks = names(dataframe)
  } else if(length(tasks) == 1) {
    tasks = grep(tasks, names(dataframe), value = TRUE)
  }
  df_tasks = dataframe[, tasks]
  n_tasks = length(tasks)
  
  n_miss = apply(df_tasks, 1, function(row) {
    i = 0
    while (i <= n_tasks) {
      if (any(row[(n_tasks - i):n_tasks] == missing_code)) {
        i = i + 1
        next
      } else {
        break
      }
      i
    }
  })
  for (nn in unique(n_miss)) {
    if (nn <= 1) {
      next
    }
    df_tasks[n_miss == nn, tasks[(n_tasks - nn + 1):n_tasks]] = replace_with 
  }
  dataframe2[, tasks] = df_tasks
  dataframe2
}

