#' Przekształca wyniki z ConQuesta 2
#' 
#' Przekształca wyniki z ConQuesta 2 otrzymane w wyniku działania polecenia
#' \code{show cases} do postaci data.frame i umożliwia ich zapis na dysku.
#' Dodatkowo umożliwia dołączenie kolumn (np. identyfikatorów) z pliku .dat,
#' z którego wyliczone zostały wyniki w ConQueście.
#' 
#' @param scores Character ze ścieżką do outputu z ConQuesta2 dla
#'  polecenia show.cases. Alternatywnie character z wynikiem działania funkcji
#'  \code{\link[base]{readLines}} dla tego pliku.
#' @param write_to Character z nazwą (ścieżką) pliku wynikowego w formacie csv.
#'  Jeżeli NULL, zwraca data.frame z wynikiem.
#' @param pv Integer okreslający liczbę wygenerowanych pv. Potrzebny do
#'  określenia długości tablicy z wynikami dla jednego ucznia w \code{scores}.
#' @param dat_file Character ze ścieżką do pliku z danymi, z których ConQuest
#'  wygenerował output (opcjonalny). Alternatywnie character z wynikiem działania funkcji
#'  \code{\link[base]{readLines}} dla tego pliku.
#' @param ... Definicja pozycji kolumn w pliku dat_file, które mają być
#'  dodane do wyników (patrz sekcja Details).
#'  
#' @return
#' Zbiór danych w postaci tabelarycznej z oszacowaniami (EAP, SD i PV) w
#' kolumnach. Uporządkowanie uczniów jest identyczne z tym w pliku
#' \code{scores}. Jeżeli podana została ścieżka zapisu (\code{write_to}), zbiór
#' jest zapisany do pliku. Domyślnie zwraca data.frame.
#' 
#' Jeżeli podano ścieżkę do pliku .dat (\code{dat_file}) i zdefiniowano pozycje
#' kolumn dodatkowych z tego pliku, zbiór danych jest powiększony o te
#' kolumny (patrz sekcja Details).
#' 
#' W przypadku, gdy wyniki są wyliczone z modelu wielowymiarowego, kolumny z 
#' oszacowaniami dla kolejnych \code{N} wymiarów mają przedrostek \code{DimN_}.
#' 
#' @details
#' Funkcja wymaga pakietu
#' \href{http://cran.r-project.org/web/packages/plyr/index.html}{plyr}
#' do działania.
#' 
#' Definicja pozycji kolumn powinna przyjmować następującą strukturę:
#'  \code{varName = c(n, k)}. \code{varName} określa nazwę pod jaką kolumna
#'  zostanie dodana do zbioru. \code{n, k} to liczby określające, odpowiednio,
#'  numer startowego i końcowego indeksu w wierszu w pliku tekstowym
#'  \code{dat_file}. Jeżeli kolumna ma szerokość równą jeden, należy dwukrotnie
#'  podać pozycję tej kolumny (np. \code{id_ucz = c(2, 2)}). Kolejne definicje
#'  kolumn należy dodawać po przecinku.
#'   
#' Zdefiniowane kolumny zostaną dodane na początku wynikowego zbioru danych w
#' kolejności w jakiej zostały zdefiniowane.
#' 
#' @encoding UTF-8
#' @export
convertScores = function(scores = "show.cases", write_to = NULL, pv = 5,
                  dat_file = NULL, ...) {
  
  if (length(scores) == 1) {
    scores = readLines(scores)
  }
  n = length(scores) / (pv + 3)
  prog = n %/% 100
  cat("Znaleziono ", n, " case'ów w pliku ", quote(scores), ".\n", sep="")
  cat("Przetwarzam...\n")
  cat("|")
  cat(paste(rep("*", max(1,prog)), collapse = ""))
  cat("|\n|")
  flush.console()
  dims = 1
  
  read_scores = function(lines) {
    pvs = lines[1:pv]
    eap = lines[(pv + 1)]
    sd = lines[(pv + 2)]
    
    pvs = lapply(strsplit(pvs, " "), as.numeric)
    names(pvs) = paste0("PV_", 1:pv)
    pvs = lapply(pvs, function(x) {x[!is.na(x)]})
    pvs = data.frame(pvs)[-1, ]
    
    if(nrow(pvs) > 1) {
      if (dims == 1) {
        dims <<- nrow(pvs)
      }
      pvs2 = list()
      for(dim in 1:dims) {
        pvs2[[dim]] = pvs[dim, ]
        names(pvs2[[dim]]) = paste0(paste0("Dim", dim, "_"), names(pvs))
      }
      pvs = Reduce(cbind, pvs2)
    }
    
    eap = unlist(lapply(strsplit(eap, " "), as.numeric))
    eap = eap[complete.cases(eap)]
    eap = data.frame(as.list(eap))
    
    sd = unlist(lapply(strsplit(sd, " "), as.numeric))
    sd = sd[complete.cases(sd)]
    sd = data.frame(as.list(sd))

    if(dims > 1) {
      names(eap) = paste0("Dim", 1:dims, "_EAP")
      names(sd) = paste0("Dim", 1:dims, "_SD")    
    } else {
      names(eap) = "EAP"
      names(sd) = "SD"
    }
    scores_df = cbind(pvs, eap, sd)
    scores_df
  }
  
  lines = list()
  step = pv + 3
  range = c(2, step)
  for(i in 1:n) {
    range_i = range + (step * (i - 1))
    lines[[i]] = scores[range_i[1]:range_i[2]]

  }
  
  # sczytaj wyniki
  dta_list = lapply(seq_along(1:n), function(i) {
    # postęp
    if ((i %% 100 == 0) & (i %/% 100 != prog)) {
      cat("-")
      flush.console()
    } else if (i == n) {
      cat("-|\n")
      flush.console()
    }
    read_scores(lines[[i]])
    })
  
  # scal wyniki
  dta_frame = plyr::rbind.fill(dta_list)
  
  # czy dokleić kolumny
  if(!is.null(dat_file)) {
    cat("Doklejam kolumny...\n")
    flush.console()
    wyniki = names(dta_frame)
    
    if (length(dat_file) == 1) {
      dat_file <- readLines(dat_file, n)
    }
    
    cols <- as.list(substitute(list(...))[-1])
    cols <- cols[names(cols) != ""]
    
    if(length(names(cols)) == 0) {
      warning("Nie zdefiniowano żadnych kolumn do dołączenia")
    }
    
    for(col in names(cols)) {
      
      a = eval(cols[[col]])
      stopifnot(is.numeric(a), length(a) == 2)
      dta_frame[, col] = substr(dat_file, a[1], a[2])      
    }
    
    dta_frame = dta_frame[, c(names(cols), wyniki)]
  }
  
  cat("Wykryto ", dims, " wymiary/ów.\n", sep="")
  
  # czy zapisać plik?
  if(!is.null(write_to)) {
    write.csv2(dta_frame, write_to, row.names = FALSE)  
    cat("Wyniki zapisano w pliku ", write_to, ".\n", sep = "")
  } else {
    dta_frame
  }
  
}
