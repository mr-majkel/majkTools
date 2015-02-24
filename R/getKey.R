#' Generuje z codebooka klucz wartości potrzebnych do rekodowania.
#'  
#' @param cdbook data.frame z codebookiem z weryfikatora. 
#' @param vars string z wyrażeniem regularnym lub wektor tekstowy z nazwami
#'  kolumn z zadaniami.
#' @param default_key wektor tekstowy określający wartości w bazie do zrekodowania,
#'  które mają nie być rekodowane na 0 (zostana pozostawione). Dotyczy tylko
#'  zadań nie określonych jako zamknięte w \code{mcq_val} (patrz Details).
#'  Domyślnie \code{c("1", "2")}.
#' @param mcq_key wektor określający wartości dla zadań zamkniętych, którym po 
#'  zrekodowaniu przypisać wartość 1, a nie 0. Liczba kodów musi
#'  zgadzać się z liczbą wykrytych zadań zamkniętych (patrz Details).
#' @param mcq_val integer określający wartość, która występuje tylko dla zadań
#'  zamkniętych (jednoznacznie je wskazuje). Domyślnie 4.
#' @param na_codes wektor określający wartości w bazie, które mają być
#'  zrekodowane jako NA, a nie 0. Dotyczy wszystkich zmiennych określonych w
#'  \code{vars}.
#' 
#' @return
#' Lista o następujących elementach:
#' items     wektor tekstowy z nazwami zmiennych pasujących do 'vars'.
#' values    lista wartości zdefiniowanych dla 'items'.
#' recodes   lista wartości, na które mają być zrekodowane 'values'.
#' open      wektor tekstowy z nazwami zmiennych określonych jako otwarte
#'           (tj. nie-zamknięte).
#' mcq       wektor tekstowy z nazwami zmiennych określonych jako zamknięte.
#' open_key  wykorzystany 'open_key'.
#' mcq_key   wykorzystany 'mcq_key' (o ile został podany).
#' 
#' @details
#' Ważne, żeby nazwy zmiennych w \code{cdbook} były zdefiniowane w kolumnie
#' 'Nazwa( zmiennej)', a etykiety wartości w kolumnie 'Etykiety( wartości)'.
#' Część nie w nawiasie stanowi zapytanie odnajdujące odpowiednie kolumny.
#' Wyrażenia 'Nazwa' oraz 'Etykiety' muszą więc identyfikować dokładnie po
#' jednej zmiennej (patrz reformat()).
#' 
#' Zakłada się, że wartości są oddzielone od swoich etykiet za pomocą
#' dwukropka ':', bez spacji po obu stronach.
#' 
#' Jeżeli \code{mcq_key} nie jest \code{NULL}, to w trakcie generowania klucza
#' zakłada się, że zmienne, które posiadają etykietę dla wartości określonej w
#' \code{mcq_val}, są zadaniami zamkniętymi.
#' 
#' @export
#' 
getKey = function(cdbook, vars, default_key = c("1", "2"),
                  mcq_key = NULL, mcq_val = 4,
                  na_codes = NULL) {
  # wczytanie codebooka
  cdbook = reformat(cdbook)
  # nazwy itemów
  items = getItems(cdbook, vars)
  # lista wartości
  values = lapply(cdbook[cdbook$var_id %in% items, "values"], getValues)
  # uzupełnienie o wartości "braków danych"
  if(!is.null(na_codes)) {
    values = lapply(values, function(vals) {
      ind = which(!(na_codes %in% vals))
      if(length(ind) > 0) {
        vals = append(vals, na_codes[ind])
      }
      return(vals)
    })
  }
  # zadania otwarte
  items_o = items
  # zadania zamknięte
  if (!is.null(mcq_key)) {
    closed = unlist(lapply(values, function(x) mcq_val %in% x))
    items_c = items[closed]
    if (length(mcq_key) != length(items_c)) {
      stop("Liczba zadeklarowanych kodów nie zgadza się z wykrytą liczbą",
           "zadań zamkniętych")
    }
    
    # zadania otwarte
    items_o = items[!closed]
  }
  
  key_list = list()
  for (it in items) {
    item_index = which(items == it)
    it_vals = values[[item_index]]
    n_vals = length(it_vals)
    # test na brak duplikatów w codebooku
    if (length(unique(it_vals)) < n_vals) {
      warning("W codebooku przynajmniej jedna wartość dla zmiennej", it, 
              "ma zdefiniowane kilka etykiet\n.")
    }
    # tworzy wpis klucza
    key_list[[it]] = data.frame(value = it_vals,
                                recode = rep(0, n_vals))
    # tworzenie recodes dla zadań otwartych
    if (it %in% items_o) {
      # określenie zakresu wartości domyślnych do pozostawienia
      def_val = default_key[default_key %in% it_vals]
      # określenie indeksów dla tych wartości
      def_index = match(def_val, it_vals)
      # przypisanie wartościom recodes
      key_list[[it]][def_index, "recode"] = def_val
    # i dla zamkniętych
    } else if (!is.null(mcq_key)){
      # określenie zadania zamkniętego
      mcq_index = which(items_c == it)
      # określenie indeksu wartości poprawnej w tablicy wartości dla zadania
      mcq_correct = match(mcq_key[mcq_index], it_vals)
      key_list[[it]][mcq_correct, "recode"] = 1
    }
    # recodes dla braków danych
    if (!is.null(na_codes)) {
      na_index = match(na_codes, it_vals)
      key_list[[it]][na_index, "recode"] = NA
    }
  }
  key_list
}
