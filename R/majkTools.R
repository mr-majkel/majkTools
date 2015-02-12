#' Wyniki 35 uczniów otrzymane z ConQuesta 2 (jednowymiarowe).
#'
#' Wyniki wygenerowane przez ConQuest 2 za pomocą polecenia \code{show cases} i
#' wczytane za pomocą funkcji \code{\link[base]{readLines}}. Wyniki zawierają
#' pięć PV, oszacowanie EAP i odchylenie standardowe dla rozkładu a posteriori
#' dla 35 uczniów. Przy wyliczaniu wyników założono jednowymiarowy model
#' pomiarowy.
#'
#' @format Character zawierający 35*8 linii z wynikami.
#'
"scores_uni"

#' Wyniki 35 uczniów otrzymane z ConQuesta 2 (wielowymiarowe).
#'
#' Wyniki wygenerowane przez ConQuest 2 za pomocą polecenia \code{show cases} i
#' wczytane za pomocą funkcji \code{\link[base]{readLines}}. Wyniki zawierają
#' pięć PV, oszacowanie EAP i odchylenie standardowe dla rozkładu a posteriori
#' dla 35 uczniów. Przy wyliczaniu wyników założono wielowymiarowy model
#' pomiarowy.
#'
#' @format Character zawierający 35*8 linii z wynikami.
#'
"scores_multi"

#' Dane kontekstowe dla 35 uczniów w formacie .dat (ConQuest2).
#'
#' Dane kontekstowe dla 35 uczniów, zawierające różne fikcyjne identyfikatory,
#' oryginalnie zapisane w formacie pliku danych ConQuesta 2 (plik tekstowy,
#' bez separatorów pól, jeden wiersz = jeden uczeń, kolumny o róznej szerokości)
#' i wczytane za pomocą funkcji \code{\link[base]{readLines}}.
#'
#' @format Character zawierający 35 wierszy z kolumnami:
#' \describe{
#'   \item{id_szk}{znaki od 1 do 4}
#'   \item{id_kl}{znaki od 5 do 5}
#'   \item{id_ucz}{znaki od 6 do 9}
#'   \item{plec}{znaki od 10 do 10}
#' }
#'
"context_dat"
