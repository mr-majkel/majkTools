#' Rysuje ICC dla podanych parametrów.
#' 
#' @description Rysuje wykresy z krzywą charaktersytyczną dla zadania (ICC), dla
#'  podanych parametrów zadania.
#' 
#' @param b numeric, trudnoścć zadania, domyślnie 0.
#' @param a numeric, dyskryminacja zadania, domyślnie 1.
#' @param c numeric, dolna asymptota (parametr pseudo-zgadywania),
#'  w zakresie \code{c(0, 1)}, domyślnie 0.
#' @param d numeric, górna asymptota (parametr pseudo-nieuważności),
#'  w zakresie \code{c(0, 1)}, domyślnie 1.
#' @param const numeric z określeniem stałej zbliżającej skalę do rozkładu
#'  normalnego.
#' @param theta dwuelementowy numeric z określeniem przedziału skali, dla którego
#'  ma być wyliczona wartość funkcji. Domyślnie \code{c(-3, 3)}.
#' @param logit logical, czy wartość funkcji ma być przedstawiona na skali logitowej?
#'  Domyślnie \code{FALSE} (tj. skala prawdopodobieństwa).
#' @param ... parametry przekazane do funkcji \code{\link[graphics]{plot}}.
#' 
#' @export
#' 
plotICC = function(b = 0, a = 1, c = 0, d = 1, const = 1.7, theta = c(-3, 3),
                   logit = FALSE, ...) {
  
  # dziedzina funkcji
  stopifnot(length(theta) == 2, min(theta) != max(theta))
  theta = seq(min(theta), max(theta), 0.01)
  
  # wartość funkcji
  prob = getProbIRT(theta = theta, b = b, a = a, c = c, d = d, const = const,
                    logit = logit)
  
  # wykres
  plot(theta, prob, type = "l", lty = 1,
       lwd = 2, ylim = c(0, 1),
       xlab = "theta", ylab = "P(x = 1)", ...)
}


#' Zwraca prawdopodobieństwo poprawnej odpowiedzi na zadanie.
#' 
#' @description Zwraca prawdopodobieństwo poprawnej odpowiedzi na zadanie przy
#'  określonych parametrach zadania.
#'  
#' @param theta numeric, punkty na skali, dla których
#'  ma być wyliczona wartość funkcji.
#' @param b numeric, trudnoścć zadania, domyślnie 0.
#' @param a numeric, dyskryminacja zadania, domyślnie 1.
#' @param c numeric, dolna asymptota (parametr pseudo-zgadywania),
#'  w zakresie \code{c(0, 1)}, domyślnie 0.
#' @param d numeric, górna asymptota (parametr pseudo-nieuważności),
#'  w zakresie \code{c(0, 1)}, domyślnie 1.
#' @param const numeric z określeniem stałej zbliżającej skalę do rozkładu
#'  normalnego.
#' @param logit logical, czy wartość funkcji ma być przedstawiona na skali logitowej?
#'  Domyślnie \code{FALSE} (tj. skala prawdopodobieństwa).
#'
#' @return Numeric, o tej samej długości co \code{theta}, którego elementy
#'  odpowiadają prawdopodobieństwu poprawnej odpowiedzi na zadanie przy
#'  określonych parametrach. Jeżeli \code{logit = TRUE}, wartość jest wyrażona
#'  w logitach (tj. odpowiada logarytmowi naturalnemu ilorazu szans).
#' 
#' @export
#' 
getProbIRT = function(theta, b = 0, a = 1, c = 0, d = 1, const = 1.7,
                      logit = FALSE) {
  prob = c + (d - c)/(1 + exp(-a * (theta - b)))
  
  if (logit) {
    logodds = log(prob/(1-prob))
    return(logodds)
  }
  return(prob)
}