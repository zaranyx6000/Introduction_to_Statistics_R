# -------------------------------------------------------------
# Funktion: conf_interval
# Zweck:    Berechnet ein (1 - alpha)-Konfidenzintervall
#           für den Mittelwert einer numerischen Stichprobe.
# -------------------------------------------------------------

conf_interval <- function(x, conf.level = 0.95, use_t = TRUE) {
  
  # Überprüfen, ob Eingabe numerisch ist
  if (!is.numeric(x)) {
    stop("x muss ein numerischer Vektor sein.")
  }
  
  # Grundlegende Kennzahlen
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  se <- sd_x / sqrt(n)  # Standardfehler
  
  # Alpha und Freiheitsgrade
  alpha <- 1 - conf.level
  df <- n - 1
  
  # Kritischer Wert
  if (use_t) {
    crit <- qt(1 - alpha/2, df = df)  # t-Verteilung
  } else {
    crit <- qnorm(1 - alpha/2)        # Normalverteilung
  }
  
  # Konfidenzintervall berechnen
  lower <- mean_x - crit * se
  upper <- mean_x + crit * se
  
  # Ergebnis als Liste zurückgeben
  result <- list(
    mean = mean_x,
    lower = lower,
    upper = upper,
    conf.level = conf.level,
    method = ifelse(use_t, "t-Verteilung", "Normalverteilung"),
    n = n
  )
  
  # Schöne Ausgabe
  cat(sprintf("\nKonfidenzintervall (%.1f%%) basierend auf %s\n", 
              conf.level * 100, result$method))
  cat(sprintf("Stichprobengrösse (n): %d\n", n))
  cat(sprintf("Mittelwert: %.3f\n", mean_x))
  cat(sprintf("Intervall: [%.3f , %.3f]\n\n", lower, upper))
  
  return(invisible(result))
}