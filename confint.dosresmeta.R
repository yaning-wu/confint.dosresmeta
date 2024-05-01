confint <- function(object, round_h = 2, round_h2 = 2, round_i2 = 0){
  # returning an error if "object" is not the correct object type
  if (!is.element("dosresmetaObject", class(object)))
      stop("Argument 'x' must be an object of class \"dosresmetaObject\".")
  # defining Q, k, and df
  qtest_obj <- qtest(object)
  q <- qtest_obj[[1]]
  df <- qtest_obj[[2]]
  k <- df + 1
  # H confidence interval
  h <- max(sqrt(q/df), 1)
  se0 <- sqrt((1/(2*(k - 2)))*(1 - (1/(3*(k - 2)^2))))
  se1 <- ((log(q)-log(df))/(sqrt(2*q) - sqrt(2*k - 3)))/2
  lci_h <- case_when(q <= k ~ exp(log(h) - 1.96*se0),
                      q > k ~ exp(log(h) - 1.96*se1))
  uci_h <- case_when(q <= k ~ exp(log(h) + 1.96*se0),
                      q > k ~ exp(log(h) + 1.96*se1))
  # H^2 confidence interval
  h2 <- h^2
  lci_h2 <- lci_h^2
  uci_h2 <- uci_h^2
  # I^2 confidence interval
  i2 <- (h2 - 1)/h2
  lci_i2 <- (lci_h^2 - 1)/lci_h^2
  uci_i2 <- (uci_h^2 - 1)/uci_h^2
  # printing all confidence intervals
  df <- data.frame(statistic = c("H", "H\U00B2", "I\U00B2 (%)"),
                   estimate = c(round(h, 2), round(h2, 2), round(i2*100, 2)),
                   lower_ci = c(round(lci_h, 2), round(lci_h2, 2), round(lci_i2*100, 2)),
                   upper_ci = c(round(uci_h, 2), round(uci_h2, 2), round(uci_i2*100, 2)))
  return(df) 
}
