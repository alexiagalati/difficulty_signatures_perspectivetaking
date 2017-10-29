print_stats = function(lmo) {
  coefs = data.frame(summary(lmo)$coefficient)
  coefs$p = 2*(1-pnorm(abs(coefs$t.value)))
  return(coefs)
}