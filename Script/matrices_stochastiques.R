fecondite_stochastique <- function(matrice, n_annees){
  n_stades <- nrow(matrice)
  f_moyenne <- matrice[1, ]
  
  multiplicateurs <- matrix(1 + rnorm(n_stades * n_annees, mean = 0, sd = 0.25), 
                            nrow = n_stades, 
                            ncol = n_annees)
  
  taux_fec <- multiplicateurs * f_moyenne
  taux_fec[taux_fec < 0] <- 0
  return(taux_fec)
}

survie_stochastique <- function(matrice, n_annees) {
  b_params <- c(5.05, 20, 75, rep(0.75, 29))
  p_mesure <- diag(matrice[-1, -ncol(matrice)])
  alpha_params <- (p_mesure * b_params) / (1 - p_mesure) 
  
  n_stades <- length(p_mesure)
  taux_survie <- matrix(NA, nrow = n_stades, ncol = n_annees)
  
  for (s in 1:n_stades) {
    taux_survie[s, ] <- rbeta(n = n_annees,
                              shape1 = alpha_params[s],
                              shape2 = b_params[s])
  }
  return(taux_survie)
}