# Pour avoir le nombre de stades
source("Script/Matrices.R")

calculer_structure_initiale <- function(matrice_choisie, n_mature_cible) {
  n_stades <- nrow(matrice_choisie)
  p_mesure <- diag(matrice_choisie[-1, -ncol(matrice_choisie)])
  
  pop <- numeric(n_stades)
  
  # Partir de 1 individu au stade le plus vieux (stade 33)
  # et remonter en divisant par la survie
  pop[33] <- 1
  for (s in 32:4) {
    pop[s] <- pop[s + 1] / p_mesure[s]
  }
  
  # Stades jeunes (1-3) proportionnels
  pop[3] <- pop[4] / p_mesure[3]
  pop[2] <- pop[3] / p_mesure[2]
  pop[1] <- pop[2] / p_mesure[1]
  
  # Normaliser pour avoir n_mature_cible matures (stades 13-33)
  facteur <- n_mature_cible / sum(pop[13:33])
  
  return(pop * facteur)
}