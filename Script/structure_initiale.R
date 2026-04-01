# Pour avoir le nombre de stades
source("Script/Matrices.R")

pop <- vector("numeric", length = n_stades)
calculer_structure_initiale <- function(matrice_choisie, n_mature_cible) {
  n_stades <- nrow(matrice_choisie)
  
  # 1. Calculer les probabilités de survie cumulées à partir de l'œuf (indice 1)
  # survie_cumul[i] est la probabilité qu'un oeuf atteigne le stade i
  survie_cumul <- vector("numeric", length = n_stades)
  survie_cumul[1] <- 1  # Pour le stade Oeuf
  
  for (i in 1:(n_stades - 1)) {
    survie_cumul[i + 1] <- survie_cumul[i] * matrice_choisie[i + 1, i]
  }
  
  # 2. Calculer la proportion de la population qui finit "mature" (stades 13 à 33)
  # (On additionne les probabilités de survie de ces stades)
  prop_mature <- sum(survie_cumul[13:33])
  
  # 3. Trouver le nombre d'oeufs nécessaire
  # N_oeufs = N_mature_voulu / proportion_mature
  n_oeufs_necessaire <- n_mature_cible / prop_mature
  
  # 4. Générer la population complète
  pop <- survie_cumul * n_oeufs_necessaire
  
  return(pop)
}
