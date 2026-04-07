# Pour avoir le nombre de stades
source("Script/Matrices.R")

calculer_structure_initiale <- function(matrice_choisie, n_mature_cible) {
  n_stades <- nrow(matrice_choisie)
  survie_cumul <- vector("numeric", length = n_stades)
  
  # --- LOGIQUE ANNEE 1 (Stades intra-annuels) ---
  
  # Le stade 1 (Oeuf) est le point de départ
  survie_cumul[1] <- 1 
  
  # Le stade 2 (Early Life) : survie des oeufs (s1)
  survie_cumul[2] <- matrice_choisie[2,1]
  
  # Le stade 3 (Fry) : survie cumulée oeufs * early life (s1 * s2)
  survie_cumul[3] <- matrice_choisie[2,1] * matrice_choisie[3,2]
  
  # --- LOGIQUE ANNEE 2 ET PLUS ---
  
  # Le passage du stade 3 au stade 4 représente la fin de la 1ère année.
  # On continue ensuite la chaîne de survie normalement pour les âges suivants.
  for (i in 3:(n_stades - 1)) {
    survie_cumul[i + 1] <- survie_cumul[i] * matrice_choisie[i + 1, i]
  }
  
  # --- CALCUL DES EFFECTIFS ---
  
  # On identifie les matures (13 à 33)
  prop_mature <- sum(survie_cumul[13:33])
  
  # Combien d'oeufs faut-il pondre pour avoir 'n_mature_cible' à l'An 1 ?
  # Note : Si on veut 500 matures à l'An 1, il faut diviser par le lambda 
  # ou s'assurer que la projection de t0 à t1 donne le bon chiffre.
  n_oeufs_initial <- n_mature_cible / prop_mature
  
  # Population finale
  pop <- survie_cumul * n_oeufs_initial
  
  return(pop)
}
