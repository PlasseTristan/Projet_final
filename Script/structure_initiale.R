# Pour avoir le nombre de stades
source("Script/Matrices.R")


# Créer un vecteur vide (rempli de 0)
nombre_ind <- vector("numeric", length =  n_stades)
names(nombre_ind)<-noms_stades

calculer_structure_initiale <- function(matrice_choisie, n_age1) {
  
  # 1. Préparation du vecteur
  n_stades <- nrow(matrice_choisie)
  noms_stades <- rownames(matrice_choisie)
  pop <- vector("numeric", length = n_stades)
  names(pop) <- noms_stades
  
  # 2. Fixer la valeur de référence
  pop["Age1"] <- n_age1
  
  # 3. REMONTER le temps (Rétro-calcul pour Egg, Larva, Fry)
  # On divise par le taux de survie pour retrouver l'effectif précédent
  # Indice 4 = Age 1, 3 = Fry, 2 = Larva, 1 = Egg
  pop["Fry"]   <- pop["Age1"] / matrice_choisie[4, 3]
  pop["Larva"] <- pop["Fry"]   / matrice_choisie[3, 2]
  pop["Egg"]   <- pop["Larva"] / matrice_choisie[2, 1]
  
  # 4. AVANCER dans le temps (Pour tous les âges de 2 à 30)
  # On multiplie par le taux de survie de la sous-diagonale
  for (i in 4:(n_stades - 1)) {
    # i correspond à l'indice de la colonne (ex: Age 1 est indice 4)
    # i + 1 correspond à l'indice de la ligne suivante (ex: Age 2 est indice 5)
    pop[i + 1] <- pop[i] * matrice_choisie[i + 1, i]
  }
  
  return(pop)
}