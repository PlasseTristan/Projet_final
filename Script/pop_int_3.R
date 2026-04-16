calculer_structure_initiale_3 <- function(matrice, n_mature_cible) {
  # 1. Calculer les valeurs et vecteurs propres
  # Le vecteur propre associé à la plus grande valeur propre (lambda) 
  # représente la structure stable de la population.
  ev <- eigen(matrice)
  
  # 2. Extraire le vecteur propre dominant (on prend la partie réelle)
  # C'est la répartition proportionnelle "naturelle" des stades
  ssd <- Re(ev$vectors[, 1])
  
  # 3. S'assurer que les valeurs sont positives
  ssd <- abs(ssd)
  
  # 4. Identifier les stades matures (3 et 4 dans ta matrice)
  # On calcule combien d'adultes il y a "relativement" dans ce vecteur
  n_adultes_relatif <- ssd[3]
  
  # 5. Calculer le facteur de mise à l'échelle
  # On veut transformer ces proportions pour que Adultes = 1000
  facteur <- n_mature_cible / n_adultes_relatif
  
  # 6. Appliquer le facteur à tous les stades
  pop_initiale <- ssd * facteur
  
  # On retourne les valeurs arrondies (on ne peut pas avoir 0.5 poisson)
  return(round(pop_initiale))
}
calculer_structure_initiale_3(measured4,1000)
