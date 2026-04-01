fecondite_stochastique <- function(matrice, n_annees){
  # 1. Identifier le nombre de stades (33 dans ton cas)
  n_stades <- nrow(matrice)
  
  # 2. Extraire la ligne de fécondité moyenne (la ligne 1)
  f_moyenne <- matrice[1, ]
  
  # 3. Générer les multiplicateurs stochastiques
  # On crée une matrice de multiplicateurs centrés sur 1
  # (1 + bruit normal)
  multiplicateurs <- matrix(1 + rnorm(n_stades * n_annees, mean = 0, sd = 0.25), 
                            nrow = n_stades, 
                            ncol = n_annees)
  
  # 4. Appliquer ces multiplicateurs à nos fécondités moyennes
  # R va multiplier chaque colonne de 'multiplicateurs' par le vecteur 'f_moyenne'
  taux_fec <- multiplicateurs * f_moyenne
  
  # 5. Sécurité : La fécondité ne peut pas être négative
  taux_fec[taux_fec < 0] <- 0
  
  return(taux_fec)
} 


# 1. Préparer b_params pour EXACTEMENT 32 transitions
# (3 spécifiques + 29 répétitions de 0.75 = 32)
b_params <- c(5.05, 20, 75, rep(0.75, 29))

# 2. La fonction corrigée (version sécurisée)
survie_stochastique <- function(matrice, n_annees, b_vec) {
  
  # Extraire la survie réelle (sous-diagonale)
  # Pour tes matrices 33x33, p_mesure aura une longueur de 32
  p_mesure <- diag(matrice[-1, -ncol(matrice)])
  
  # Vérification de sécurité pour éviter les messages d'avis
  if(length(p_mesure) != length(b_vec)) {
    stop(paste("Erreur de dimension ! p_mesure a", length(p_mesure), 
               "éléments mais b_vec en a", length(b_vec)))
  }
  
  # Calcul de Alpha
  alpha_params <- (p_mesure * b_vec) / (1 - p_mesure)
  
  # Génération de la matrice de tirages
  # nrow = 32 (transitions), ncol = n_annees
  taux_survie <- matrix(rbeta(n = length(p_mesure) * n_annees, 
                              shape1 = alpha_params, 
                              shape2 = b_vec), 
                        nrow = length(p_mesure), 
                        ncol = n_annees)
  
  return(taux_survie)
}