fecondite_stochastique_3 <- function(matrice, n_annees){
  # 1. Identifier le nombre de stades (33)
  n_stades <- nrow(matrice)
  
  # 2. Extraire la ligne de fécondité théorique (la ligne 1)
  f_theo <- matrice[1, ]
  
  # 3. Générer les multiplicateurs stochastiques
  # On crée une matrice de multiplicateurs centrés sur 1 (1 + bruit normal)
  multiplicateurs <- matrix(1 + rnorm(n_stades * n_annees, mean = 0, sd = 0.25), 
                            nrow = n_stades, 
                            ncol = n_annees)
  
  # 4. Appliquer ces multiplicateurs à nos fécondités théoriques
  taux_fec <- multiplicateurs * f_theo
  
  return(taux_fec)
} 

fec <- fecondite_stochastique_3(measured4,n_annees)
fec
measured3
survie_stochastique_3 <- function(matrice, n_annees) {
  # 1. Extraire la survie réelle (sous-diagonale)
  # Pour une matrice 4x4, p_mesure a une longueur de 3
  p_mesure <- diag(matrice[-1, -ncol(matrice)])
  n_taux <- length(p_mesure)
  
  # 2. Préparer beta (Adaptation de ta logique 5.05, 20, 75)
  # On définit un beta pour chaque transition de survie
  # S1->S2: 5.05 | S2->S3: 20 | S3->S4: 75
  b_params <- c(20, 100)
  
  # Sécurité pour éviter Alpha = Inf si p_mesure vaut 1
  p_mesure <- pmin(p_mesure, 0.9999)
  
  # 3. Calcul de Alpha (Formule de la moyenne de la loi Beta)
  alpha_params <- (p_mesure * b_params) / (1 - p_mesure)
  
  # 4. Génération de la matrice de tirages
  # On génère n_annees pour chacun des 3 taux de survie
  taux_survie <- matrix(rbeta(n = n_taux * n_annees, 
                              shape1 = alpha_params, 
                              shape2 = b_params), 
                        nrow = n_taux, 
                        ncol = n_annees)
  
  return(taux_survie)
}

surv <- survie_stochastique_3(measured4,n_annees)
surv

stase_stochastique_3 <- function(matrice, n_annees) {
  p_mesure <- diag(matrice)
  n_stades <- length(p_mesure)
  
  # On remonte b_params pour plus de stabilité (ex: 80)
  b_params <- c(1, 80, 80) 
  
  p_mesure_safe <- pmax(pmin(p_mesure, 0.9999), 0)
  
  # Calcul de alpha (évite division par zéro)
  alpha_params <- (p_mesure_safe * b_params) / pmax(1 - p_mesure_safe, 1e-6)
  
  # Génération
  taux_stase <- matrix(0, nrow = n_stades, ncol = n_annees)
  
  # On ne boucle que sur les stades où p > 0
  indices_actifs <- which(p_mesure_safe > 0)
  
  for(i in indices_actifs) {
    taux_stase[i, ] <- rbeta(n = n_annees, 
                             shape1 = alpha_params[i], 
                             shape2 = b_params[i])
  }
  
  return(taux_stase)
}

stas <- stase_stochastique_3(measured4,n_annees)
stas



# Supposons que n_annees = 2 d'après tes sorties
liste_final <- list()

for (t in 1:n_annees) {
  # 1. Créer une matrice vide 4x4
  M_t <- matrix(0, nrow = 3, ncol = 3)
  
  # 2. Insérer la FÉCONDITÉ (Ligne 1)
  # Ici, seul le stade 3 est reproducteur d'après tes données
  M_t[1, 3] <- fec[3, t] 
  
  # 3. Insérer la STASE (Diagonale)
  # On remplit les cases [1,1], [2,2], [3,3], [4,4]
  diag(M_t) <- stas[, t]
  
  # 4. Insérer la SURVIE de passage (Sous-diagonale)
  M_t[2, 1] <- surv[1, t] # Stade 1 -> 2
  M_t[3, 2] <- surv[2, t] # Stade 2 -> 3
 
  
  # 5. SÉCURITÉ : Vérification des colonnes
  # La somme de la survie (stase + passage) ne doit pas dépasser 1
  for (j in 1:3) {
    survie_totale <- sum(M_t[2:3, j]) # On ignore la ligne 1 (fécondité)
    if (survie_totale > 1) {
      M_t[2:3, j] <- M_t[2:3, j] / (survie_totale + 0.0001)
    }
  }
  
  liste_final[[t]] <- M_t
}

# Vérification de la matrice de l'année 1
print(liste_final[[2]])



# On suppose que 'liste_final' contient tes matrices 4x4 assemblées
lambdas <- numeric(length(liste_final))

for (i in 1:length(liste_final)) {
  # Calcul des valeurs propres (eigenvalues)
  ev <- eigen(liste_final[[i]])$values
  
  # Lambda est la partie réelle de la plus grande valeur propre
  # On utilise Re() car eigen peut retourner des nombres complexes
  lambdas[i] <- max(Re(ev))
}

# Afficher les résultats
print(lambdas)

# Calculer la moyenne géométrique de lambda (croissance à long terme)
lambda_moyen <- exp(mean(log(lambdas)))
cat("Le taux de croissance moyen est de :", lambda_moyen)
