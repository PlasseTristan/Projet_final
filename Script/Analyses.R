options(scipen = 10)


########################
stockage_present<-c(0,231185,14868)
stockage_propose<-c(0,500000,15000)

### Étape 2.3
### Creation de la boucle 


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

# Scénario Mesuré
pop_ini_measured_500 <- calculer_structure_initiale(measured, 500)

# Scénario Endangered
# pop_endangered <- calculer_structure_initiale(endangered, 500)

# Scénario Null
# pop_null <- calculer_structure_initiale(null_prev, 500)


### Générateur de stochasité sur la fécondité
taux_fecondite <- (1 + rnorm(n_stades, mean = 0, sd = 0.25))*measured[1,]

# Vérification
print(taux_fecondite)

# 1. Définir tes paramètres Beta (le "poids" de la certitude)
# On crée un vecteur de 32 valeurs (pour les 32 transitions de survie)
b_params <- c(5.05, 20, 75, rep(0.75, 29)) 

# 2. Extraire les taux de survie réels de la matrice (sous-diagonale)
# On récupère les valeurs où row = col + 1
p_mesure <- diag(measured[-1, -ncol(measured)])

# 3. Calculer Alpha pour que la moyenne de la Beta soit égale à p_mesure
alpha_params <- (p_mesure * b_params) / (1 - p_mesure)

# 4. Tirer les taux de survie
taux_survie <- rbeta(n = length(alpha_params), 
                               shape1 = alpha_params, 
                               shape2 = b_params)
print(taux_survie)
print(p_mesure)
mean(taux_survie-p_mesure)




### Runner les modèles 100 ans ###

n_annees <- 100
# Créer une structure pour stocker les résultats (33 stades x 101 colonnes)
simulation <- matrix(0, nrow = n_stades, ncol = n_annees + 1)
rownames(simulation) <- rownames(measured)
colnames(simulation) <- paste0("An_", 0:n_annees)

# Année 0
simulation[, 1]<-calculer_structure_initiale(measured,500)
# Boucle de projection sur 100 ans
for (t in 1:n_annees) {
  # N(t+1) = Matrice %*% N(t)
  simulation[, t+1] <- measured %*% simulation[, t]
}

simulation[,101]


simulation

#Nombre d'individus matures (10 ans ou plus) après 100 ans (avec stock)
#pas trop clair mais gap de 10 ans avant stock (maturité à 10ans)
pop_final <- 5000




#