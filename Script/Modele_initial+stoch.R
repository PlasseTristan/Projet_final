### 1. Préparation des paramètres stochastiques ###
n_annees <- 100
n_stades <- nrow(null_pred)
n_larves <-stockage_present[1]
n_fry <- stockage_present[2]
mat_fec_stoch <- fecondite_stochastique(null_pred, n_annees) # Matrice 33 x 100
mat_surv_stoch <- survie_stochastique(null_pred, n_annees) # Matrice 32 x 100

### 2. Initialisation de la simulation ###
simulation_stoch <- matrix(0, nrow = n_stades, ncol = n_annees + 1)
rownames(simulation_stoch) <- rownames(null_pred)

# On utilise toujours ta fonction de structure initiale
pop_init <- calculer_structure_initiale(null_pred, 500)
pop_init[1:3] <- 0  # On vide les stades infra-annuels
simulation_stoch[, 1] <- pop_init

### 3. Boucle de projection stochastique ###
for (t in 1:n_annees) {
  
  # A. Créer la matrice temporaire pour l'année T
  M_t <- matrix(0, nrow = n_stades, ncol = n_stades)
  
  # B. Insérer les survies stochastiques de l'année t (sur la sous-diagonale)
  # mat_surv_stoch[, t] contient les 32 survies tirées pour cette année
  diag(M_t[-1, -n_stades]) <- mat_surv_stoch[, t]
  
  # C. Appliquer la "Logique Chevalier Cuivré" (Condensation 1 an)
  # On calcule la survie combinée des stades 1, 2 et 3 de CETTE année
  # s_an1_t = s(1->2) * s(2->3) * s(3->4)
  s_an1_t <- mat_surv_stoch[1, t] * mat_surv_stoch[2, t] * mat_surv_stoch[3, t]
  
  # D. Insérer le recrutement direct vers le Stade 4 (Age 1)
  # On prend la fécondité stochastique de l'année t (ligne 1 de null_pred avec bruit)
  # Rappel : mat_fec_stoch[, t] contient les fécondités des stades 1 à 33
  M_t[4, 13:33] <- mat_fec_stoch[13:33, t] * s_an1_t
  
  # E. Nettoyage : On s'assure que les transitions 1->2, 2->3, 3->4 
  # ne sont pas comptées deux fois (elles sont déjà dans s_an1_t)
  M_t[2,1] <- 0
  M_t[3,2] <- 0
  M_t[4,3] <- 0
  
  # F. Projection (votre code actuel)
  n_t_plus_1 <- M_t %*% simulation_stoch[, t]
  
  # G. Ajout de l'ensemencement (Stockage)
  # n_larves et n_fry sont vos valeurs de stockage (ex: 231185 et 14868)
  # On leur applique la survie restante pour qu'ils rejoignent le Stade 4 (Age 1)
  apport_stade4 <- (n_larves * mat_surv_stoch[2, t] * mat_surv_stoch[3, t]) + 
    (n_fry * mat_surv_stoch[3, t])
  
  # On injecte ces "nouveaux" individus de l'année dans le stade 4
  n_t_plus_1[4] <- n_t_plus_1[4] + apport_stade4
  
  # H. Sauvegarder
  simulation_stoch[, t+1] <- n_t_plus_1
}

### 4. Analyse des résultats ###
total_final <- sum(simulation_stoch[, n_annees + 1])
cat("Population finale après 100 ans (stochastique) :", round(total_final), "\n")


# Tracer l'évolution de la population totale sur 100 ans
# Supposons que 'res' est le résultat de votre fonction simule_stochastique
df_plot <- data.frame(
  Annee = 0:100,
  Abondance = colSums(simulation_stoch)
)

# Sécurité : ggplot n'aime pas le log(0). 
# On remplace les 0 par une valeur infime si la population s'éteint.
df_plot$Abondance[df_plot$Abondance <= 0] <- 0.1

ggplot(df_plot, aes(x = Annee, y = Abondance)) +
  # Trajectoire de la population
  geom_line(color = "#2c3e50", linewidth = 1) +
  
  # Échelle logarithmique (base 10)
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000), 
                # On force les limites entre 0.1 (notre "0") et 10000
                limits = c(0.1, 100000),
                labels = c("1", "10", "100", "1 000", "10 000")) +
  
  # Thème et esthétique
  theme_minimal() +
  labs(
    title = "Projection stochastique de la population (Âges 1-30)",
    x = "Années",
    y = "Nombre d'individus (log10)"
  ) +
  
  # Amélioration visuelle des axes
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )


