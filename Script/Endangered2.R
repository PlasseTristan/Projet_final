# ==========================================================
# PARAMÈTRES DE BASE ENDANGERED
# ==========================================================
f_rates <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             18523, 21571, 24749, 27847, 30339, 32771, 35037, 36699, 
             38034, 39442, 40865, 42009, 43022, 44098, 45230, 46268, 
             47231, 48144, 49019, 49841,0)

s_rates <- c(
  0.075, 0.023, 0.007, 0.625, 0.741, 0.798, 0.833, 0.857, 0.873, 0.885, 0.898, 0.910, 0.916, # Jusqu'à Age 10
  0.918, 0.921, 0.926, 0.927, 0.929, 0.932, 0.933, 0.933, 0.934, 0.935, 0.935, 0.935, 0.935, # Suite
  0.936, 0.936, 0.936, 0.937, 0.937, 0.937, 0)

# ==========================================================
# FÉCONDITÉ PONDÉRÉE PAR lx DANS CHAQUE GROUPE
# ==========================================================
f1_eff <- mean(f_rates[1:3])
f2_eff <- mean(f_rates[4:12])
f3_eff <- mean(f_rates[13:23])
f4_eff <- mean(f_rates[24:32])
f5_eff <- f_rates[33]

# ==========================================================
# SURVIE ANNUELLE MOYENNE DANS CHAQUE GROUPE
# ==========================================================
s1 <- prod(s_rates[1:3])  #Produit des 3 survies
s2 <- prod(s_rates[4:12])   # Produit des 9 survies
s3 <- prod(s_rates[13:23])  # Produit des 11 survies
s4 <- prod(s_rates[24:32])  # Produit des 9 survies
s5 <- s_rates[33]           # Stade seul


cat("\n\nSurvies moyennes par groupe :")
cat("\n  s1 :", round(s1, 4))
cat("\n  s2 :", round(s2, 4))
cat("\n  s3 :", round(s3, 4))
cat("\n  s4 :", round(s4, 4))
cat("\n  s5 :", round(s5, 4))

# ==========================================================
# DURÉE MOYENNE DANS CHAQUE GROUPE
# ==========================================================
d1 <- 1
d2 <- 9
d3 <- 11
d4 <- 9
d5 <- 1

# ==========================================================
# PROBABILITÉ RESTER / AVANCER
# ==========================================================
p_rester1  <- 0
p_avancer1 <- s1 * (1/d1)

p_rester2  <- mean(s_rates[4:12])
p_avancer2 <- s2 * (1/d2)

p_rester3  <- mean(s_rates[13:23])
p_avancer3 <- s3 * (1/d3)

p_rester4  <- mean(s_rates[24:32])
p_avancer4 <- s4 * (1/d4)

cat("\n\nProbabilités rester / avancer :")
cat("\n  Groupe 1 : rester =", round(p_rester1, 4), "| avancer =", round(p_avancer1, 4))
cat("\n  Groupe 2 : rester =", round(p_rester2, 4), "| avancer =", round(p_avancer2, 4))
cat("\n  Groupe 3 : rester =", round(p_rester3, 4), "| avancer =", round(p_avancer3, 4))
cat("\n  Groupe 4 : rester =", round(p_rester4, 4), "| avancer =", round(p_avancer4, 4))

# ==========================================================
# CONSTRUCTION DE LA MATRICE M5
# ==========================================================
M6 <- matrix(0, nrow = 5, ncol = 5)

# Fécondité (ligne 1)
M6[1, 1] <- f1_eff   # = 0
M6[1, 2] <- f2_eff
M6[1, 3] <- f3_eff
M6[1, 4] <- f4_eff
M6[1, 5] <- f5_eff

# Rétention (diagonale principale)
M6[1, 1] <- p_rester1
M6[2, 2] <- p_rester2
M6[3, 3] <- p_rester3
M6[4, 4] <- p_rester4
M6[5, 5] <- s5

# Avancement (sous-diagonale)
M6[2, 1] <- p_avancer1
M6[3, 2] <- p_avancer2
M6[4, 3] <- p_avancer3
M6[5, 4] <- p_avancer4

# ==========================================================
# VÉRIFICATION
# ==========================================================


cat("\n\nMatrice réaliste 5x5 :\n")
print(round(M6, 6))

cat("\nLambda matrice originale (endangered) :", round(Re(eigen(endangered)$values[1]), 6))
cat("\nLambda matrice M6                   :", round(Re(eigen(M6)$values[1]), 6))

endangered2 <- M6