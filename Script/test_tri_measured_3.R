# ==========================================================
# 1. CALCUL DES PARAMÈTRES AGRÉGÉS
# ==========================================================

# Fécondités moyennes par bloc
f_vals <- c(
  f_rates[1], 
  f_rates[2], 
  f_rates[3], 
  mean(f_rates[4:12]), 
  mean(f_rates[13:22]), 
  mean(f_rates[23:33])
)

# Survies annuelles moyennes par bloc
s_vals <- c(
  s_rates[1], 
  s_rates[2], 
  s_rates[3], 
  mean(s_rates[4:12]), 
  mean(s_rates[13:22]), 
  mean(s_rates[23:33])
)

# ==========================================================
# 2. CONSTRUCTION DE LA MATRICE (6x6)
# ==========================================================
n_stades <- 6
L6 <- matrix(0, nrow = n_stades, ncol = n_stades)

# --- Ligne 1 : Fécondités ---
L6[1, ] <- f_vals

# --- Transitions Stades individuels 1-2-3 ---
L6[2, 1] <- s_rates[1]
L6[3, 2] <- s_rates[2]
L6[4, 3] <- s_rates[3] # Passage du stade 3 au premier bloc [4-12]

# --- Fonction P et G ---
calc_PG <- function(s_moy, d) {
  P <- ((1 - (s_moy^(d-1))) / (1 - (s_moy^d))) * s_moy
  G <- (s_moy^d * (1 - s_moy)) / (1 - s_moy^d)
  return(list(P = P, G = G))
}

# --- BLOC 4 : Âges 4-12 (durée d = 9 ans) ---
res4 <- calc_PG(s_vals[4], 9)
L6[4, 4] <- res4$P
L6[5, 4] <- res4$G

# --- BLOC 5 : Âges 13-22 (durée d = 10 ans) ---
res5 <- calc_PG(s_vals[5], 10)
L6[5, 5] <- res5$P
L6[6, 5] <- res5$G

# --- BLOC 6 : Âges 23-33 (durée d = 11 ans) ---
# Comme c'est le dernier stade, on ne "gradue" plus, on y reste jusqu'à la mort
res6 <- calc_PG(s_vals[6], 11)
L6[6, 6] <- res6$P 

# ==========================================================
# 3. ANALYSE
# ==========================================================
print("Matrice de Leslie condensée (6x6) :")
print(round(L6, 4))

lambda_L6 <- Re(eigen(L6)$values[1])
lambda_orig <- Re(eigen(measured)$values[1])

cat("\nLambda matrice 6x6 :", round(lambda_L6, 6))
cat("\nLambda matrice originale :", round(lambda_orig, 6))
cat("\nDifférence :", round(lambda_L6 - lambda_orig, 6))