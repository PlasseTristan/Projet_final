# PARAMÈTRES DE BASE
# ==========================================================
f_rates <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             18096, 21035, 24114, 27133, 29563, 31934, 34142, 35761,
             37062, 38435, 39821, 40936, 41923, 42972, 44075, 45086,
             46025, 46914, 47767, 48568, 0)

s_rates <- c(
  0.073, 0.023, 0.006, 0.609, 0.723, 0.778, 0.812, 0.836, 0.851, 0.863, 0.875, 0.887, 0.893,
  0.895, 0.898, 0.902, 0.903, 0.905, 0.908, 0.910, 0.910, 0.910, 0.911, 0.912, 0.911, 0.911,
  0.912, 0.912, 0.912, 0.913, 0.913, 0.913, 0
)



# 1. Calcul des paramètres pour 5 stades
f_vals <- c(f_rates[1], f_rates[2], f_rates[3], mean(f_rates[4:12]), mean(f_rates[13:33]))
s_vals <- c(s_rates[1], s_rates[2], s_rates[3], mean(s_rates[4:12]), mean(s_rates[13:33]))

n_stades <- 5
L5 <- matrix(0, n_stades, n_stades)

# --- FÉCONDITÉS (Ligne 1) ---
L5[1, ] <- f_vals

# --- TRANSITIONS STADES 1 à 3 (Strictes) ---
L5[2, 1] <- s_rates[1]
L5[3, 2] <- s_rates[2]
L5[4, 3] <- s_rates[3] # Transition du stade 3 vers le bloc 4-12

# --- BLOC 4 : Âges 4-12 (durée d=9) ---
res4 <- calc_PG(s_vals[4], 9)
L5[4, 4] <- res4$P
L5[5, 4] <- res4$G

# --- BLOC 5 : Âges 13-33 (durée d=21) ---
res5 <- calc_PG(s_vals[5], 21)
L5[5, 5] <- res5$P 

print(round(L5, 5))
# Comparaison des Lambdas
lambda_condensee <- Re(eigen(L5)$values[1])
cat("\nLambda matrice condensée :", round(lambda_condensee, 6))
cat("\nLambda matrice M10 :", round(Re(eigen(measured)$values[1]), 6))