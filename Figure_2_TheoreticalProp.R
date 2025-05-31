# Define solve_rho function
solve_rho <- function(p0, p1) {
  if (p0 <= 0 || p0 >= 1 || p1 <= 0 || p1 >= 1) {
    return(NA)
  }
  
  equation <- function(rho) {
    term1 <- (p0 - p1) * ((p0 * (1 - p0 + rho * p0)) / rho + 
                            (p1 - rho * p1^2) / (1 - rho) - 
                            2 * p0 * p1)
    term2 <- (1 - p0 + rho * p0 - rho * p1) * 
      ((p1 * (1 - p1)) / (1 - rho)^2 - 
         (p0 * (1 - p0)) / rho^2)
    term1 + term2
  }
  
  tryCatch({
    uniroot(equation, lower = 1e-6, upper = 1 - 1e-6)$root
  }, error = function(e) {
    NA
  })
}

# Define calculate_values function
calculate_values <- function(p0, p1) {
  denominator <- sqrt(p0 * (1 - p0)) + sqrt(p1 * (1 - p1))
  N_0 <- sqrt(p0 * (1 - p0)) / denominator
  N_1 <- sqrt(p1 * (1 - p1)) / denominator
  R_1 <- sqrt(p1) / (sqrt(p0) + sqrt(p1))
  c(N_0 = N_0, N_1 = N_1, R_1 = R_1)
}

# Parameters
p0 <- 0.5
p1_values <- seq(0.05, 0.95, by = 0.05)

# Compute R_0
rho_values <- sapply(p1_values, function(p1) solve_rho(p0, p1))

# Compute N_0, N_1, R_1
results <- t(sapply(p1_values, function(p1) calculate_values(p0, p1)))
N_0 <- results[, "N_0"]
N_1 <- results[, "N_1"]
R_1 <- results[, "R_1"]

# Plotting
plot(p1_values, rho_values, type = "o", pch = 19, col = "red",
     ylim = c(0, 1), xlab = expression(p[1]), ylab = "Allocation Proportion",
     main = expression(paste("Allocation proportions given that ", p[0], " = 0.7")))
lines(p1_values, R_1, type = "o", pch = 19, col = "lightcoral")
lines(p1_values, N_0, type = "o", pch = 17, col = "blue")
lines(p1_values, N_1, type = "o", pch = 17, col = "lightblue")

# Add legend
legend("topleft", legend = c(expression(rho[R[0]^n]), expression(rho[R[1]]), expression(rho[N[0]^n]), expression(rho[N[1]])),
       col = c("red", "lightcoral", "blue", "lightblue"), pch = c(19, 19, 17, 17), lty = 1)

# Add reference line and grid
abline(h = 0.5, col = "grey", lty = 2)
grid()



# --- Second Plot ---
# Plot R_1, N_1, and CR = 0.5
plot(p1_values, R_1, type = "o", pch = 19, col = "lightcoral",
     ylim = c(0, 1), xlab = expression(p[1]), ylab = "Allocation Proportion",
     main = expression(paste("Allocation proportions given that ", p[0], " = 0.5")))
lines(p1_values, N_1, type = "o", pch = 17, col = "lightblue")
abline(h = 0.5, col = "green", lty = 2, lwd = 2)  # CR = 0.5

# Add legend
legend("topleft", legend = c(expression(rho[R[1]]), expression(rho[N[1]]), expression(rho[CR])),
       col = c("lightcoral", "lightblue", "green"), pch = c(19, 17, NA), lty = c(1, 1, 2), lwd = c(1, 1, 2))
