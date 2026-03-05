# ==========================================
# Question 2: Computer Chips (Discrete PMF)
# ==========================================
cat("--- Question 2: Computer Chips ---\n")
x2 <- c(0, 1, 2, 3, 4)
p2 <- c(0.4, 0.3, 0.15, 0.10, 0.05)

# P(X <= 2) and P(X > 1)
prob_x_le_2 <- sum(p2[x2 <= 2])
prob_x_gt_1 <- sum(p2[x2 > 1])

# Mean and Variance
mu2 <- sum(x2 * p2)
var2 <- sum((x2^2) * p2) - mu2^2

cat("P(X <= 2):", prob_x_le_2, "\n")
cat("P(X > 1): ", prob_x_gt_1, "\n")
cat("Mean:     ", mu2, "\n")
cat("Variance: ", var2, "\n\n")

# ==========================================
# Question 3: Chemical Supply
# ==========================================
cat("--- Question 3: Chemical Supply ---\n")
x3 <- c(1, 2, 3, 4, 5)
p3 <- c(0.4, 0.2, 0.2, 0.1, 0.1)

mu3 <- sum(x3 * p3)
var3 <- sum((x3^2) * p3) - mu3^2
sd3 <- sqrt(var3)

# Scaling by 10 (Y = 10X)
mu_Y <- 10 * mu3
var_Y <- 10^2 * var3
sd_Y <- 10 * sd3

cat("Drums - Mean:", mu3, "| Variance:", var3, "| StdDev:", sd3, "\n")
cat("Gallons - Mean:", mu_Y, "| Variance:", var_Y, "| StdDev:", sd_Y, "\n\n")

# ==========================================
# Question 1: Linear Combinations
# ==========================================
cat("--- Question 1: Linear Combinations ---\n")
muX <- 9.5; sdX <- 0.4
muY <- 6.8; sdY <- 0.1

cat("3X    -> Mean:", 3 * muX, "| StdDev:", 3 * sdX, "\n")
cat("Y - X -> Mean:", muY - muX, "| StdDev:", sqrt(sdY^2 + sdX^2), "\n")
cat("X + 4Y-> Mean:", muX + 4 * muY, "| StdDev:", sqrt(sdX^2 + 16 * sdY^2), "\n\n")

# ==========================================
# Question 10: Daily Revenue
# ==========================================
cat("--- Question 10: Daily Revenue ---\n")
mu_gas <- c(1500, 500, 300)
sd_gas <- c(180, 90, 40)
prices <- c(2.60, 2.75, 2.90)

# Dot product for mean, weighted sum of variances for variance
mu_R <- sum(prices * mu_gas)
var_R <- sum((prices^2) * (sd_gas^2))
sd_R <- sqrt(var_R)

cat("Mean Revenue:   $", mu_R, "\n")
cat("StdDev Revenue: $", sd_R, "\n")
