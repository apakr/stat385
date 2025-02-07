# Al Pakrosnis
# Prof. Dale Embers
# STAT 385
# Homework 2

setwd("~/Desktop/stat385/")

# Q1 (a)
set.seed(385)
exp_sample <- rexp(500, 1/4)

mean(exp_sample)
# 4.280625

var(exp_sample)
# 16.48086

hist(exp_sample)
# Add plot

mean(exp_sample > 2)
# .646

pexp(2, rate = 1/4, lower.tail = FALSE)
# .6065307

# (b)
m1 <- matrix(exp_sample, nrow = 10, ncol = 50, byrow = TRUE)
col_means <- apply(m1, 2, mean)

# (c)
exp_sample[exp_sample>4]


# Q2 (a)
n_values <- c(10, 20, 30)
p_values <- c(0.25, 0.6)
expected <- matrix(0, nrow = length(n_values), ncol = length(p_values), dimnames = list(paste0("n=", n_values), paste0("p=", p_values)))

for (i in seq_along(n_values)) {
  for (j in seq_along(p_values)) {
    expectation <- 0
    n <- n_values[i]
    p <- p_values[j]
    for (k in 0:n) {
      prob <- choose(n, k) * p^k * (1 - p)^(n - k)
      expectation <- expectation + k * prob
    }
    expected[i, j] <- expectation
  }
}
expected

# (b)
BinExp <- function(n, p) {
  if (n <= 1 || p <= 0 || p >= 1) stop("n must be > 1 and 0 < p < 1")
  expectation <- 0
  for (k in 0:n) {
    prob <- choose(n, k) * p^k * (1 - p)^(n - k)
    expectation <- expectation + k * prob
  }
  return(expectation)
}

# Q3
par(mfrow = c(2, 3))
for (n in n_values) {
  for (p in p_values) {
    k_values <- 0:n
    probabilities <- sapply(k_values, function(k) choose(n, k) * p^k * (1 - p)^(n - k))
    plot(k_values, probabilities, type = "h", lwd = 2, col = "blue", 
         main = paste("PMF for n =", n, "p =", p),
         xlab = "k (possible values)", ylab = "P(X=k)")
  }
}

# Q4 (a)
schools <- read.csv("College.csv")

# (b)
rownames(schools) <- schools[, 1]
schools <- schools[, -1] 

View(schools)

# (c)
summary(schools)

pairs(schools[, sapply(schools, is.numeric)])

boxplot(Enroll ~ Private, data = schools)

schools$Faculty <- cut(schools$PhD, breaks = c(-Inf, 75, 85, Inf), labels = c("Moderate", "Good", "Great"), ordered = TRUE)
boxplot(Top10perc ~ Faculty, data = schools)

par(mfrow = c(2, 2))
hist(schools$Outstate, breaks = 20, main = "Out-of-State Tuition", xlab = "Outstate", col = "lightgreen")
hist(schools$PhD, breaks = 15, main = "PhD Percentage", xlab = "PhD", col = "skyblue")
hist(schools$Enroll, breaks = 15, main = "Enrollment", xlab = "Enrollment", col = "salmon")
hist(schools$Top10perc, breaks = 10, main = "Top 10% Percentage", xlab = "Top 10%", col = "gold")

