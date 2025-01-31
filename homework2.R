# Al Pakrosnis
# Prof. Dale Reed
# STAT 385
# Homework 2

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

# Q3

# Q4


