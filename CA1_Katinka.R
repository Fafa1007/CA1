library(readr)
CA1 <- read_csv("CA1.csv")
View(CA1)

dat <- as.matrix(CA1)

# Question 1
means <- matrix(nrow = 5, ncol = 5)
for (i in 1:5) {
  # calculate the means for all column for one time period
  means[i,] = t(apply(dat[dat[,5] == i,], 2, mean))
}

means[,1:4]

# can also use dplyr

# alternative method: matrices -

all_means <- matrix(nrow = 5, ncol = 5)
for (i in 1:5) { # one loop for each time period
  mat_i <- dat[dat[,5] == i,]
  vec1 <- matrix(1, nrow = nrow(mat_i))
  means_i <- (1/nrow(mat_i)) * vec1 %*% t(vec1) %*% mat_i
  all_means[i,] <- means_i[1,]
}

print(all_means)

# Question 2
install.packages("corrplot")
library(corrplot)

for (i in 1:5) {
  png(paste0("correlation_plot_period_", i, ".png"), width = 800, height = 600)
  mat_i <- dat[dat[,5] == i,1:4]
  varcovmat <- cor(mat_i)
  corrplot(varcovmat, method = 'circle')
  dev.off()
}

# Question 3
tp1 <- dat[dat[,5]==1,]
X1 <- tp1[,1]
X2 <- tp1[,3]
vec1 <- matrix(1, nrow = length(X1))

# method 1: use the deviance vectors

dev_vec_X1 <- X1 - 1/length(X1) * vec1 %*% t(vec1) %*% X1
dev_vec_X2 <- X2 - 1/length(X2) * vec1 %*% t(vec1) %*% X2
angle <- acos((t(dev_vec_X1)%*% dev_vec_X2)/(norm(dev_vec_X1)*norm(dev_vec_X2)))
angle # in radians

# method 2: use the variance covariance matrix

cov_mat <- cov(tp1)
angle <- acos(cov_mat[1,3] / (sqrt(cov_mat[1,1])*sqrt(cov_mat[3,3])))
angle # in radians

# The angle is 1.5 radians, which is almost pi/2 (a 90 degree angle). This implies that the vectors are almost orthogonal, meaning that they have nearly no correlation. In question 2, we see that the correlation between x1 and x3 is 0.015, which is indeed small.

# Question 4
b <- matrix(c(-1, 0, 0, 3, 0), nrow = 5)
y_means <- t(b) %*% t(all_means)
y_means

Y <- matrix(ncol = 5, nrow = 30)
for (i in 1:5) {
  mat_i <- dat[dat[,5] == i,]
  y_i <- mat_i %*% b
  Y[,i] <- y_i
}

cov(Y)

y_cov <- t(b) %*% cov(all_means) %*% b
y_cov
