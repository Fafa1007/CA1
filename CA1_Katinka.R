CA1 <- read.csv("CA1.csv")
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
