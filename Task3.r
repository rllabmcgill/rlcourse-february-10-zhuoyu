# Show the single estimator of the maximum expected value is biased

# assume we have three random variables

# each variable has its normal distribution

# get the 200 means of 100 samples for each variable

# get the maximum

# plot the density
X1 <- rep(NA, 200)
X2 <- rep(NA, 200)
X3 <- rep(NA, 200)
Q1 <- rep(NA, 200)
Q2 <- rep(NA, 200)
for(i in 1:200){
	X1[i] <- mean(rnorm(100, mean=0, sd=2))
	X2[i] <- mean(rnorm(100, mean=0.2, sd=2))
	X3[i] <- mean(rnorm(100, mean=0.3, sd=2))
	Q1[i] <- max(X1[i], X2[i], X3[i])
	a <- which(c(X1[i], X2[i], X3[i])==Q1[i])
	Q2[i] <- mean(rnorm(100, mean=c(0, 0.2, 0.3)[a], sd=2))
}

library(ggplot2)
df1 <- data.frame(Value = c(X1, X2, X3, Q1, Q2),Estimator=rep(c("u1","u2","u3","Q1","Q2"), each=200))
ggplot(df1, aes(Value, fill = Estimator, colour = Estimator)) +
  geom_density(alpha = 0.1) + geom_vline(xintercept = c(0, 0.2, 0.3, mean(Q1), mean(Q2)))
  
# convergence
X1 <- matrix(rep(NA, 200*200), nrow=200)
X2 <- matrix(rep(NA, 200*200), nrow=200)
X3 <- matrix(rep(NA, 200*200), nrow=200)
Q1 <- matrix(rep(NA, 200*200), nrow=200)
Q2 <- matrix(rep(NA, 200*200), nrow=200)
for(j in 1:200){
	for(i in 1:200){
		X1[i,j] <- mean(rnorm(i*5, mean=0, sd=2))
		X2[i,j] <- mean(rnorm(i*5, mean=0.2, sd=2))
		X3[i,j] <- mean(rnorm(i*5, mean=0.3, sd=2))
		Q1[i,j] <- max(X1[i,j], X2[i,j], X3[i,j])
		a <- which(c(X1[i,j], X2[i,j], X3[i,j])==Q1[i,j])
		Q2[i,j] <- mean(rnorm(i*5, mean=c(0, 0.2, 0.3)[a], sd=2))
	}
}
df2 <- data.frame(Value = c(rowMeans(X1), rowMeans(X2), rowMeans(X3), rowMeans(Q1), rowMeans(Q2)),Estimator=rep(c("u1","u2","u3","Q1","Q2"), each=200), SampleSize=rep(5*(1:200),5))
ggplot(df2, aes(x=SampleSize, y=Value, fill = Estimator, colour = Estimator)) + geom_line()# +  geom_line(aes(linetype=Estimator))
  