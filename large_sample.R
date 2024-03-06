# Bernoulli random variable Y
# probability that Y = 1 is p = 0.78 per SW
# sample size n = 2, 5, 25, 100, 1000 
# r = 10,000 repeated samples 

r <- 10^4 # number of replications
p <- 0.78

muY <- p
sigmaY <- sqrt(p*(1-p))

# distribution of Ybar and standardized Ybar 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# n = 2 observations (sample size)
n <- 2

# replicate() returns a matrix with dimensions n*r, 
# containing drawn samples as columns.
# rbinom() generates pseudo-random numbers from binomial(size, prob)
# binomial with size = 1 is equivalent to bernoulli
Y <- replicate(r, rbinom(n, size = 1, prob = p)) 
# colMeans() computes the mean of each column and returns a vector.
Ybar <- colMeans(Y) # sample means
head(Ybar) # print the first few entries of Ybar to the console
mean(Ybar) # to illustrate LLN

# To see probability (i.e. relative frequency), use histogram() 
# after uploading the lattice package
library(lattice)
histogram(Ybar, xlim = c(-0.5, 1.5), 
          main = "Sampling distribution of Ybar: n = 2")

# plot the density of Ybar 
hist(Ybar,
     freq = FALSE,
     xlim = c(0, 1), 
     main = "Sampling distribution of Ybar: n = 2")

# overlay N(muY, sigmaY/sqrt(n)) pdf
curve(dnorm(x, mean = muY, sd = sigmaY/sqrt(n)), 
      col = "red", 
      add = TRUE)

# standardized sample means
sYbar <- (Ybar - muY)/(sigmaY/sqrt(n)) 

# plot relative frequency
histogram(sYbar, xlim = c(-3.5, 3.5), 
          main = "Sampling distribution of standardized Ybar: n = 2")

# plot the density (instead of relative frequency) of sYbar 
hist(sYbar,
     freq = F, 
     xlim = c(-3, 3), 
     main = "Sampling distribution of standardized Ybar: n = 2")

# overlay N(0,1) pdf
curve(dnorm(x), 
      col = "red", 
      add = T)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Repeat for n = 5, 25, 50, 100, 1000

# loop over various sample sizes
for (n in c(5, 25, 50, 100, 1000)) {
  Y <- replicate(r, rbinom(n, size = 1, prob = p))
  Ybar <- colMeans(Y)
  sYbar <- (Ybar - muY)/(sigmaY/sqrt(n))
  
  print(paste("n =", n))
  print(mean(Ybar))
  print(head(Ybar))
  
  #print(histogram(Ybar, xlim = c(-0.5, 1.5), main = paste("Sampling distribution of Ybar: n =", n)))
  
  hist(Ybar, freq = F, xlim = c(0, 1), main = paste("Sampling distribution of Ybar: n =", n))
  curve(dnorm(x, mean = muY, sd = sigmaY/sqrt(n)), col = "red", add = T)
 
  #print(histogram(sYbar, xlim = c(-3.5, 3.5),  main = paste("Sampling distribution of standardized Ybar: n =", n)))
  
  hist(sYbar, freq = F, xlim = c(-3, 3), main = paste("Sampling distribution of standardized Ybar: n =", n))
  curve(dnorm(x), col = "red", add = T)
}
