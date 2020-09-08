library(mcmcse)
library(coda)
library(extraDistr)
library(tidyr)
source("Util.R")





# length of dimensions is number of variables, each number in dimensions is number of possible values for each variable

dimensions <- c(10,10,10,10)

# each rowhas to be a distribution
network <- bayesianNetwork(dimensions)
network <- array(data = rep(0.1, 10000), dim = dimensions)




## Set up priors for each variable
priors <- matrix(ncol = max(dimensions))
for (i in dimensions) {
  prior <- generateDistribution(i)
  if (i < max(dimensions)) {
    fillup <- max(dimensions) - i
    for (j in seq(fillup)) {
      prior <- c(prior, 0)
    }
  }
  priors <- rbind(priors, prior)
}
priors <-priors[-1,]

for (i in seq(4)) {priors[i,] <- 0.1}

## table to store real probabilites
table <- as.matrix(crossing(Y = 1:10, X1 = 1:10, X2 = 1:10, X3 = 1:10, value = 0))
# helper table to store 
index_table <- as.matrix(crossing(X1 = 1:10, X2 = 1:10, X3 = 1:10))



## Dividend p(y|X1,X2,X3) * p(X1) * p(X2) * p(X3)
for(i in seq(1000)) {
  indices <- index_table[i,]
  table[i,5] <- network[1,indices[1],indices[2], indices[3]] * priors[2,indices[1]] * priors[3,indices[2]] * priors[4, indices[3]]
}
# p(Y=1) = divisor
divisor <- rep(0,10)
for (i in seq(1000)) {
  divisor[1] <- divisor[1] + table[i, 5]
}
# normalized posterior
for (i in seq(1000)) {
  table[i, 5] <- table[i, 5] / divisor[1]
}


## Dividend p(y|X1,X2,X3) * p(X1) * p(X2) * p(X3)
for(i in seq(1000)) {
  indices <- index_table[i,]
  table[i+1000,5] <- network[2,indices[1],indices[2], indices[3]] * priors[2,indices[1]] * priors[3,indices[2]] * priors[4, indices[3]]
}
# p(Y=2) = divisor
for (i in seq(1000)) {
  divisor[2] <- divisor[2] + table[i+1000, 5]
}
# normalized posterior
for (i in seq(1000)) {
  table[i+1000, 5] <- table[i+1000, 5] / divisor[2]
}


## Dividend p(y|X1,X2,X3) * p(X1) * p(X2) * p(X3)
for(i in seq(1000)) {
  indices <- index_table[i,]
  table[i+2000,5] <- network[3,indices[1],indices[2], indices[3]] * priors[2,indices[1]] * priors[3,indices[2]] * priors[4, indices[3]]
}
# p(Y=3) = divisor
for (i in seq(1000)) {
  divisor[3] <- divisor[3] + table[i+2000, 5]
}
# normalized posterior
for (i in seq(1000)) {
  table[i+2000, 5] <- table[i+2000, 5] / divisor[3]
}


## Dividend p(y|X1,X2,X3) * p(X1) * p(X2) * p(X3)
for(i in seq(1000)) {
  indices <- index_table[i,]
  table[i+3000,5] <- network[4,indices[1],indices[2], indices[3]] * priors[2,indices[1]] * priors[3,indices[2]] * priors[4, indices[3]]
}
# p(Y=4) = divisor
for (i in seq(1000)) {
  divisor[4] <- divisor[4] + table[i+3000, 5]
}
# normalized posterior
for (i in seq(1000)) {
  table[i+3000, 5] <- table[i+3000, 5] / divisor[4]
}



## Dividend p(y|X1,X2,X3) * p(X1) * p(X2) * p(X3)
for(i in seq(1000)) {
  indices <- index_table[i,]
  table[i+4000,5] <- network[5,indices[1],indices[2], indices[3]] * priors[2,indices[1]] * priors[3,indices[2]] * priors[4, indices[3]]
}
# p(Y=5) = divisor
for (i in seq(1000)) {
  divisor[5] <- divisor[5] + table[i+4000, 5]
}
# normalized posterior
for (i in seq(1000)) {
  table[i+4000, 5] <- table[i+4000, 5] / divisor[5]
}



## Dividend p(y|X1,X2,X3) * p(X1) * p(X2) * p(X3)
for(i in seq(1000)) {
  indices <- index_table[i,]
  table[i+5000,5] <- network[6,indices[1],indices[2], indices[3]] * priors[2,indices[1]] * priors[3,indices[2]] * priors[4, indices[3]]
}
# p(Y=6) = divisor
for (i in seq(1000)) {
  divisor[6] <- divisor[6] + table[i+5000, 5]
}
# normalized posterior
for (i in seq(1000)) {
  table[i+5000, 5] <- table[i+5000, 5] / divisor[6]
}


## Dividend p(y|X1,X2,X3) * p(X1) * p(X2) * p(X3)
for(i in seq(1000)) {
  indices <- index_table[i,]
  table[i+6000,5] <- network[7,indices[1],indices[2], indices[3]] * priors[2,indices[1]] * priors[3,indices[2]] * priors[4, indices[3]]
}
# p(Y=7) = divisor
for (i in seq(1000)) {
  divisor[7] <- divisor[7] + table[i+6000, 5]
}
# normalized posterior
for (i in seq(1000)) {
  table[i+6000, 5] <- table[i+6000, 5] / divisor[7]
}


## Dividend p(y|X1,X2,X3) * p(X1) * p(X2) * p(X3)
for(i in seq(1000)) {
  indices <- index_table[i,]
  table[i+7000,5] <- network[8,indices[1],indices[2], indices[3]] * priors[2,indices[1]] * priors[3,indices[2]] * priors[4, indices[3]]
}
# p(Y=8) = divisor
for (i in seq(1000)) {
  divisor[8] <- divisor[8] + table[i+7000, 5]
}
# normalized posterior
for (i in seq(1000)) {
  table[i+7000, 5] <- table[i+7000, 5] / divisor[8]
}


## Dividend p(y|X1,X2,X3) * p(X1) * p(X2) * p(X3)
for(i in seq(1000)) {
  indices <- index_table[i,]
  table[i+8000,5] <- network[9,indices[1],indices[2], indices[3]] * priors[2,indices[1]] * priors[3,indices[2]] * priors[4, indices[3]]
}
# p(Y=9) = divisor
for (i in seq(1000)) {
  divisor[9] <- divisor[9] + table[i+8000, 5]
}
# normalized posterior
for (i in seq(1000)) {
  table[i+8000, 5] <- table[i+8000, 5] / divisor[9]
}


## Dividend p(y|X1,X2,X3) * p(X1) * p(X2) * p(X3)
for(i in seq(1000)) {
  indices <- index_table[i,]
  table[i+9000,5] <- network[10,indices[1],indices[2], indices[3]] * priors[2,indices[1]] * priors[3,indices[2]] * priors[4, indices[3]]
}
# p(Y=10) = divisor
for (i in seq(1000)) {
  divisor[10] <- divisor[10] + table[i+9000, 5]
}
# normalized posterior
for (i in seq(1000)) {
  table[i+9000, 5] <- table[i+9000, 5] / divisor[10]
}





## create an initial sample

initial <- c(1,1,1,1)

ESS <- rep(0, 10)

for (i in seq(10)) {
  samples <- RWM(initial, i, network, dimensions, priors, 3000)
  ESS[i] <- multiESS(samples)
}

optimalParameter = which.max(ESS)

errors <- rep(0,20)

samples <- rbind(initial)

for (i in seq(20)) {
  tail <- samples[length(samples[,1]),]
  samples <- rbind(samples, RWM(tail, optimalParameter, network, dimensions, priors, 5000))
  sampledValues <- as.matrix(crossing(Y = 1:10, X1 = 1:10, X2 = 1:10, X3 = 1:10))
  
  if (length(sampledValues[,1])!= prod(dimensions)) {
    print("Not converged at iteration")
    print(i)
    next
  }
  
  
  ## count occurences of each sample
  occurences <- rep(0, prod(dimensions))
  
  for (k in seq(prod(dimensions))) {
    for (j in seq(length(samples[,1]))) {
      if (all(sampledValues[k,] == samples[j,])) {
        occurences[k] <- occurences[k] + 1
      }
    }
  }
  sampledValues <- cbind(sampledValues, occurences)
  sampledValues <- sampledValues[do.call(order, as.data.frame(sampledValues)),]
  
  Y1 <- sum(sampledValues[sampledValues[,1] == 1,c(5)])
  Y2 <- sum(sampledValues[sampledValues[,1] == 2,c(5)])
  Y3 <- sum(sampledValues[sampledValues[,1] == 3,c(5)])
  Y4 <- sum(sampledValues[sampledValues[,1] == 4,c(5)])
  Y5 <- sum(sampledValues[sampledValues[,1] == 5,c(5)])
  Y6 <- sum(sampledValues[sampledValues[,1] == 6,c(5)])
  Y7 <- sum(sampledValues[sampledValues[,1] == 7,c(5)])
  Y8 <- sum(sampledValues[sampledValues[,1] == 8,c(5)])
  Y9 <- sum(sampledValues[sampledValues[,1] == 9,c(5)])
  Y10 <- sum(sampledValues[sampledValues[,1] == 10,c(5)])
  
  
  for (m in seq(1000)) { sampledValues[m,5] <- sampledValues[m,5] / Y1 }
  for (n in seq(1001,2000)) { sampledValues[n,5] <- sampledValues[n,5] / Y2 }
  for (n in seq(2001,3000)) { sampledValues[n,5] <- sampledValues[n,5] / Y3 }
  for (n in seq(3001,4000)) { sampledValues[n,5] <- sampledValues[n,5] / Y4 }
  for (n in seq(4001,5000)) { sampledValues[n,5] <- sampledValues[n,5] / Y5 }
  for (n in seq(5001,6000)) { sampledValues[n,5] <- sampledValues[n,5] / Y6 }
  for (n in seq(6001,7000)) { sampledValues[n,5] <- sampledValues[n,5] / Y7 }
  for (n in seq(7001,8000)) { sampledValues[n,5] <- sampledValues[n,5] / Y8 }
  for (n in seq(8001,9000)) { sampledValues[n,5] <- sampledValues[n,5] / Y9 }
  for (n in seq(9000,10000)) { sampledValues[n,5] <- sampledValues[n,5] / Y10 }
  
  error <- 0
  for (o in seq(prod(dimensions))) {
    error <- error + abs(table[o,5] - sampledValues[o,5])
  }
  errors[i] <- error
}

errors <- unlist(errors)

errors_ess <- errors
ess_exploration_factor <- sum(sampledValues[,5] != 0) / 100

write(errors_ess, "uniform ess errors")
write.csv2(samples, "uniform network ess samples")
write(ess_exploration_factor, "uniform ess exploration factor")














## acceptance


acceptance_rates <- rep(0, 10)

for (i in seq(10)) {
  acceptance_rates[i] <- RWM_Acceptance(initial, i, network, dimensions, priors, 3000)
}

optimalParameter = which.min(acceptance_rates)
distance_from_optimal <- rep(0.234, length(acceptance_rates))
distance_from_optimal <- distance_from_optimal - acceptance_rates
optimalParameter <- which.min(abs(distance_from_optimal))


samples <- rbind(initial)

for (i in seq(20)) {
  tail <- samples[length(samples[,1]),]
  samples <- rbind(samples, RWM(tail, optimalParameter, network, dimensions, priors, 5000))
  sampledValues <- as.matrix(crossing(Y = 1:10, X1 = 1:10, X2 = 1:10, X3 = 1:10))
  
  if (length(sampledValues[,1])!= prod(dimensions)) {
    print("Not converged at iteration")
    print(i)
    next
  }
  
  
  ## count occurences of each sample
  occurences <- rep(0, prod(dimensions))
  
  for (k in seq(prod(dimensions))) {
    for (j in seq(length(samples[,1]))) {
      if (all(sampledValues[k,] == samples[j,])) {
        occurences[k] <- occurences[k] + 1
      }
    }
  }
  sampledValues <- cbind(sampledValues, occurences)
  sampledValues <- sampledValues[do.call(order, as.data.frame(sampledValues)),]
  
  Y1 <- sum(sampledValues[sampledValues[,1] == 1,c(5)])
  Y2 <- sum(sampledValues[sampledValues[,1] == 2,c(5)])
  Y3 <- sum(sampledValues[sampledValues[,1] == 3,c(5)])
  Y4 <- sum(sampledValues[sampledValues[,1] == 4,c(5)])
  Y5 <- sum(sampledValues[sampledValues[,1] == 5,c(5)])
  Y6 <- sum(sampledValues[sampledValues[,1] == 6,c(5)])
  Y7 <- sum(sampledValues[sampledValues[,1] == 7,c(5)])
  Y8 <- sum(sampledValues[sampledValues[,1] == 8,c(5)])
  Y9 <- sum(sampledValues[sampledValues[,1] == 9,c(5)])
  Y10 <- sum(sampledValues[sampledValues[,1] == 10,c(5)])
  
  
  for (m in seq(1000)) { sampledValues[m,5] <- sampledValues[m,5] / Y1 }
  for (n in seq(1001,2000)) { sampledValues[n,5] <- sampledValues[n,5] / Y2 }
  for (n in seq(2001,3000)) { sampledValues[n,5] <- sampledValues[n,5] / Y3 }
  for (n in seq(3001,4000)) { sampledValues[n,5] <- sampledValues[n,5] / Y4 }
  for (n in seq(4001,5000)) { sampledValues[n,5] <- sampledValues[n,5] / Y5 }
  for (n in seq(5001,6000)) { sampledValues[n,5] <- sampledValues[n,5] / Y6 }
  for (n in seq(6001,7000)) { sampledValues[n,5] <- sampledValues[n,5] / Y7 }
  for (n in seq(7001,8000)) { sampledValues[n,5] <- sampledValues[n,5] / Y8 }
  for (n in seq(8001,9000)) { sampledValues[n,5] <- sampledValues[n,5] / Y9 }
  for (n in seq(9000,10000)) { sampledValues[n,5] <- sampledValues[n,5] / Y10 }
  
  error <- 0
  for (o in seq(prod(dimensions))) {
    error <- error + abs(table[o,5] - sampledValues[o,5])
  }
  errors[i] <- error
}

errors_acceptance <- unlist(errors)
ar_exploration_factor <- sum(sampledValues[,5] != 0) / 100

write(errors_acceptance, "uniform acceptance errors")
write.csv2(samples, "uniform network acceptance samples")
write(ar_exploration_factor, "uniform acceptance exploration factor")










