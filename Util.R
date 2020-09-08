library(extraDistr)

# generates a discrete probability distribution with a set number of possible values the distribution can take (e.g. 2 for coinflip)
generateDistribution <- function(nrPossibleValues) {
  x = runif(nrPossibleValues)
  x = x / sum(x)
  return(x)
}

# generates a Bayesian Network
# dimensions is a vector of numbers, wherein each value specifies the number of possible values each node can take. All but the first represent parents feeding into the child node.
bayesianNetwork <- function(dimensions) {
  x <- vector()
  for (i in seq(prod(dimensions))) {
    x <- c(x, generateDistribution(dimensions[1]))
  }
  return(array(x, dim = dimensions))
}

generateProposal <- function(lastPosition, jumpDistance) {
  proposals <- vector()
  for (i in seq(lastPosition)) {
    proposals <- c(proposals, rdunif(1, lastPosition[i]-jumpDistance, lastPosition[i]+jumpDistance))
  }
  return(proposals)
}

## from https://stackoverflow.com/questions/11561856/add-new-row-to-dataframe-at-specific-row-index-not-appended
insertRow <- function(existingDF, newrow, r) {
  if (r == 1 ) {return(rbind(c(1,1,1), existingDF))}
  if (r == length(existingDF[,1])) {return(rbind(existingDF, c(2,2,2)))}
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  return(existingDF)
}


RWM <- function(initial, distance, network, dimensions, priors, iterations) {
  
  ## create matrix where to store the sampls
  samples <- matrix(data = initial, nrow = 1, ncol = length(initial))
  
  
  
  ### SAMPLING STARTS ###
  for (iter in seq(iterations)) {
    print(iter)
    lastSample <- tail(samples, n=1)
    
    # the proposed sample
    proposal <- generateProposal(lastSample, jumpDistance = distance)
    
    # next two loops check proposal for validity
    for (i in proposal) {
      if (i < 1 || i > max(dimensions)) {
        proposal <- as.vector(lastSample)
      }
    }
    
    for (i in seq(proposal)) {
      if (priors[i, proposal[i]] == 0) {
        proposal <- as.vector(lastSample)
      }
    }
    
    
    ## select the value in the network array that corresponds to the proposal p(y|x1,...,xN)
    value <- network[t(as.matrix(proposal))]
    
    
    # compute p(y|x1,...,xN) * p(x1) * ... * p(xN)
    for (i in seq(length(proposal)-1)) {
      value <- value * priors[i+1,proposal[i+1]]
    }
    
    lastValue <- network[lastSample]
    for (i in seq(length(lastSample)-1)) {
      lastValue <- lastValue * priors[i+1,lastSample[i+1]]
    }
    
    proposalValue <- value / lastValue
    
    ## the acceptance probablity will need 
    pAccept <- runif(1, min = 0, max = 1)
    
    ## accept with probability pAccept
    if (proposalValue >= pAccept) {
      samples <- rbind(samples, proposal)
    } else {
      samples <- rbind(samples, lastSample)
    }
    
  }
  return(samples)
}

RWM_Acceptance <- function(initial, distance, network, dimensions, priors, iterations) {
  
  ## create matrix where to store the sampls
  samples <- matrix(data = initial, nrow = 1, ncol = length(initial))
  
  ## number of accepted moves
  accepted <- 0
  
  
  ### SAMPLING STARTS ###
  for (iter in seq(iterations)) {
    lastSample <- tail(samples, n=1)
    
    # the proposed sample
    proposal <- generateProposal(lastSample, jumpDistance = distance)
    
    # next two loops check proposal for validity
    for (i in proposal) {
      if (i < 1 || i > max(dimensions)) {
        proposal <- as.vector(lastSample)
      }
    }
    
    for (i in seq(proposal)) {
      if (priors[i, proposal[i]] == 0) {
        proposal <- as.vector(lastSample)
      }
    }
    
    
    ## select the value in the network array that corresponds to the proposal p(y|x1,...,xN)
    value <- network[t(as.matrix(proposal))]
    
    
    # compute p(y|x1,...,xN) * p(x1) * ... * p(xN)
    for (i in seq(length(proposal)-1)) {
      value <- value * priors[i+1,proposal[i+1]]
    }
    
    lastValue <- network[lastSample]
    for (i in seq(length(lastSample)-1)) {
      lastValue <- lastValue * priors[i+1,lastSample[i+1]]
    }
    
    proposalValue <- value / lastValue
    
    ## the acceptance probablity will need 
    pAccept <- runif(1, min = 0, max = 1)
    
    ## accept with probability pAccept
    if (proposalValue >= pAccept) {
      samples <- rbind(samples, proposal)
      
      ## this deterines if a new proposal that is the same as the old one is counted
      if(!all(proposal == lastSample)) {
        #print(proposal)
        #print(lastSample)
        accepted <- accepted + 1 
      }
    } else {
      samples <- rbind(samples, lastSample)
    }
    
  }
  #acceptance_rate_storage[2] <- accepted/iterations
  print(accepted)
  acceptance_rate <- accepted/iterations
  return(acceptance_rate)
}
