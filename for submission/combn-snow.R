####################################################################################
# Snow parallelization of combn() from the CRAN
# combinat package: http://cran.r-project.org/web/packages/combinat/index.html

# Function Arguments:
# cls <- clusters
# x <- input vector of integers and/or characters
# m <- number of elements in a combination
# fun <- function to apply to the resulting output
# simplify <- if TRUE, print output as a matrix with m rows and nCm columns
  # else <- print output as a list 
  # nCm is the total number of combinations generated
# ... <- parameters for fun

# Helper functions for handling characters in input vector x:
# nCm <- calculate the total number of combinations 
  # taken directly from R combinat package nCm.R
  # inserted into this file saw combinat need not be installed when program is run
####################################################################################

combnsnow <- function(cls, x, m, fun = NULL, simplify = TRUE, ...) {
  
  # Input checks taken directly from the source code combn.R
  # from the CRAN combinat package
  if(length(m) > 1) {
    warning(paste("Argument m has", length(m), 
                  "elements: only the first used"))
    m <- m[1]
  }
  if(m < 0)
    stop("m < 0")
  if(m == 0)
    return(if(simplify) vector(mode(x), 0) else list())
  if(is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == x)
    x <- seq(x)
  n <- length(x)
  if(n < m)
    stop("n < m")
  
  nofun <- is.null(fun)
  count <- nCm(n, m, 0.10000000000000002)
  
  # call function to compute combinations
  retval <- mycombn(cls, x, m)

  # prepare output for formatting
  retval <- array(unlist(retval))
  
  # apply provided function
  if (!nofun) {
    retval <- sapply(retval, fun)
  }
  
  # format output
  if(!simplify) {
    mat <- matrix(retval, m, count)
    retval <- mat
    l <- list()
    for (i in 1:count) {
      l <- c(l, list(c(retval[, i])))
    }
    retval <- l
  }
  else {
    mat <- matrix(retval, m, count)
    retval <- mat
  }
  
  return(retval)
}


# Computes the indices of the next combination to generate 
# The indices then get mapped to the actual values from the input vector
next_comb <- function(comb, k, n) {
  i <- k 
  comb[i] <- comb[i] + 1
  
  while( (i >= 1) && (comb[i] >= n - k + i)) {
    i <- i - 1
    comb[i] <- comb[i] + 1
  }
  
  if(comb[1] == 1)
    return(list(comb, 0)) # exit function when no more combns to be generated
  
  for(j in (i+1):(k)) {
    if((i+1) <= k)  
      comb[j] <- comb[j-1] + 1
  }  
  
  return(list(comb,1)) #return a combination
}

# get each node's group of combs according to what is in their mychunk
# e.g. if mychunk contains 1,2, then grab all combinations that start with a 1 and 2
findmycomb <- function() { 
  mychunk <- mychunk + 1
  len <- length(mychunk) # get the number of values in mychunk
  out <- c() # store this node's found combinations
  myn <- c(n - mychunk+1) # store the lengths of the subsets this node gets
  
  # cae[[1]] contains comb[]; cae[[2]] contains the exit value
  for(i in 1:len) { 
    out <- c(out, x[cae[[1]]+mychunk[i]])
    while(1) {
      cae <- next_comb(cae[[1]], m, myn[i])
      if(cae[[2]] == 0) # if next_combn() returns 0, exit
        break;
      out <- c(out, x[cae[[1]]+mychunk[i]])
    }
    
    cae<-list(c(0:(m-1)), 1) # reset comb and exit value
    myn[i] <- myn[i] - 1 
  }
  
  return(out)
}

# using "wrap" allocation - assigning node work from front and back of input
setmychunk <- function() { 
  mychunk <<- c()
  chunkNum <- 1 
  i<-myid
  
  while(i < n-m+1) {
    mychunk <<- c(mychunk, i)
    chunkNum <- chunkNum + 1
    if(chunkNum %% 2 == 0)
      i <- i + 2 * (ncls - myid - 1)
    else
      i <- i + 2 * myid 
    
    i <- i + 1
  }
  
  return(list(mychunk)) # for printing purposes
}

# export variables to the clusters and calculates the combinations
mycombn <- function(cls, x, m) { 
  ncls <- length(cls) # number of nodes in cluster
  n <- length(x) # length of array
  comb <- c(0:(m-1)) # initialize comb 
  
  # if you have more nodes than there are groups of combinations to be assigned, need to reduce # of nodes
  # there should be at most n-m+1 nodes, one per group of combinations
  # reassigning will cause some intial lag at the start of program 
  if(n-m+1 < ncls) {
    warning(paste("Argument cls has more nodes than will be used,  
                  reassigning ", n-m+1, "nodes only"))
    cls <- makePSOCKcluster(rep("localhost", n-m+1))
    ncls <- length(cls)
  }
  
  cae <- list(comb, 1) # stores comb and exit value (1 to continue looping; 0 to exit)
  numGroups <- n-m+1 # total number of groups of combinations to find 
  
  # ship needed objects to workers
  clusterExport(cls, c("m", "n", "x", "cae", "numGroups", "setmychunk", 
                       "ncls", "next_comb", "findmycomb"), envir=environment())
  
  # set id of each node 
  setmyid <- function(i) { 
    myid <<- i 
  } 

  clusterApply(cls, 0:(ncls-1), setmyid) 
  clusterEvalQ(cls, setmychunk()) # split up the work evenly
  
  ret_chunk <- clusterEvalQ(cls, findmycomb()) 
  Reduce(c, ret_chunk)
}

####################################################################################
# Helper Function
####################################################################################

# n choose m - calculates the total number of combinations for a given input
"nCm"<-
  function(n, m, tol = 9.9999999999999984e-009)
  {
    #  DATE WRITTEN:  7 June 1995               LAST REVISED:  10 July 1995
    #  AUTHOR:  Scott Chasalow
    #
    #  DESCRIPTION: 
    #        Compute the binomial coefficient ("n choose m"),  where n is any 
    #        real number and m is any integer.  Arguments n and m may be vectors;
    #        they will be replicated as necessary to have the same length.
    #
    #        Argument tol controls rounding of results to integers.  If the
    #        difference between a value and its nearest integer is less than tol,  
    #        the value returned will be rounded to its nearest integer.  To turn
    #        off rounding, use tol = 0.  Values of tol greater than the default
    #        should be used only with great caution, unless you are certain only
    #        integer values should be returned.
    #
    #  REFERENCE: 
    #        Feller (1968) An Introduction to Probability Theory and Its 
    #        Applications, Volume I, 3rd Edition, pp 50, 63.
    #
    len <- max(length(n), length(m))
    out <- numeric(len)
    n <- rep(n, length = len)
    m <- rep(m, length = len)
    mint <- (trunc(m) == m)
    out[!mint] <- NA
    out[m == 0] <- 1  # out[mint & (m < 0 | (m > 0 & n == 0))] <-  0
    whichm <- (mint & m > 0)
    whichn <- (n < 0)
    which <- (whichm & whichn)
    if(any(which)) {
      nnow <- n[which]
      mnow <- m[which]
      out[which] <- ((-1)^mnow) * Recall(mnow - nnow - 1, mnow)
    }
    whichn <- (n > 0)
    nint <- (trunc(n) == n)
    which <- (whichm & whichn & !nint & n < m)
    if(any(which)) {
      nnow <- n[which]
      mnow <- m[which]
      foo <- function(j, nn, mm)
      {
        n <- nn[j]
        m <- mm[j]
        iseq <- seq(n - m + 1, n)
        negs <- sum(iseq < 0)
        ((-1)^negs) * exp(sum(log(abs(iseq))) - lgamma(m + 1))
      }
      out[which] <- unlist(lapply(seq(along = nnow), foo, nn = nnow, 
                                  mm = mnow))
    }
    which <- (whichm & whichn & n >= m)
    nnow <- n[which]
    mnow <- m[which]
    out[which] <- exp(lgamma(nnow + 1) - lgamma(mnow + 1) - lgamma(nnow - 
                                                                     mnow + 1))
    nna <- !is.na(out)
    outnow <- out[nna]
    rout <- round(outnow)
    smalldif <- abs(rout - outnow) < tol
    outnow[smalldif] <- rout[smalldif]
    out[nna] <- outnow
    out
  }