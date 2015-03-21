####################################################################################
# R call function for the Thrust parallelization of combn() from the CRAN
# combinat package: http://cran.r-project.org/web/packages/combinat/index.html

# Function Arguments:
# x <- input vector of integers and/or characters
# m <- number of elements in a combination
# fun <- function to apply to the resulting output
# simplify <- if TRUE, print output as a matrix with m rows and nCm columns
  # else <- print output as a list 
  # nCm is the total number of combinations generated
# ... <- parameters for fun

# Helper functions for handling characters in input vector x:
# is.letter <- function to check if there's a char in x
# asc <- convert char to ASCII decimal value
# chr <- convert decimal value to ASCII character
# nCm <- calculate the total number of combinations 
  # taken directly from R combinat package nCm.R
  # inserted into this file saw combinat need not be installed when program is run
####################################################################################

combnthrust<- function(x, m, fun = NULL, simplify = TRUE, ...)
{
	require(Rcpp)
	#require(combinat) # necessary for nCm in line 24
	dyn.load("combnthrust.so")

	# Input checks taken directly from combn source code
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

	# Error checks for the scheduling variables: sched and chunksize
	# R handles the error when 'sched' is not a string/character vector
	
	# If sched is provided, then sched must be static, dynamic, guided, or NULL
	# if (!grepl('static', sched) && !grepl('dynamic', sched) && !grepl('guided', sched) && !is.null(sched)) {
	# 		stop("Scheduling policy must be static, dynamic, or guided.")
	# }
	# # Set to default values depending on what is/are provided
	# if (is.null(sched) && is.null(chunksize)) {
	# 	sched <- 'static'
	# 	chunksize <- 1
	# }
	# else if (!is.null(sched) && is.null(chunksize)) {
	# 	chunksize <- 1
	# }
	# else if (is.null(sched) && !is.null(chunksize)) { # if sched is provided, but chunk size is not
	# 	sched <- 'static'
	# 	warning("'sched' is replaced with default 'static' and 'chunksize' is overriden with default value.")
	# }

	# Checks if input vector x has characters
	# If so, then convert chars to their ASCII decimal values
	# Operate on the ASCII decimal values for the chars
	ischarx <- match('TRUE', is.letter(x))
	if (!is.na(ischarx)) {
		ischarx_arr <- is.letter(x)
		for (i in 1:length(ischarx_arr)) {
			if (ischarx_arr[i]) {
				if (length(asc(x[i])) == 1) {
					x[i] <- asc(x[i])
				}
				else {
					x[i] <- as.character(x[i])
				}
			}
			
		}
		x <- strtoi(x, base=10)
	}
	
	# Initialize output matrix
	retmat <- matrix(0, m, count)
	# Call the function through Rcpp
	retmat <- .Call("combnthrust", x, m, n, count)

	# Convert from ASCII decimal values back to chars if necessary
	if (!is.na(ischarx)) {
		for (i in 1:length(retmat)) {
			if ((as.integer(retmat[i]) >= 97 && as.integer(retmat[i]) <= 122)
			|| (as.integer(retmat[i]) >= 65 && as.integer(retmat[i]) <= 90)) {
				retmat[i] <- chr(retmat[i]);
			}
		}
	}

	# Apply provided function to the output
	if (!is.null(fun)) {
		apply(retmat, 2, fun(...))
	}

	# Format results
	if (simplify) {
		out <- retmat
	}
	else {
		out <- list()
		for (i in 1:ncol(retmat)) {
			out <- c(out, list(c(retmat[, i])))
		}
	}
	return(out)
}

# function to check if there's a char in x
is.letter <- function(x) grepl("[[:alpha:]]", x)
# convert char to ascii decimal value
asc <- function(x) { strtoi(charToRaw(x),16L) }
# convert decimal value to ascii character
chr <- function(n) { rawToChar(as.raw(n)) }

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
	out[m == 0] <- 1	# out[mint & (m < 0 | (m > 0 & n == 0))] <-  0
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