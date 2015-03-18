#############################################################################
# R call function for the OpenMP parallelization of combn() from the CRAN
# combinat package: http://cran.r-project.org/web/packages/combinat/index.html

# NOTE: Output is out of order.

# Function Arguments:
# x <- input vector of integers and/or characters\
# m <- number of elements in a combination
# fun <- function to apply to the resulting output
# simplify <- if TRUE print output as a matrix with m rows and nCm columns
# where nCm is the total number of combinations generated
# ... <- parameters for fun

# Helper functions for handling characters in input vector x:
# is.letter <- # function to check if there's a char in x
# asc <- convert char to ASCII decimal value
# chr <- convert decimal value to ASCII character

#############################################################################

combn <- function(x, m, fun = NULL, simplify = TRUE, 
	sched = NULL, chunksize = NULL, ...)
{
	require(Rcpp)
	#require(combinat) # necessary for nCm in line 24
	dyn.load("combn-final.so")

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
	if (!grepl('static', sched) && !grepl('dynamic', sched) && !grepl('guided', sched) && !is.null(sched)) {
			stop("Scheduling policy must be static, dynamic, or guided.")
	}
	# Set to default values depending on what is/are provided
	if (is.null(sched) && is.null(chunksize)) {
		sched <- 'static'
		chunksize <- 1
	}
	else if (!is.null(sched) && is.null(chunksize)) {
		chunksize <- 1
	}
	else if (is.null(sched) && !is.null(chunksize)) { # if sched is provided, but chunk size is not
		sched <- 'static'
		warning("'sched' is replaced with default 'static' and 'chunksize' is overriden with default value.")
	}

	# Checks if input vector x has characters
	# If so, then convert chars to their ASCII decimal values
	# Operate on the ASCII decimal values for the chars
	ischarx <- match('TRUE', is.letter(x))
	if (!is.na(ischarx)) {
		ischarx_arr <- is.letter(x)
		for (i in 1:length(charx)) {
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


	# Calculate positions for output
	pos <- vector()
	temp_n <- n
	for (i in 1:(n-m+1)) {
		pos <- c(pos, nCm(temp_n-i, m-1))
	}
	temp <- pos[1]
	pos[1] <- 0
	for (i in 2:length(pos)) {
		temp2 <- pos[i]
		pos[i] <- temp
		temp <- pos[i] + temp2
	}

	# pos <- c(0, 29, 57, 84, 110, 135, 159, 182, 204, 225, 245, 264, 282, 299, 315, 330, 344, 357, 369, 380, 390, 399, 407, 414, 420, 425, 429, 432, 434)
	
	# Initialize output matrix
	retmat <- matrix(0, m, count)
	# Call the function through Rcpp
	retmat <- .Call("combn", x, m, n, count, sched, chunksize, pos)

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