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
	count <- 45

	# Error checks for scheduling parameters: sched and chunksize
	# R handles error when 'sched' is not a string/character vector
	# If sched is provided, then sched must be static, dynamic, guided, or NULL
	if (!grepl('static', sched) && !grepl('dynamic', sched) && !grepl('guided', sched) && !is.null(sched)) {
			stop("Scheduling policy must be static, dynamic, or guided.")
	}
	# Set to default values
	if (is.null(sched) && is.null(chunksize)) {
		sched <- 'static'
		chunksize <- 1
	}
	else if (!is.null(sched) && is.null(chunksize)) {
		chunksize <- 1
	}
	else if (is.null(sched) && !is.null(chunksize)) { # if sched is provided, but chunk size is not
		sched <- 'static'
		warning("Value for 'sched' is replaced with default 'static' and 'chunksize' is overriden with default value.")
	}


	# Checks if input vector has characters
	# Then convert chars to their ascii decimal values
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
	
	retmat <- matrix(0, m, count)
	retmat <- .Call("combn", x, m, n, count, sched, chunksize)

	# Convert from ascii decimal values back to chars if necessary
	if (!is.na(ischarx)) {
		for (i in 1:length(retmat)) {
			if ((as.integer(retmat[i]) >= 97 && as.integer(retmat[i]) <= 122)
			|| (as.integer(retmat[i]) >= 65 && as.integer(retmat[i]) <= 90)) {
				retmat[i] <- chr(retmat[i]);
			}
		}
	}


	if (!is.null(fun)) {
		apply(retmat, 2, fun) # apply function to columns
	}

	if (simplify) {
		out <- retmat
	}
	else {
		out <- split(retmat, c(col(retmat)))
	}

	return(out)

}

# Helper functions for handling characters in input vector x
# function to check if there's a char in x
is.letter <- function(x) grepl("[[:alpha:]]", x)
# convert char to ascii decimal value
asc <- function(x) { strtoi(charToRaw(x),16L) }
# convert decimal value to ascii character
chr <- function(n) { rawToChar(as.raw(n)) }