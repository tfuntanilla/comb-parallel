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
	count <- 10

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

	retmat <- matrix(0, m, count)
	retmat <- .Call("combn", x, m, n, count, sched, chunksize)

	return(retmat)

}

is.letter <- function(x) grepl("[[:alpha:]]", x)