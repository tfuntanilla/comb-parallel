combn <- function(x, m, fun = NULL, simplify = TRUE, sched = NULL, chunksize = NULL, ...)
{
	require(Rcpp)
	require(combinat) # necessary for nCm in line 24
	dyn.load("combn.so")

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
	out <- vector("list", count)
	a <- 1:m
	out[[1]] <- if(nofun) x[a] else fun(x[a], ...)
	if(simplify) {
		dim.use <- NULL
		if(nofun) {
			if(count > 1)
				dim.use <- c(m, count)
		}
		else {
			out1 <- out[[1]]
			d <- dim(out1)
			if(count > 1) {
				if(length(d) > 1)
				  dim.use <- c(d, count)
				else if(length(out1) > 1)
				  dim.use <- c(length(out1), count)
			}
			else if(length(d) > 1)
				dim.use <- d
		}
	}

	#retmat <- matrix(0, m, count)
	
	#charCheck <- is.letter(x)
	#if (match('TRUE', charCheck)) {
	#	isChar <- 1
	#}

	#retmat <- .Call("combn", x, m, n, count, sched, chunksize)
	#return(retmat)

	out <- .Call("combn", x, m, n, count)

	if(simplify) {
		if(is.null(dim.use))
			out <- unlist(out)
		else out <- array(unlist(out), dim.use)
	}
	
	return(out)

}

is.letter <- function(x) grepl("[[:alpha:]]", x)