combn <- function(x, m, fun = NULL, simplify = TRUE, sched = NULL, chunksize = NULL, ...)
{
	require(Rcpp)
	#require(combinat) # necessary for nCm
	dyn.load("combn.so")

	# Input checks directly from the source
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
	count <- 2300
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

	# set up matrix for return
	retmat <- matrix(0, m, count)

	if (is.null(chunksize)) { # if chunksize isn't provided
		chunksize <- 1
	}
	if (is.null(sched)) { # if no scheduling policy provided
		retmat <- .Call("combn", x, m, n, count, "static", chunksize)	
	}
	else {
		retmat <- .Call("combn", x, m, n, count, sched, chunksize)
	}

	# if simplify is FALSE then need to print retmat as a list

	return(retmat)

}