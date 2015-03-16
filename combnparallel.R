combn <- function(x, m, fun = NULL, simplify = TRUE, ...)
{
	# input checks copied from the source code
	if(length(m) > 1) {
		warning(paste("Argument m has", length(m), 
			"elements: only the first used"))
		m <- m[1]
	}
	if(m < 0)
		stop("m < 0")
	if (m == 0)
		return(if(simplify) vector(mode(x), 0) else list())
	if(is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == x)
		x <- seq(x)
	n <- length(x)
	if(n < m)
		stop("n < m")
	nofun <- is.null(fun)

	# count is the count of the total combinations possible
	count <- nCm(n, m, 0.10000000000000002)

	# out is a list of length count with each element initialized to NULL
	out <- vector("list", count)

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


	ncls <- length(cls)

	# export variables, function, and number of clusters
	clusterExport(cls, c("x", "m", "fun", "simplify", "combnsnow", "ncls"), envir=environment())

	getchunksize <- function(i) {
		chunksize <- count/cls
		
		# chunksize needs to be at least m
		if (chunksize < m) {
			chunksize <- chunksize + (m - chunksize)
		}
	}

	# have all workers get their chunksize
	clusterApply(cls, 1:ncls, getchunksize)



}


combnsnow <- function(m, count)
{
	out <- matrix(nrow=m, ncol=count)

	
}

