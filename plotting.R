library(ggplot2)

################################## OPENMP ###################################
# Set the different chunk sizes
nCms <- c(4950, 44850, 161700, 551300, 1313400, 2573000, 4455100)

openmp <- c(0.005, 0.01266667, 0.01866667, 0.03866667, 0.088, 0.1453333, 0.2013333)
omp_df <- data.frame(openmp, nCms)

cran <- c(0.116, 0.2956667, 1.154, 3.547333, 9.553667, 19.692, 30.11533)
cran_df <- data.frame(cran, nCms)

thinned <- floor(seq(from=1,to=dim(omp_df)[1],length=7))
thinned2 <- floor(seq(from=1,to=dim(cran_df)[1],length=7))

# Plot
ggplot(data=omp_df, aes(x=nCms, y=time, color=Implementation)) + 
  geom_line(data=omp_df, aes(x=nCms, y=openmp, color="OpenMP")) + 
  geom_line(data=cran_df, aes(x=nCms, y=cran, color="CRAN")) + 
  geom_point(data=omp_df[thinned,], aes(x=nCms, y=openmp, color="OpenMP")) + 
  geom_point(data=cran_df[thinned2,], aes(x=nCms, y=cran, color="CRAN")) + 
  xlab("Number of Combinations") + ylab("Elapsed Time (s)") + 
  scale_y_continuous(breaks=seq(0, 30, 2))

################################## END OPENMP ###################################

################################## SNOW ###################################
# Set the different chunk sizes
nCms <- c(4950, 44850, 161700, 551300, 1313400, 2573000, 4455100)

#SNOW
d1 <- c()
df1 <- data.frame(d1, nCms)

#CRAN
d2 <- c()
df2 <- data.frame(d2, nCms)

thinned <- floor(seq(from=1,to=dim(df1)[1],length=7))
thinned2 <- floor(seq(from=1,to=dim(df2)[1],length=7))

# Plot
ggplot(data=df1, aes(x=nCms, y=time, color=Implementation)) + 
  geom_line(data=df1, aes(x=nCms, y=d1, color="Snow")) + 
  geom_line(data=df2, aes(x=nCms, y=d2, color="CRAN")) + 
  geom_point(data=df1[thinned,], aes(x=nCms, y=d1, color="Snow")) + 
  geom_point(data=df2[thinned2,], aes(x=nCms, y=d2, color="CRAN")) + 
  xlab("Number of Combinations") + ylab("Elapsed Time (s)") + 
  scale_y_continuous(breaks=seq(0, 30, 2))

################################## END SNOW ###################################


################################## All ###################################
# Set the different chunk sizes
nCms <- c(4950, 44850, 161700, 551300, 1313400, 2573000, 4455100)

#SNOW
d1 <- c()
df1 <- data.frame(d1, nCms)

#CRAN
d2 <- c()
df2 <- data.frame(d2, nCms)

#OPENMP
d3 <- c()
df3 <- data.frame(d3, nCms)

#THRUST
d4 <- c()
df4 <- data.frame(d4, nCms)

len <- length(nCms)
thinned <- floor(seq(from=1,to=dim(df1)[1],length=len))
thinned2 <- floor(seq(from=1,to=dim(df2)[1],length=len))
thinned3 <- floor(seq(from=1,to=dim(df3)[1],length=len))
thinned4 <- floor(seq(from=1,to=dim(df4)[1],length=len))

# Plot
ggplot(data=df1, aes(x=nCms, y=time, color=Implementation)) + 
  geom_line(data=df1, aes(x=nCms, y=d1, color="Snow")) + 
  geom_line(data=df2, aes(x=nCms, y=d2, color="CRAN")) + 
  geom_line(data=df3, aes(x=nCms, y=d3, color="OpenMP")) + 
  geom_line(data=df4, aes(x=nCms, y=d4, color="Thrust")) + 
  geom_point(data=df1[thinned,], aes(x=nCms, y=d1, color="Snow")) + 
  geom_point(data=df2[thinned2,], aes(x=nCms, y=d2, color="CRAN")) + 
  geom_point(data=df3[thinned3,], aes(x=nCms, y=d3, color="OpenMP")) + 
  geom_point(data=df4[thinned4,], aes(x=nCms, y=d4, color="Thrust")) + 
  xlab("Number of Combinations") + ylab("Elapsed Time (s)") + 
  scale_y_continuous(breaks=seq(0, 30, 2))

################################## END OPENMP ###################################

