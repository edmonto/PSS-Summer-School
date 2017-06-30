# Eduard Danalache
# Federal Reserve Board of Governors
# OCDO, DMID Section

library(ggplot2)
library(clue)
library(microbenchmark)

#clear workspace
rm(list=ls())

# identify home directory
home.dir    <- "C:/Users/Edi/Documents/Programming/R/PSS-Summer-School/Module 1/a4_m1_2017/"

# identify auxillary directories, based off of home.dir
code.dir    <- paste0(home.dir, "code/")
data.dir    <- paste0(home.dir, "data/")
plots.dir   <- paste0(home.dir, "plots/")

# identify input and output directories, based of of data.dir
input.dir   <- paste0(data.dir, "input/")
output.dir  <- paste0(data.dir, "output/")


# Bonus
make_mtx <- function(n) {
  m <- matrix(nrow=n, ncol=n)
  for (i in 1:n) {
    for (j in 1:n) {
      m[i, j] <- as.integer(runif(1, 1, 1000))
    }
  }
  return(m)
}

test_runtime <- function(n) {
  n_matrix <- make_mtx(n)
  solve_LSAP(n_matrix, maximum=T) #use Hungarian Algorithm function from clue library
}


df <- data.frame(n=integer(), runtime=double())
for (i in 1:8) {
  #time how long 1000 iterations of the maximization function take and average the times for each matrix size
  df[nrow(df) + 1,] <- c(i, mean(microbenchmark(test_runtime(i), times=1000)$time))
}

matrix_sum_graph <- ggplot(data=df, aes(x=n, y=runtime)) + geom_line(aes(color="Observed Times"), size=2) + 
  geom_smooth(method="lm", aes(color="Linear Model"), se=F, linetype=2) + 
  geom_smooth(method="lm", aes(color="Exponential Model"), formula=(y~exp(x)), se=F, linetype=2) +
  labs(y="Runtime (nanoseconds)", title="Matrix Size vs Runtime of Maximization") +
  theme(plot.title = element_text(hjust = 0.5))

pdf(paste0(plots.dir, "matrix_sum_bonus.pdf"))
plot(matrix_sum_graph)
dev.off()






