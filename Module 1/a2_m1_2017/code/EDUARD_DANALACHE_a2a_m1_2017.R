# Eduard Danalache
# Federal Reserve Board of Governors
# OCDO, DMID Section

# clear environment
rm(list=ls())

# identify home directory
home.dir    <- "C:/Users/Edi/Documents/Programming/R/PSS-Summer-School/Module 1/a2a_m1_2017/"

# identify auxillary directories, based off of home.dir
code.dir    <- paste0(home.dir, "code/")
data.dir    <- paste0(home.dir, "data/")
plots.dir   <- paste0(home.dir, "plots/")

# identify input and output directories, based of of data.dir
input.dir   <- paste0(data.dir, "input/")
output.dir  <- paste0(data.dir, "output/")

# load provided workspace
load(file=paste0(input.dir, "a2a_data.Rdata"))

# initialize answers list
answers <- list()


# problem 2.1
compute_X <- function(k, year, df) {
  X <- c(0, 0, 0)  #init vector to hold T, U, P
  
  year <- 69 - (year - 1948) #1-index year to read from vector
  
  for (i in 1:3) {  #iterate for each indicator
    for (j in 0:(k[i] - 1)) {   #iterate for each year
      #modify X[i] to reflect if the indicator is increasing or decreasing
      X[i] <- X[i] + sign(c(df[[i + 2]][year + j]))[1] 
    }
  }
  
  #set X[i] to 1 or 0 if indicator is supposed to be increasing or decreasing
  for (i in 1:3) {
    if (i == 1 || i == 3) {
      X[i] <- (X[i] > 0)
    } else {
      X[i] <- (X[i] < 0)
    }
  }
  
  return(X)
}

chngs_846 <- compute_X(c(8, 4, 6), 2012, agg_econ)
answers$q2.1 <- chngs_846


# problem 2.2
mean_sq_diff <- function(expected_values, calculated_values) {
  sum <- 0
  for (i in 1:length(expected_values)) {
    sum <- sum + (calculated_values[i] - expected_values[i]) ** 2
  }
  
  return(sum)
}

min_gamma_X <- function(gammas, X, year, df) {
  year <- 69 - (year - 1948) #1-index year to read from vector
  
  G_h <- sum(gammas * X)
  G <- as.integer(df$gdp_chng[year - 1] > 1)
  
  return(mean_sq_diff(G, G_h))
}

constr_mat <- matrix(c(chngs_846[1], -1*chngs_846[1],
                       chngs_846[2], -1*chngs_846[2],
                       chngs_846[3], -1*chngs_846[3]), 2)
theta <- c(.3, .3, .3)
constr_vec <- c(0, -1)

optimum <- constrOptim(theta, min_gamma_X, NULL, constr_mat, constr_vec, X=chngs_846, year=2012, df=agg_econ)
weights_given <- optimum$par
answers$q2.2 <- weights_given


# problem 2.3
compute_all_X <- function(k, df) {
  end_year <- 68 - max(k)[1] #determine how many years we can't calculate based on largest number of years to check
  
  X_m <- data.frame(matrix(nrow=end_year, ncol=3))
  names(X_m) <- c("T", "U", "P")
  
  for (i in 1:(end_year)) {
    year <- 2016 - i #ignore 2016 (no 2017 value)
    
    X <- compute_X(k, year, df)
    
    for (j in 1:3) {
      X_m[i,][j] <- X[j]
    }
  }
  
  return(X_m)
}

X_m <- compute_all_X(c(8, 4, 6), agg_econ)
answers$q2.3 <- X_m


# problem 2.4
k_A <- c(1, 1, 1)
k_B <- c(2, 2, 3)
k_C <- c(12, 3, 4)
k_D <- c(5, 7, 9)
k_E <- c(6, 3, 4)

X_m_A <- compute_all_X(k_A, agg_econ)
X_m_B <- compute_all_X(k_B, agg_econ)
X_m_C <- compute_all_X(k_C, agg_econ)
X_m_D <- compute_all_X(k_D, agg_econ)
X_m_E <- compute_all_X(k_E, agg_econ)

calc_predictions <- function(X_m, gammas, df) {
  expected <- vector(mode="double", length=length(X_m[[1]]))
  calculated <- vector(mode="double",length=length(X_m[[1]]))
  
  for (i in 1:length(X_m[[1]])) {
    expected[i] <- as.integer(df$gdp_chng[i] < 1)
    calculated[i] <- sum(unlist(X_m[i,]) * gammas)
  }
  
  return(mean_sq_diff(expected, calculated))
}

gammas <- c(1/3, 1/3, 1/3)

pred_A <- calc_predictions(X_m_A, gammas, agg_econ)
pred_B <- calc_predictions(X_m_B, gammas, agg_econ)
pred_C <- calc_predictions(X_m_C, gammas, agg_econ)
pred_D <- calc_predictions(X_m_D, gammas, agg_econ)
pred_E <- calc_predictions(X_m_E, gammas, agg_econ)

guesses <- c(pred_A/length(unlist(X_m_A[[1]])),
             pred_B/length(unlist(X_m_B[[1]])),
             pred_C/length(unlist(X_m_C[[1]])),
             pred_D/length(unlist(X_m_D[[1]])), 
             pred_E/length(unlist(X_m_E[[1]])))
best <- which(guesses == min(guesses))

if (best == 1) {
  k_bestof5 <- k_A
} else if (best == 2) {
  k_bestof5 <- k_B
} else if (best == 3) {
  k_bestof5 <- k_C
} else if (best == 4) {
  k_bestof5 <- k_D
} else {
  k_bestof5 <- k_E
}

answers$q2.4 <- k_bestof5

output.file <- "EDUARD_DANALACHE_a2a_m1_2017.RData"
save(answers, file=paste0(output.dir, output.file))




