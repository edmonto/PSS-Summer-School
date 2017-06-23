# Eduard Danalache
# Federal Reserve Board of Governors
# OCDO, DMID Section

#install.packages("bizdays")
library(bizdays)

# identify home directory
home.dir    <- "C:/Users/Edi/Documents/Programming/R/PSS-Summer-School/Module 1/a1_m1_2017/"

# identify auxillary directories, based off of home.dir
code.dir    <- paste0(home.dir, "code/")
data.dir    <- paste0(home.dir, "data/")
plots.dir   <- paste0(home.dir, "plots/")

# identify input and output directories, based of of data.dir
input.dir   <- paste0(data.dir, "input/")
output.dir  <- paste0(data.dir, "output/")

# load provided workspace
load(file=paste0(input.dir, "a1_m1_2017.RData"))

# initialize answers list
answers <- list()


# question 3.2
# part a
vec_types <- c(typeof(v1), typeof(v2), typeof(v3), typeof(v4))
answers$q3.2a <- vec_types

# part b
vec_lengths <- c(length(v1), length(v2), length(v3), length(v4))
vec_tails <- c(v1[length(v1)], v2[length(v2)], v3[length(v3)], v4[length(v4)])
vec_info <- c()
for (i in 1:4) {
  vec_info <- c(vec_info, vec_lengths[i], vec_tails[i], vec_types[i])
}
answers$q3.2b <- vec_info


# question 3.3
# part a
word <- c("f", "e", "d", "e", "r", "a", "l", "r", "e", "s", "e", "r", "v", "e")
vec_fed <- c()
for (i in 1:450) {
  vec_fed <- c(vec_fed, word[sample(length(word), 1)])
}
answers$q3.3a <- vec_fed

# part b
bimodal_vec <- c(rnorm(250, -5, 2), rnorm(250, 5, 2))
answers$q3.3b <- bimodal_vec

# part c
bimodal_int_vec <- as.integer(bimodal_vec)
answers$q3.3c <- bimodal_int_vec


# question 3.4
# part a
seventeenth <- l1[[17]]
answers$q3.4a <- seventeenth

# part b
boat_name <- l1$boat
answers$q3.4b <- boat_name


# question 3.5
# part a
alpha <- as.list(letters)
answers$q3.5a <- alpha

# part b
two_list <- list(five=numeric(5), multi="yellen")
answers$q3.5b <- two_list

# part c
equal_methods <- list(substring(two_list$multi, 1, 1), substring(two_list[[2]], 1, 1))
answers$q3.5c <- equal_methods


# question 3.6
# part a
dims <- c(nrow(m1), ncol(m1))
answers$q3.6a <- dims

# part b
br_corn <- m1[dims[1], dims[2]]
answers$q3.6b <- br_corn

# part c
m1_subset <- head(m1, 5)
answers$q3.6c <- m1_subset

# part d
elem_mult <- (m1 + m2) / m2 - (m1 * m2)
answers$q3.6d <- elem_mult

# part e
matrix_mult <- m1 %*% m2
answers$q3.6e <- matrix_mult


# question 3.7
# part a
rows <- nrow(df1)
answers$q3.7a <- rows

# part b
cols <- colnames(df1)
answers$q3.7b <- cols

# part c
col_a <- df1$a
answers$q3.7c <- col_a


# question 3.8
# part a
trade_log <- data.frame(date=as.Date(c("2014-01-05", "2014-05-05", "2014-08-01", "2014-10-17")),
                        sender=c("Matt", "Jake", "Lauren", "Jill"),
                        receiver=c("John", "Mike", "Joe", "Alice"),
                        parval=c(100, 350, 140.50, 1050.25),
                        bonds=c(8L, 6L, 15L, 2L))
answers$q3.8a <- trade_log

# part b
total <- c()
for (i in 1:4) {
  total <- c(total, trade_log$parval[i] * trade_log$bonds[i])
}
trade_log_improved <- trade_log
trade_log_improved$total <- total
answers$q3.8b <- trade_log_improved


# question 3.9
# part a
sample_df <- data.frame(matrix(nrow=5000, ncol=5))
answers$q3.9a <- sample_df

# part b
names(sample_df)[1] <- "date"
for (i in 1:5000) {
  sample_df$date[i] <- sample(seq(as.Date("1900-01-01"), as.Date("2016-01-01"), by="day"), 1)
}
sample_df$date <- as.Date(sample_df$date, origin="1970-01-01")
answers$q3.9b <- sample_df

# part c
names(sample_df)[2] <- "name"
for (i in 1:5000) {
  sample_df$name[i] <- paste0(letters[sample(1:26, 1)], letters[sample(1:26, 1)], letters[sample(1:26, 1)])
}
answers$q3.9c <- sample_df

# part d
names(sample_df)[3] <- "name_sum"
for (i in 1:5000) {
  sample_df$name_sum[i] <- 0
  for (j in 1:3) {
    sample_df$name_sum[i] <- sample_df$name_sum[i] + match(c(substr(sample_df$name[i], j, j)),letters)
  }
}
answers$q3.9d <- sample_df


# question 4.10
# part a
date_diff <- function(x) {
  #is.Date check from https://gist.github.com/micstr/69a64fbd0f5635094a53
  if (!(tryCatch(!is.na(as.Date(x)), error=function(err) {FALSE}))) {
    print(paste0(paste0("Error: Invalid input. ", x), " is not a date."))
    stop()
  } else {
    month <- strftime(x, format="%m")
    year <- strftime(x, format="%Y")
    first_day <- as.Date(paste0(year, "-", month, "-01"))
    cal <- create.calendar(weekdays=c("saturday", "sunday"), name="cal")
    return(-1 * bizdays(x, first_day, cal))
  }
}
answers$q4.10a <- date_diff

# part b
dist1 <- function(a, b) {
  return(sqrt((a[1] - b[1]) ** 2 + (a[2] - b[2]) ** 2))
}
answers$q4.10b <- dist1


# question 4.11
dist2 <- function(a, b, dims, method) {
  sum <- 0
  if (method == "loop") {
    for (i in 1:dims) {
      sum <- sum + (a[i] - b[i]) ** 2
    }
  } else if (method == "vectorize") {
    c <- a - b
    c <- c ** 2
    for (i in c) {
      sum <- sum + i
    }
  } else {
    print("Invalid method")
    stop()
  }
  return(sqrt(sum))
}
answers$q4.11 <- dist2


# question 4.12
sqr_mat <- function(z) {
  if (is.vector(z)) {
    if ((sqrt(length(z)) %% 1) == 0) {
      return(matrix(z, sqrt(length(z))))
    } else {
      print("Invalid argument length. Vector length is not a perfect square.")
      stop()
    }
  } else if (is.matrix(z)) {
    if (nrow(z) == ncol(z)) {
      return(z[])
    } else {
      print("Invalid argument dimensions. Input is not a square matrix.")
      stop()
    }
  } else {
    print("The argument has to be a square matrix or vector with a erfect square length.")
    stop()
  }
}
answers$q4.12 <- sqr_mat

output.file <- "EDUARD_DANALACHE_a1_m1_2017.RData"
save(answers, file=paste0(output.dir, output.file))

