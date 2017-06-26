# Eduard Danalache
# Federal Reserve Board of Governors
# OCDO, DMID Section

# clear environment
rm(list=ls())

# identify home directory
home.dir    <- "C:/Users/eduar/Documents/Programming/PSS-Summer-School/Module 1/a2b_m1_2017/"

# identify auxillary directories, based off of home.dir
code.dir    <- paste0(home.dir, "code/")
data.dir    <- paste0(home.dir, "data/")
plots.dir   <- paste0(home.dir, "plots/")

# identify input and output directories, based of of data.dir
input.dir   <- paste0(data.dir, "input/")
output.dir  <- paste0(data.dir, "output/")

# load provided workspace
load(file=paste0(input.dir, "a2_data.Rdata"))

# initialize answers list
answers <- list()


# problem 2.1
find_firstchar <- function(board, first_char) {
  positions <- matrix(NA, 1, 2)
  
  for (i in 1:nrow(board)) {
    for (j in 1:ncol(board)) {
      if (board[i, j] == first_char) {
        positions <- rbind(positions, matrix(c(i, j), 1, 2))
      }
    }
  }
  
  return(positions[-1,])
}

answers$find_firstchar <- find_firstchar


# problem 2.2
make_poss_coords <- function(board, previous_pos) {
  poss_pos <- matrix(c(previous_pos[1] - 1, previous_pos[1], previous_pos[1] + 1,
                       previous_pos[1] - 1, previous_pos[1] + 1,
                       previous_pos[1] - 1, previous_pos[1], previous_pos[1] + 1,
                       previous_pos[2] - 1, previous_pos[2] - 1, previous_pos[2] - 1,
                       previous_pos[2], previous_pos[2],
                       previous_pos[2] + 1, previous_pos[2] + 1, previous_pos[2] + 1), 8, 2)
  
  for (i in nrow(poss_pos):1) {
    if ((poss_pos[i, 1] < 1) | (poss_pos[i, 1] > nrow(board)) |
        (poss_pos[i, 2] < 1) | (poss_pos[i, 2] > ncol(board))) {
      poss_pos <- poss_pos[-i,]
    }
  }
  
  return(poss_pos)
}

answers$make_poss_coords <- make_poss_coords

#make_poss_coords(mtx_test, c(2, 2))


# problem 2.4
# do 2.4 before 2.3 b/c function from 2.4 help with 2.3
det_which_dir <- function(coord_1, coord_2) {
  if (coord_1[1] - coord_2[1] == 1) {
    if (coord_1[2] - coord_2[2] == 1) {
      return("up-left")
    } else if (coord_1[2] - coord_2[2] == 0) {
      return("up")
    } else if (coord_1[2] - coord_2[2] == -1) {
      return("up-right")
    }
  } else if (coord_1[1] - coord_2[1] == 0) {
    if (coord_1[2] - coord_2[2] == 1) {
      return("left")
    } else if (coord_1[2] - coord_2[2] == 0) {
      return("coordinates are the same")
    } else if (coord_1[2] - coord_2[2] == -1) {
      return("right")
    }
  } else if (coord_1[1] - coord_2[1] == -1) {
    if (coord_1[2] - coord_2[2] == 1) {
      return("down-left")
    } else if (coord_1[2] - coord_2[2] == 0) {
      return("down")
    } else if (coord_1[2] - coord_2[2] == -1) {
      return("down-right")
    }
  }
  
  return("coordinates not adjacent")
}

#TESTS
# det_which_dir(c(2, 2), c(1, 1))
# det_which_dir(c(2, 2), c(1, 2))
# det_which_dir(c(2, 2), c(1, 3))
# det_which_dir(c(2, 2), c(2, 1))
# det_which_dir(c(2, 2), c(2, 3))
# det_which_dir(c(2, 2), c(3, 1))
# det_which_dir(c(2, 2), c(3, 2))
# det_which_dir(c(2, 2), c(3, 3))
# det_which_dir(c(2, 2), c(3, 4))
# det_which_dir(c(2, 2), c(2, 2))

answers$det_which_dir <- det_which_dir


# problem 2.3
eval_coords <- function(board, poss_coords, coord_1, coord_2, next_char) {
  if (is.na(coord_2[1])) {
    valid_coords <- apply(poss_coords, MAR=1, function(x) (board[x[1], x[2]] == next_char))
  } else {
    prev_dir <- det_which_dir(coord_1, coord_2)
    
    valid_coords <- apply(poss_coords, MAR=1, function(x) (det_which_dir(x, coord_1) == prev_dir & (board[x[1], x[2]] == next_char)))
  }

  return(matrix(poss_coords[valid_coords], sum(valid_coords), 2))
}

#eval_coords(mtx_test, make_poss_coords(mtx_test, c(2, 1)), c(2, 1), c(1, 1), "O")

answers$eval_coords <- eval_coords


# problem 2.5
word_search <- function(board, word) {
  word_chars <- strsplit(word, c())
  
  first_char_locs <- find_firstchar(board, word_chars[[1]][1])
  
  if (!length(first_char_locs)) {
    return(matrix(c(-1, -1, "The word is not in the matrix"), 1, 3))
  }
  
  poss_matches <- list()
  
  if (!is.matrix(first_char_locs)) {
    first_char_locs <- matrix(first_char_locs, 1, 2)
  }
  
  for (i in 1:nrow(first_char_locs)) {
    poss_matches[[i]] <- matrix(first_char_locs[i,], 1, 2)
  }
  
  temp <- list()
  for (i in 2:length(word_chars[[1]])) {
    for (j in 1:length(poss_matches)) {
      poss_coords <- make_poss_coords(board, poss_matches[[j]][i - 1,])
      
      coord_1 <- poss_matches[[j]][i - 1,]
      coord_2 <- poss_matches[[j]][i - 2,]
      next_char <- word_chars[[1]][i]
      valid_coords <- eval_coords(board, poss_coords, coord_1, coord_2, next_char)
      
      if (!nrow(valid_coords)) {
        next
      }
      
      n_iter <- length(temp)
      for (k in (n_iter + 1):(n_iter + nrow(valid_coords))) {
        temp[[k]] <- rbind(poss_matches[[j]], valid_coords[k - n_iter,])
      }
    }
    
    poss_matches <- temp
    if (!length(poss_matches)) {
      return(matrix(c(-1, -1, "The word is not in the matrix"), 1, 3))
    }
    temp <- list()
  }
  
  results <- matrix(nrow=0, ncol=3)
  colnames(results) <- c("row", "col", "direction")
  
  for (i in 1:length(poss_matches)) {
    results <- rbind(results, c(poss_matches[[i]][1,], det_which_dir(poss_matches[[i]][1,], poss_matches[[i]][2,])))
  }
  
  return(results)
}

word_search(mtx_test, "ZZZZ")

answers$word_search <- word_search


# problem 2.6
lword_search <- function(board, words) {
  results <- list()

  for (i in 1:length(words)) {
    results[[words[i]]] <- word_search(board, words[i])
  }
  
  return(results)
}

answers$lword_search <- lword_search

output.file <- "EDUARD_DANALACHE_a2b_m1_2017.RData"
save(answers, file=paste0(output.dir, output.file))





















