day <- "03"
dd <- glue::glue("C:/Users/Owner/Personal/Coding/Misc/R packages/Christmas 2020 puzzles/Inputs/{day}_input.txt")

## Import 02_input.txt, transform into matrix
map.import <- read.csv(dd, header = FALSE)[[1]]
map.import <- stringr::str_split(map.import, "")
map.cols <- length(map.import[[1]])
map.import <- collate_nested_list(map.import)
map <- matrix(map.import, ncol = map.cols, byrow = TRUE)

# Part 1 ------------------------------------------------------------------

## Map path collisions with trees
path_collisions <- map_collisions(map, 3)
## Count tree collisions
sum(path_collisions == "X")


# Part 2 ------------------------------------------------------------------
path_collisions_1 <- map_collisions(map, 1, 1)
path_collisions_2 <- map_collisions(map, 3, 1)
path_collisions_3 <- map_collisions(map, 5, 1)
path_collisions_4 <- map_collisions(map, 7, 1)
path_collisions_5 <- map_collisions(map, 1, 2)

path_collisions_1.sum <- sum(path_collisions_1 == "X")
path_collisions_2.sum <- sum(path_collisions_2 == "X")
path_collisions_3.sum <- sum(path_collisions_3 == "X")
path_collisions_4.sum <- sum(path_collisions_4 == "X")
path_collisions_5.sum <- sum(path_collisions_5 == "X")

product <- prod(path_collisions_1.sum,path_collisions_2.sum,path_collisions_3.sum,path_collisions_4.sum,path_collisions_5.sum)

# Functions ---------------------------------------------------------------
## Periodically extend a matrix across columns
## matrix - matrix to append columns periodically
## final_width - cease appending when width reaches final_width
## col - integer column to copy
periodic_extend_col <- function(matrix, final_width, col = 1) {
  if (length(matrix[1,]) < final_width) {
    periodic_extend_col(cbind(matrix, matrix[,col]), final_width, col+1)
  } else {
    matrix
  }
}

## Map trajectory with "X" for collision with tree, "O" otherwise
## map - matrix of map
## step.horizontal - integer rightward steps for each downward step
map_collisions <- function(map, step.horizontal = 3, step.vertical = 1) {
  map_extended <- periodic_extend_col(map, length(map[,1])*step.horizontal)
  path.y <- seq(from = 1, to = length(map_extended[,1]), by = step.vertical)[-1]
  path.x <- seq(from = 1, to = length(map_extended[1,]), by = step.horizontal)[-1]
  for (i in 1:length(path.y)) {
    if (map_extended[path.y[i],path.x[i]] == ".") {
      map_extended[path.y[i], path.x[i]] <- "O"
    } else {
      map_extended[path.y[i], path.x[i]] <- "X"
    }
  }
  map_extended
}

collate_nested_list <- function(nested_list) {
  output <- c()
  for (i in 1:length(nested_list)) {
    output <- c(output, nested_list[[i]])
  }
  output
}
