## Find numbers that sum to a value, and take their product
sum_search <- 2020

numbers <- read.csv("C:/Users/Owner/Personal/Coding/Misc/R packages/Christmas 2020 puzzles/Inputs/01_input.txt", header = FALSE)[[1]]

# Sum 2 numbers to sum_search ---------------------------------------------
## Create matrix mesh
numbers.bycol <- matrix(rep(numbers, length(numbers)), nrow = length(numbers))
numbers.byrow <- matrix(rep(numbers, length(numbers)), nrow = length(numbers), byrow = TRUE)
Upper_triangle <- upper.tri(numbers.bycol, diag = TRUE)

numbers.sum <- Upper_triangle*(numbers.bycol + numbers.byrow)

## Vector of all sums to sum_search e.g. values_1[i]+values_2[i] == sum_search
values_1 <- numbers.bycol[numbers.sum == sum_search]
values_2 <- numbers.byrow[numbers.sum == sum_search]

## Product
product <- values_1 * values_2


# Sum 3 numbers to sum_search ---------------------------------------------

## Create 3D arrays
numbers.3d.by_x <- array(rep(1, length(numbers)^3), rep(length(numbers), 3))
numbers.3d.by_y <- numbers.3d.by_x; numbers.3d.by_z <- numbers.3d.by_x
for (i in 1:length(numbers)) {
  numbers.3d.by_x[,,i] <- Upper_triangle*numbers.bycol
  numbers.3d.by_y[,,i] <- Upper_triangle*numbers.byrow
  numbers.3d.by_z[,,i] <- numbers[i]
}

numbers.3d.sum <- numbers.3d.by_x + numbers.3d.by_y + numbers.3d.by_z
numbers.3d.sum.sum_search <- numbers.3d.sum == sum_search

## Vector of all sums to sum_search e.g. values_1[i]+values_2[i]+values_3[i] == sum_search
values_1 <- numbers.3d.by_x[numbers.3d.sum.sum_search]
values_2 <- numbers.3d.by_y[numbers.3d.sum.sum_search]
values_3 <- numbers.3d.by_z[numbers.3d.sum.sum_search]

## Product
product <- `[[`(values_1 * values_2 * values_3, 1)
