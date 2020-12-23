## Find numbers that sum to a value, and take their product
sum_search <- 2020

numbers <- read.csv("C:/Users/Owner/Personal/Coding/Misc/R packages/Christmas 2020 puzzles/Inputs/01_input.txt", header = FALSE)[[1]]

## Create matrix mesh
numbers.bycol <- matrix(rep(numbers, length(numbers)), nrow = length(numbers))
numbers.byrow <- matrix(rep(numbers, length(numbers)), nrow = length(numbers), byrow = TRUE)

numbers.sum <- upper.tri(numbers.bycol, diag = TRUE)*(numbers.bycol + numbers.byrow)

## Vector of all sums to sum_search e.g. values_1[i]+values_2[i] == sum_search
values_1 <- numbers.bycol[numbers.sum == sum_search]
values_2 <- numbers.byrow[numbers.sum == sum_search]

## Product
product <- values_1 * values_2