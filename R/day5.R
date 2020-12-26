day <- "05"
dd <- glue::glue("C:/Users/Owner/Personal/Coding/Misc/R packages/Christmas 2020 puzzles/Inputs/{day}_input.txt")

## Import input
boarding_codes <- readr::read_file(dd)
boarding_codes <- stringr::str_split(boarding_codes, "\r\n")[[1]]


# Part 1 ------------------------------------------------------------------

## Find boarding passes
boarding_passes <- find_boarding_passes(boarding_codes)

pass.id.max <- max(as.numeric(stringr::str_extract(boarding_passes, "(?<=ID )[0-9]*")))

# Part 2 ------------------------------------------------------------------
## Find IDs
ids <- as.numeric(stringr::str_extract(boarding_passes, "(?<=ID )[0-9]*"))
## Find missing seat
ids.expected <- seq(min(ids), max(ids))
ids.expected[!(ids.expected %in% ids)]

# Functions ---------------------------------------------------------------

find_boarding_passes <- function(boarding_codes) {
  purrr::map_chr(boarding_codes,
                 function(code) {
                   ## Extract row / col codes
                   code.row <- stringr::str_extract(code, ".{7}")
                   code.col <- stringr::str_extract(code, ".{3}$")
                   ## Find row / col seats
                   seat.row <- find_range_complete(c(0, 127), code.row)
                   seat.col <- find_range_complete(c(0, 7), code.col)
                   ## Calculate Seat ID
                   seat.id <- seat.row*8 + seat.col
                   ## Output boarding code
                   glue::glue("{code}: row {seat.row}, column {seat.col}, seat ID {seat.id}")
                 })
}

find_range <- function(range = c(0, 127), code = "B") {
  mid.upper <- (range[2]-range[1]+1)/2
  if (code == "B" | code == "R") {
    ## Take upper half
    c(range[1] + mid.upper, range[2])
  } else if (code == "F" | code == "L") {
    ## Take lower half
    c(range[1], range[1]+mid.upper-1)
  } else {
    stop("code must be in c('B', 'F', 'R', 'L')")
  }
}

find_range_complete <- function(range, code) {
  if (nchar(code) == 1) {
    find_range(range, code)[1]
  } else {
    code.split <- stringr::str_split(code, "")[[1]][1]
    find_range_complete(find_range(range, code.split), stringr::str_remove(code, "."))
  }
  
}
