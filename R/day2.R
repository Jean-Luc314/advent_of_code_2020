day <- "02"
dd <- glue::glue("C:/Users/Owner/Personal/Coding/Misc/R packages/Christmas 2020 puzzles/Inputs/{day}_input.txt")

pol_pass <- read.csv(dd, header = FALSE)[[1]]

# Part 1 ------------------------------------------------------------------

## Identify valid passwords
valid_password.lgl <- get_valid_passwords_part1(pol_pass)
valid_password <- pol_pass[valid_password.lgl]

## Number of valid passwords
sum(valid_password.lgl)

# Part 2 ------------------------------------------------------------------

## Identify valid passwords
valid_password.lgl <- get_valid_passwords_part2(pol_pass)
valid_password <- pol_pass[valid_password.lgl]

## Number of valid passwords
sum(valid_password.lgl)

# Functions ---------------------------------------------------------------

## Identify valid passwords - Part 1
## pol_pass - character vector of policy / passwords in form
##            "{min}-{max} {letter}: {password}"
## Gives logical on which pol_pass passwords are valid
get_valid_passwords_part1 <- function(pol_pass) {
  ## Extract information from pol_pass
  # Min and Max for the letter and the password
  letter <- stringr::str_extract(pol_pass, "[a-z]+")
  min_letter <- stringr::str_extract(pol_pass, "\\d+")
  max_letter <- stringr::str_remove(stringr::str_extract(pol_pass, "-\\d+"), "-")
  password <- stringr::str_extract(pol_pass, "[a-z]+$")

  ## Output logical - is the count of letter bounded by [min, max]?
  purrr::map_lgl(1:length(password),
                 function(i) {
                   ## Identify letters in password
                   password_letters <- stringr::str_split(password[i], "")[[1]]
                   ## Count number of times letters occur in password
                   letter_count <- sum(password_letters == letter[i])
                   ## Is the password valid?
                   if (as.numeric(min_letter[i]) <= letter_count & letter_count <= as.numeric(max_letter[i])) TRUE else FALSE
                 })
}


## Identify valid passwords - Part 2
## pol_pass - character vector of policy / passwords in form
##            "{index.1}-{index.2} {letter}: {password}"
## Gives logical on which pol_pass passwords are valid
get_valid_passwords_part2 <- function(pol_pass) {
  ## Extract information from pol_pass
  # Min and Max for the letter and the password
  letter <- stringr::str_extract(pol_pass, "[a-z]+")
  index.1 <- as.numeric(stringr::str_extract(pol_pass, "\\d+"))
  index.2 <- as.numeric(stringr::str_remove(stringr::str_extract(pol_pass, "-\\d+"), "-"))
  password <- stringr::str_extract(pol_pass, "[a-z]+$")
  
  ## Output logical - is the count of letter bounded by [min, max]?
  purrr::map_lgl(1:length(password),
                 function(i) {
                   ## Identify letters in password
                   password_letters <- stringr::str_split(password[i], "")[[1]]
                   
                   ## XoR
                   index.1.lgl <- password_letters[index.1[i]] == letter[i]
                   index.2.lgl <- password_letters[index.2[i]] == letter[i]
                   is.valid <- `%%`(index.1.lgl+index.2.lgl, 2)
                   
                   ## Is the password valid?
                   as.logical(is.valid)
                 })
}
