day <- "02"
dd <- glue::glue("C:/Users/Owner/Personal/Coding/Misc/R packages/Christmas 2020 puzzles/Inputs/{day}_input.txt")

pol_pass <- read.csv(dd, header = FALSE)[[1]]

## Identify valid passwords
valid_password.lgl <- get_valid_passwords(pol_pass)
valid_password <- pol_pass[valid_password.lgl]

## Number of valid passwords
sum(valid_password.lgl)



# Functions ---------------------------------------------------------------
## Identify valid passwords
## pol_pass - character vector of policy / passwords in form
##            "{min}-{max} {letter}: {password}"
## Gives logical on which pol_pass passwords are valid
get_valid_passwords <- function(pol_pass) {
  ## Extract information from pol_pass
  # Min and Max for the letter and the password
  letter <- stringr::str_extract(pol_pass, "[a-z]+")
  min_letter <- stringr::str_extract(pol_pass, "\\d")
  max_letter <- stringr::str_extract(stringr::str_extract(pol_pass, "-\\d"), "\\d")
  password <- stringr::str_extract(pol_pass, "[a-z]+$")

  ## Output logical - is the count of letter bounded by [min, max]?
  purrr::map_lgl(1:length(password),
                 function(i) {
                   ## Identify letters in password
                   password_letters <- stringr::str_split(password[i], "")[[1]]
                   ## Account for case (not needed here) where multiple letters are used e.g. "1-3 ac: qfdx"
                   letters <- stringr::str_split(letter[i], "")[[1]]
                   ## Count number of times letters occur in passwork
                   letter_count <- sum(purrr::map_dbl(letters,
                                                      function(letters) {
                                                        sum(password_letters == letters)
                                                      }))
                   ## Is the password valid?
                   if (min_letter[i] <= letter_count & letter_count <= max_letter[i]) TRUE else FALSE
                 })
}
