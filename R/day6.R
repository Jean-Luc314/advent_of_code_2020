day <- "06"
dd <- glue::glue("C:/Users/Owner/Personal/Coding/Misc/R packages/Christmas 2020 puzzles/Inputs/{day}_input.txt")

## Import input
answers.everyone <- readr::read_file(dd)
answers.everyone <- stringr::str_split(answers.everyone, "\r\n\r\n")[[1]]


# Part 1 ------------------------------------------------------------------

## Answer counts
answers.everyone.concat <- stringr::str_replace_all(answers.everyone, "\r\n", "")
sum(count_answers_any(answers.everyone))


# Part 2 ------------------------------------------------------------------

answers.everyone.concat <- stringr::str_replace_all(answers.everyone, "\r\n", " ")
sum(count_answers_all(answers.everyone.concat))

# Functions ---------------------------------------------------------------

count_answers_any <- function(answers.everyone) {
  purrr::map_dbl(answers.everyone,
                 function(answers) {
                   answers.unique <- unique(stringr::str_split(answers, "")[[1]])
                   length(answers.unique)
                 })
}

count_answers_all <- function(answers.everyone) {
  purrr::map_dbl(answers.everyone,
                 function(answers) {
                   ## Split by people's answers
                   answers.split <- stringr::str_split(answers, " ")[[1]]
                   ## Map over people's answer
                   answers.lgl <- purrr::map(answers.split,
                                             function(answer) {
                                               letters %in% stringr::str_split(answer, "")[[1]]
                                             })
                   ## Identify letters that all answered yes
                   if (length(answers.lgl) == 1) {
                     letters.lgl <- answers.lgl[[1]]
                   } else {
                     letters.lgl <- Reduce(`&`, answers.lgl)
                   }
                   answers.everyone <- letters[letters.lgl]
                   ## Count answers answer by everyone
                   length(answers.everyone)
                 })
}
