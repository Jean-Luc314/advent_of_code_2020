day <- "04"
dd <- glue::glue("C:/Users/Owner/Personal/Coding/Misc/R packages/Christmas 2020 puzzles/Inputs/{day}_input.txt")

## Import 03_input.txt
batch <- readr::read_file(dd)
## Split by passports
batch <- stringr::str_split(batch, "\r\n\r\n")[[1]]
## Remove \n
batch <- purrr::map_chr(batch,
                        stringr::str_replace_all,
                        "\n",
                        " ")
## Remove \r
batch <- purrr::map_chr(batch,
                        stringr::str_replace_all,
                        "\r",
                        " ")
batch <- paste0(batch, " ")
## Batch is now character vector of each passport

# Part 1 ------------------------------------------------------------------

## Find all valid passports
passports.valid <- valid_passports_part1(batch)
## Count valid passports
length(passports.valid)


# Part 2 ------------------------------------------------------------------

## Find all valid passports
passports.valid <- valid_passports_part2(batch)
## Count valid passports
length(passports.valid)

# Functions ---------------------------------------------------------------

## Find all passports - contains every fields in fields.expected
# batch - character vector of passports
# fields.expected - character vector of fields to demand be present
valid_passports_part1 <- function(batch) {
  ## Expected Fields
  fields.expected <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")
  ## Allow cid to not be required
  fields.expected <- fields.expected[fields.expected != "cid"]
  
  ## Remove Passports without all fields
  batch[filter_fields(batch, fields.expected)]
}

## Find all passports - contains every fields in fields.expected
# batch - character vector of passports
# fields.expected - character vector of fields to demand be present
valid_passports_part2 <- function(batch) {
  ## Expected Fields
  fields.expected <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")
  ## Allow cid to not be required
  fields.expected <- fields.expected[fields.expected != "cid"]
  
  ## Remove Passports without all fields
  batch <- batch[filter_fields(batch, fields.expected)]
  ## Remove fields that fail birth year test
  batch <- batch[filter_digits_range(batch, field = "byr", 1920, 2002, 4)]
  ## Remove fields that fail issue year test
  batch <- batch[filter_digits_range(batch, field = "iyr", 2010, 2020, 4)]
  ## Remove fields that fail expiration year test
  batch <- batch[filter_digits_range(batch, field = "eyr", 2020, 2030, 4)]
  ## Remove fields that fail height test
  batch <- batch[filter_hgt(batch, 150, 193, 59, 76)]
  ## Remove fields that fail hair test
  batch <- batch[filter_hcl(batch)]
  ## Remove fields that fail eye colour test
  batch <- batch[filter_ecl(batch)]
  ## Remove fields that fail passport ID test
  batch <- batch[filter_pid(batch)]
  ## Return valid passports
  batch
}

filter_fields <- function(batch, fields.expected) {
  purrr::map_lgl(batch,
                 function(batch, fields.expected) {
                   fields.found <- stringr::str_extract_all(batch, "[a-z]{3}:")[[1]]
                   fields.found <- stringr::str_remove_all(fields.found, ":")
                   ## Are all expected fields present
                   all(fields.expected %in% fields.found)
                 },
                 fields.expected)
}

filter_digits_range <- function(batch, field = "byr", minimum = 1920, maximum = 2002, digits = 4) {
  purrr::map_lgl(batch,
                 function(batch) {
                   field.chr <- stringr::str_extract(batch, glue::glue("(?<={field}:).+?(?= )"))
                   field.non_num <- stringr::str_extract(field.chr, "[^0-9]")
                   if (!is.na(field.non_num)) {
                     FALSE
                   } else {
                     field.len <- nchar(field.chr)
                     field.num <- as.numeric(field.chr)
                     ## Return logical: TRUE is conditions met; FALSE otherwise
                     field.len == digits & minimum <= field.num & field.num <= maximum
                   }
                 })
}

filter_hgt <- function(batch, cm.min = 150, cm.max = 193, in.min = 59, in.max = 76) {
  purrr::map_lgl(batch,
                 function(batch) {
                   field.hgt <- stringr::str_extract(batch, "(?<=hgt:).+?(?= )")
                   hgt.metric <- stringr::str_extract(field.hgt, "(?<=.)[a-z]{2}")
                   hgt.value <- stringr::str_remove(field.hgt, "[a-z]{2}$")
                   ## If field.hgt does not have two [a-z] at the end of string
                   if (is.na(hgt.metric)) {
                     FALSE
                   ## If string has none numeric elements
                   } else if (!is.na(stringr::str_extract(hgt.value, "[^0-9]+"))) {
                     FALSE
                   } else {
                     if (hgt.metric == "cm") {
                       cm.min <= as.numeric(hgt.value) & as.numeric(hgt.value) <= cm.max
                     } else if (hgt.metric == "in") {
                       in.min <= as.numeric(hgt.value) & as.numeric(hgt.value) <= in.max
                     } else {
                       FALSE
                     }
                   }
                 })
}

filter_hcl <- function(batch) {
  purrr::map_lgl(batch,
                 function(batch) {
                   hcl.colour <- stringr::str_extract(batch, "(?<=hcl:).+?(?= )")
                   hcl.colour.first <- stringr::str_split(hcl.colour, "")[[1]][1]
                   if (hcl.colour.first != "#") {
                     FALSE
                   } else {
                     hcl.colour.code <- stringr::str_remove(hcl.colour, "#")
                     hcl.colour.code.filter <- stringr::str_remove_all(hcl.colour.code, "[^0-9a-f]")
                     nchar(hcl.colour.code.filter) == 6
                   }
                 })
}

filter_ecl <- function(batch, ecl.allowed = c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) {
  purrr::map_lgl(batch,
                 function(batch) {
                   hcl.colour <- stringr::str_extract(batch, "(?<=ecl:).+?(?= )")
                   ## Is the colour in the allowed list?
                   hcl.colour %in% ecl.allowed
                 })
}

filter_pid <- function(batch) {
  purrr::map_lgl(batch,
                 function(batch) {
                   pid.num <- stringr::str_extract(batch, "(?<=pid:).+?(?= )")
                   nchar(pid.num) == 9
                 })
}
