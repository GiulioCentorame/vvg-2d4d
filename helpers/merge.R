# Function for merging numerics and flagging failures to match
merge.numeric = function(x) {
  # If all values are NA, return NA
  output <- ifelse(length(na.omit(x)) == 0, as.numeric(NA),
                   # otherwise, if all values are equal, merge them
                   ifelse(length(unique(na.omit(x))) == 1, unique(x),
                          # otherwise, return class-appropriate conflict code
                          -999))
  return(output)
}

# function for merging strings and flagging failures to match
merge.character = function(x) {
  # If all values are NA, return NA
  output <- ifelse(length(na.omit(x)) == 0, as.character(NA),
                   # otherwise, if all values are equal, merge them
                   ifelse(length(unique(na.omit(x))) == 1, unique(x),
                          # otherwise, return class-appropriate conflict code
                          "CONFLICT!"))
  return(output)
}

# check double-coding
dbl_code_count <- function(x) {
  group_by(x, Subject) %>% 
    summarize(n = n()) %>% 
    with(., table(n))
}

# unit tests for merge functions
stopifnot(merge.numeric(c(1, 1, 1, 1)) == 1)
stopifnot(merge.numeric(c(1, 1, 1, 3)) == -999)
stopifnot(merge.numeric(c(1, NA, NA, NA)) == 1)
stopifnot(merge.character(c(1, "blue", NA, NA)) == "CONFLICT!")
stopifnot(merge.character(c("blue", "blue", NA, NA)) == "blue")
stopifnot(merge.numeric(1) == 1)
stopifnot(merge.character("blue") == "blue")
stopifnot(class(merge.numeric(NA)) == "numeric") 
stopifnot(class(merge.numeric(c(NA, NA))) == "numeric")
stopifnot(class(merge.character(NA)) == "character")
stopifnot(class(merge.character(c(NA, NA))) == "character")

# data frame test
testframe <- data.frame(ID = c(1, 1, 2, 2, 3, 3, 4, 4),
                        str1 = c("blue", "blue", "blue", "red", "blue", NA, NA, NA ),
                        dbl1 = c(10, 10, 10, 15, 10, NA, NA, NA),
                        stringsAsFactors = F)

testframe %>% 
  group_by(ID) %>% 
  summarize_if(is.numeric, merge.numeric)

testframe %>% 
  group_by(ID) %>% 
  summarize_if(is.character, merge.character)
