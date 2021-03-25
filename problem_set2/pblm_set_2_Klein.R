# Thomas Klein
# Spring 2021, Programming and Data Management, Problem Set 2

# setting working directory 
# will need to change to upload files
# and write files to expected location
# on a different machine

setwd("/Users/tklein/Desktop/JHU_Classes/programming_data_mgmt/problem_set2")


# library
library(tidyverse)
library(rebus)
library(lubridate)

#############
# Problem 1 #
#############

# Part 1
animals_vec_1 <- c("dog", "cat", "canary", "talc", "Captain")
# TODO: add all of your code for this part here.

animals_vec_2 <- str_subset(animals_vec_1, pattern = START %R% "c")



# Part 2
captains_vec_1 <- c("Kirk", "Yeoh", "Sisko", "Pike", "Picard")
# TODO: add all of your code for this part here.

captains_vec_2 <- str_subset(
  captains_vec_1,
  pattern = START %R% ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR %R% END
  )



# Part 3
captains_vec_1 <- c("Kirk", "Yeoh", "Sisko", "Pike", "Picard")
# TODO: add all of your code for this part here.

captains_vec_3 <- captains_vec_1[str_length(captains_vec_1) == 4]



# Part 4
words_vec_1 <- c("TINY", "byway", "Heavy", "Yikes!")
# TODO: add all of your code for this part here.

words_vec_2 <- str_subset(words_vec_1, pattern = or("y", "Y") %R% END)


# Part 5
message_1 <- "My 5-digit zip code is 90001 and my birthdate is 1/01/2000"
# TODO: add all of your code for this part here.

message_2 <- str_replace(
  message_1, 
  pattern = DIGIT %R% DIGIT %R% DIGIT %R% DIGIT %R% DIGIT, 
  replacement = "XXXXX"
)



# Part 6
grades_1_fct <- factor(c("C", "D", "F", "A", "A", "B", "D","D", "C", "B"))
# TODO: add all of your code for this part here.



grades_1_fct_other <- fct_collapse(
  grades_1_fct, 
  A = "A", 
  B = "B", 
  Other = str_subset(levels(grades_1_fct), pattern = START %R% negated_char_class("A", "B") %R% END)
)




# Part 7
grades_2_fct <- factor(c("B", "C", "D", "F", "A", "A", "B", "D","D", "B", "C", "B"))
# TODO: add all of your code for this part here.


grades_2_fct_infreq <- forcats::fct_infreq(grades_2_fct)





# Part 8
make_uc_if_len_more_than_3 <- function(a_string) {
  if (str_length(a_string) > 3) {
    return(str_to_upper(a_string))
  } else {
    return(a_string)
  }
}




animals_vec_1 <- c("dog", "toad", "cat", "bird")




animals_vec_2 <- c() # Create the object to use later
for (i in 1:length(animals_vec_1)) {
  animals_vec_2 <- c(animals_vec_2, 
                     make_uc_if_len_more_than_3(animals_vec_1[i]))
}



# TODO: add all of your code for this part here.



animals_vec_3 <- sapply(
  animals_vec_1, make_uc_if_len_more_than_3
)



#############
# Problem 2 #
#############
get_uc_words <- function(post) {
  str_extract_all(post, "\\b[A-Z]{2,}\\b") %>% unlist()
}
# TODO: add all of your code for this problem here.


social_media_post <- read_csv("social_media_post.csv", col_names = F)


sm_uc_words <- lapply(
  social_media_post, 
  get_uc_words
) %>%
  unlist() %>% 
  tibble()


write_csv(sm_uc_words, col_names = F, path = "pblm_2_student_output.csv")



#############
# Problem 3 #
#############
column_names <- c("review", "email", "review_dt")
product_review_tib <- read_tsv("product_review.tsv", 
                               col_names = column_names)
# TODO: add all of your code for this problem here.


good_word_pattern <- "\\b(good|great|super|happy|like)\\b"

negative_word_pattern <- "\\b(bad|terrible|stinks|awful|cheap)\\b"



sentiment_function <- function(review){
  
  if(
    str_detect(
      tolower(review), 
      pattern = good_word_pattern
    )
  ) {return("Positive")} else{
    
    if(
      str_detect(
        tolower(review), 
        pattern = negative_word_pattern
      )
    ) {return("Negative")} else{
      return("Unknown")
    }
    
  }
  
}


product_review_tib$sentiment <- lapply(product_review_tib$review, sentiment_function) %>% unlist()


product_review_tib <- product_review_tib[, c("sentiment", "review", "email", "review_dt")]




write_tsv(
  product_review_tib, 
  "pblm_3_student_output.tsv"
)









#############
# Problem 4 #
#############
product_review_tib <- read_tsv("pblm_3_instructor_output.tsv")
product_review_tib <- product_review_tib %>% add_column(phone = "")

for (i in 1:nrow(product_review_tib)) {
  # TODO: add all of your code here.
  
  phone_values <- or(
    # phone number separated by dashes only
    DIGIT %R% DIGIT %R% DIGIT %R% UP_DASH %R% DIGIT %R% DIGIT %R% DIGIT %R% UP_DASH %R%
      DIGIT %R% DIGIT %R% DIGIT %R% DIGIT, 
    # phone number separated by dots only
    DIGIT %R% DIGIT %R% DIGIT %R% DOT %R% DIGIT %R% DIGIT %R% DIGIT %R% DOT %R% DIGIT %R% DIGIT %R% DIGIT %R% DIGIT,
    # phone number with area code in parenthesis, separated by dashes only
    OPEN_PAREN %R% DIGIT %R% DIGIT %R% DIGIT %R% CLOSE_PAREN %R% UP_DASH %R% DIGIT %R%
      DIGIT %R% DIGIT %R% UP_DASH %R% DIGIT %R% DIGIT %R% DIGIT %R% DIGIT, 
    # phone number with area code in parenthesis separated with space, other digits separated with dash
    OPEN_PAREN %R% DIGIT %R% DIGIT %R% DIGIT %R% CLOSE_PAREN %R% SPACE %R% DIGIT %R%
      DIGIT %R% DIGIT %R% UP_DASH %R% DIGIT %R% DIGIT %R% DIGIT %R% DIGIT, 
    # phone number separated by spaces only
    DIGIT %R% DIGIT %R% DIGIT %R% SPACE %R% DIGIT %R% DIGIT %R% DIGIT %R% SPACE %R%
      DIGIT %R% DIGIT %R% DIGIT %R% DIGIT
  )
  
  
  # if a phone value is detected
  # the value is extracted and transformed
  # so that it is only contains numbers separated by dots
  # and that value is put into the phone column
  # if no phone number is detected, NA gets put in phone column
  
  if(
    str_detect(
      product_review_tib[i, "review"], 
      pattern = phone_values
    )
  ){
    
    product_review_tib[i, "phone"] <- str_extract(
      product_review_tib[i, "review"], 
      pattern = phone_values
    ) %>% str_replace_all(
      pattern = or(UP_DASH, SPACE), 
      replacement = DOT
    ) %>% 
      str_replace_all(
        pattern = or(OPEN_PAREN, CLOSE_PAREN), 
        replacement = ""
      )
    
    
  } else{
    product_review_tib[i, "phone"] <- NA_character_
  }
  
  
  
  
}

readr::write_csv(product_review_tib, "pblm_4_student_output.csv")





#############
# Problem 5 #
#############

# Part 1 - do not add any code to this part
product_review_tib <- read_tsv("pblm_3_instructor_output.tsv")

# Part 2 - do not add any code to this part
product_sentiment_count_tib <- 
  tibble(sentiment = c("positive", "negative", "unknown"), count = 0)

# Part 3
# TODO: add all of your code for this part here.

# creating a table of counts based on the values
# in the sentiment column of the product_review_tib table
# and then using the sentiment column in the count tibble
# to pull those counts into the count tibble

product_sentiment_count_tib$count <- table(product_review_tib$sentiment)[
  product_sentiment_count_tib$sentiment
  ] %>% 
  as.numeric()



# Step 4
# TODO: add all of your code for this part here.

sentiment_fct <- fct_reorder(
  product_sentiment_count_tib$sentiment, product_sentiment_count_tib$count
)




# Step 5
# TODO: add all of your code for this part here.


product_sentiment_count_tib <- product_sentiment_count_tib %>% 
  arrange(sentiment_fct)




#############
# Problem 6 #
#############
product_review_tib <- read_tsv("pblm_3_instructor_output.tsv")

# Part 1
# TODO: add all of your code for this part here.


cat(
  "The review:\n",
  '"', 
  product_review_tib[[which.min(product_review_tib$review_dt), "review"]],
  '"',
  "\n", 
  "is the oldest review and was created ", 
  difftime(today(), min(product_review_tib$review_dt), units = "weeks") %>% as.numeric() %>% floor(), 
  " weeks ago on ",
  wday(min(product_review_tib$review_dt), label = T, abbr = T) %>% as.character(),
  " ",
  month(min(product_review_tib$review_dt), label = T, abbr = T) %>% as.character(), 
  " ",
  day(min(product_review_tib$review_dt)), 
  " ",
  year(min(product_review_tib$review_dt)) %>% str_sub(-2, -1),
  ".",
  sep = ""
)





# Part 2
# TODO: add all of your code for this part here.



# the function below
# takes the review date column
# converts the dates to abbreviated day labels
# collapses the days into weekends or weekday labels
# creates a named vector with counts of those labels
# sorts that vector in decreasing order (highest value first)
# gets the names of those vectors (either weekend or weekday)
# and then pulls out the first value
# this value is then pasted into the statement that prints
# this should always paste the correct value so that the statement is true
# I tested this be turning the decreasing parameter in the sort funciton on and off
# and confirmed that doing so swtiched my results


cat(
  "Most of the reviews were created on the ", 
  product_review_tib$review_dt %>% 
    wday(label = T) %>% 
    fct_collapse(
      "weekday" = c("Mon", "Tue", "Wed", "Thu", "Fri"), 
      "weekend" = c("Sun", "Sat")
    ) %>% 
    table() %>% 
    sort(decreasing = T) %>% 
    names() %>% 
    head(1), 
  ".", sep = ""
)










