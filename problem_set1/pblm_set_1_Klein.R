# Thomas Klein
# Spring 2021, Programming and Data Management, Problem Set 1

#############
# Problem 1 #
#############

# Part 1:
format_captain_ship_info <- 
  function(captain_last_name, ship_name, ship_registry) {
# printing the ship info
# using paste for ship_registry since we are expecting that to be a vector    
    paste(
      captain_last_name, "|",
      ship_name, "|", 
      paste(
        ship_registry, collapse = ' '
      ), 
      sep = ' '
      )
}

# Part 2:
final_output_vector <- c()

final_output_vector <- c(
  final_output_vector,
  format_captain_ship_info("Pike", "Enterprise", c("NCC", "1701"))
)

final_output_vector <- c(
  final_output_vector,
  format_captain_ship_info("Picard", "Enterprise", c("NCC", "1701", "D"))
)

final_output_vector <- c(
  final_output_vector,
  format_captain_ship_info("Janeway", "Voyager", c("NCC", "74656"))
)



# Part 3:
# All of this Part 3 has been coded for you.
writeLines(final_output_vector)

#############
# Problem 2 #
#############

# Part 1
# All of this Part 1 has been coded for you.
original_scores <- c(40, 20, 70, 20, 58, 68, 97, 42)

# Part 2

# creating list of scores that are lower than the max score
# and higher than the min score
original_scores_wo_min_max <- original_scores[
  original_scores < max(original_scores) & 
    original_scores > min(original_scores)
]

# creating mean of those scores
original_scores_wo_min_max_mean <- mean(original_scores_wo_min_max)

# Part 3

# determining the score adjustment factor
score_adjustment_factor <- if(original_scores_wo_min_max_mean < 50){
  1.5
} else if(original_scores_wo_min_max_mean >= 70){
  1.1
} else {
  1.2
}


# Part 4
# All of this Part 4 has been coded for you.
adjusted_original_scores <- ceiling(original_scores * score_adjustment_factor)

# Part 5

scores_df <- data.frame(
  'original_scores' = original_scores, 
  'final_scores' = adjusted_original_scores
)


mean_scores_df <- data.frame(
  'statistic' = c(
    'original scores', 
    'scores without min max', 
    'adjusted scores'
  ), 
  'mean' = c(
    mean(original_scores), 
    mean(original_scores_wo_min_max), 
    mean(adjusted_original_scores)
  )
)


# Part 6
# All of this Part 6 has been coded for you.
print(scores_df)
print(mean_scores_df)

#############
# Problem 3 #
#############

# Part 1
# All of this Part 1 has been coded for you.
original_vector <- c(9, 6, 2, 8, NA, 3, 1, 8, NA, 5)

# Part 2

my_list <- list(
  Elem_1 = original_vector, 
  Elem_2 = original_vector[!is.na(original_vector)], 
  Elem_3 = sum(original_vector[!is.na(original_vector)], na.rm = T), 
  Elem_4 = sort(unique(original_vector[!is.na(original_vector)])), 
  Elem_5 = sort(unique(original_vector[!is.na(original_vector)]))[
    sort(unique(original_vector[!is.na(original_vector)])) >= 5
  ], 
  Elem_6 = c('five', 'six', 'seven', 'eight', 'nine')[
    match(
      sort(unique(original_vector[!is.na(original_vector)]))[
        sort(unique(original_vector[!is.na(original_vector)])) >= 5
        ], 
      table = c(5:9)
    )
  ]
)



# Part 3
# All of this Part 3 has been coded for you.
my_list

#############
# Problem 4 #
#############

# Part 1
library(auk)
library(tidyverse)

ebird_taxonomy <- ebird_taxonomy



# Part 2

glimpse(ebird_taxonomy)

# Part 3

my_ebird_taxonomy <- ebird_taxonomy[c(1:1000), 'common_name']


# This last line of Part 3 has been coded for you.
head(my_ebird_taxonomy)

# Part 4

my_500th_bird <- my_ebird_taxonomy[500]

print(my_500th_bird)

# Part 5

set.seed(5)
my_ebird_sample <- my_ebird_taxonomy[sample(x = length(my_ebird_taxonomy), size = 10)] %>% 
  sort()



# not sure why I'm not getting the same results as
# the problem set PDF
print(my_ebird_sample)



# This last line of Part 5 has been coded for you.
my_ebird_sample

#############
# Problem 5 #
#############

# Part 1
# All of this Part 1 has been coded for you.
actual_sales_2020 <- c(10, 2000, 30, 5, 100, 1000, 90, 73, 22, 53, 42, 85)

# Part 2

names(actual_sales_2020) <- month.abb

for(m in 1:length(actual_sales_2020)){
  
  if(actual_sales_2020[m] > 1000 | actual_sales_2020[m] < 10){
    
    print(paste(
      names(actual_sales_2020[m]), 
      'sales amount of', 
      actual_sales_2020[m], 
      'is out of bounds. Set to NA.'
    ))
    
    actual_sales_2020[m] <- NA
  }
}

# Part 3
# All of this Part 3 has been coded for you.
sales_tib <- tibble(Month = month.abb, Actual_Sales_2020 = actual_sales_2020)
sales_tib

# Part 4

sales_tib[sales_tib$Month == 'Feb', "Actual_Sales_2020"] <- 95
sales_tib[sales_tib$Month == 'Apr', "Actual_Sales_2020"] <- 15

# Part 5

sales_tib <- sales_tib %>% 
  mutate(
    Sales_2021_Threshold = ceiling(1.15 * Actual_Sales_2020), 
    Sales_2021_Stretch = ceiling(1.35 * Actual_Sales_2020)
  )

# This last line of Part 5 has been coded for you.
View(sales_tib)

