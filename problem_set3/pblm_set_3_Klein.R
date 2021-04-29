# Thomas Klein
# Spring 2021, Programming and Data Management, Problem Set 3

library(tidyverse)

################################################################################
# Problem 1
################################################################################

###############
# Part 1
###############

set.seed(5)
# TODO: add all of your code for this part here.

starwars_characters <- starwars %>% 
  sample_n(10) %>% 
  arrange(species, desc(gender)) %>% 
  select(
    name, homeworld, hair_color, eye_color, 
    starts_with("s")
  )

###############
# Part 2
###############
# TODO: add all of your code for this part here.


avg_height <- starwars %>% 
  filter(species %in% c("Droid", "Human")) %>% 
  group_by(species) %>% 
  summarize(height_mean = mean(height, na.rm = T)) %>% 
  ungroup()


###############
# Part 3
###############
# TODO: add all of your code for this part here.

droid_avg_height <- starwars %>% 
  filter(species == "Droid") %>% 
  summarize(height_mean = mean(height, na.rm = T)) %>% 
  mutate(species = "Droid") %>% 
  select(species, height_mean)

human_avg_height <- starwars %>% 
  filter(species == "Human") %>% 
  summarize(height_mean = mean(height, na.rm = T)) %>% 
  mutate(species = "Human") %>% 
  select(species, height_mean)


avg_height <- bind_rows(droid_avg_height, human_avg_height)



###############
# Part 4
###############
# TODO: add all of your code for this part here.


starwars_bmi <- starwars %>% 
  mutate(bmi = mass / ((height / 100)^2)) %>% 
  filter(!is.na(bmi)) %>% 
  select(name, bmi) %>% 
  arrange(desc(bmi)) %>% 
  top_n(5, bmi)

###############
# Part 5
###############
# TODO: add all of your code for this part here.


starwars %>% 
  pull(species) %>% 
  table()

###############
# Part 6
###############
# TODO: add all of your code for this part here.

starwars %>% 
  count(species) %>% 
  print(n = 38)


###############
# Part 7
###############
# TODO: add all of your code for this part here.

starwars %>% 
  group_by(species) %>% 
  tally() %>% 
  ungroup() %>% 
  print(n = 38)


###############
# Part 8
###############
# TODO: add all of your code for this part here.


starwars_mass_height <- starwars %>% 
  select(species, height, mass) %>% 
  na.omit() %>% 
  group_by(species) %>% 
  summarize(
    avg_mass = mean(mass), 
    avg_height = mean(height)
  )


starwars_mass_height %>% 
  print(n = nrow(starwars_mass_height))


###############
# Part 9
###############
# TODO: add all of your code for this part here.


short_med_tall <- starwars %>% 
  filter(
    species == "Human" & !is.na(height)
  ) %>% 
  select(name, height) %>% 
  mutate(
    height = if_else(
      height < 171, "short", 
      if_else(height >= 190, "tall", "medium")
    )
  ) %>% 
  arrange(name)


short_med_tall %>% 
  print(n = nrow(short_med_tall))
  
  

###############
# Part 10
###############
# TODO: add all of your code for this part here.


tallest_3 <- starwars %>%
  filter(
    species %in% c("Droid", "Gungan", "Human") & 
      !is.na(height)
  ) %>% 
  select(name, species, height) %>% 
  arrange(species, desc(height)) %>% 
  group_by(species) %>% 
  top_n(3, height) %>% 
  ungroup()


###############
# Part 11
###############

# Step 1
wage <- c(14, 12, 18, 20, 15)

# Step 2
# TODO: add all of your code for this step here.

wage_new <- map_dbl(wage, ~ .x * 1.2)

wage_new

###############
# Part 12
###############

# Step 1
dep <- tibble(num = c(1:6),
              name = c("Accounting", "Compliance", "Logistics", 
                       "Planning", "Sales", "Service"))

emp <- tibble(name = c("Ann", "Art",  "Bob", "Dan", "Fay", "Jon", "Liz", "Mia"),
              dep_num = c(3, 6, 5, 1, 6, 3, 6, 1))

# Step 2
# TODO: add all of your code for this step here.

dep_with_no_emp <- anti_join(
  dep, emp, by = c("num" = "dep_num")
) %>% 
  rename(
    dep_num = num, 
    dep_name = name
  )


###############
# Part 13
###############

# Step 1
contact_info <- tibble(
  department = c("Sales", "Service", "Finance"),
  phone = c("111-222-3333", "444.555.6666", "777 888 9999")
)

# Step 2
# TODO: add all of your code for this step here.

contact_info_new <- contact_info %>% 
  mutate(
    area_code = str_sub(phone, 1, 3), 
    exchange = str_sub(phone, 5, 7), 
    number = str_sub(phone, 9, 12)
  ) %>% 
  select(-phone)

###############
# Part 14
###############

# Step 1
set.seed(8)
star_wars_df <- starwars %>% select(species, name, hair_color) %>% 
  filter(species %in% c("Human", "Droid")) %>% sample_frac(0.50) %>% 
  mutate(hair_color = case_when(is.na(hair_color) ~ "none",
                                !is.na(hair_color) ~ hair_color)) %>% 
  arrange(name)
star_wars_df

# Step 2
star_trek_df <- tibble(
  species = c("Human", "Klingon", "Droid", "Vulcan"),
  name = c("Kirk", "Worf", "Data", "Spock"),
  hair_color = c("brown", "brown", "black", "black")
)

# Step 3
# TODO: add all of your code for this step here.


star_wars_trek <- bind_rows(star_wars_df, star_trek_df) %>% 
  arrange(hair_color, species, name)

star_wars_trek %>% 
  print(n = nrow(star_wars_trek))

################################################################################
# Problem 2
################################################################################

# Part 1
score_wide <- tibble(
  student = c("Ann", "Bob", "Bob", "Sue", "Tom"),
  exam_1 = c("60 pts", "20 pts", "20 pts", "40 pts", "50 pts"),
  exam_2 = c("65 pts", "30 pts", "30 pts", "50 pts", "50 pts"),
  exam_3 = c("70 pts", "30 pts", "30 pts", "50 pts", "50 pts"),
  exam_4 = c("85 pts", "40 pts", "40 pts", "40 pts", "70 pts")
)

# Part 2
# TODO: add all of your code for this part here.

score_wide_duplicate_row <- score_wide %>% 
  filter(duplicated(score_wide))



# Part 3
# TODO: add all of your code for this part here.

score_long <- score_wide %>% 
  distinct() %>% 
  pivot_longer(starts_with("ex"), names_to = "exam", values_to = "score") %>% 
  mutate(
    score = str_sub(score, 1, 2) %>% as.numeric(),
    score = if_else(score < 50, 50, score)
    ) %>% 
  arrange(exam, desc(score), student)


score_long %>% 
  print(n = nrow(score_long))


# Part 4
# TODO: add all of your code for this part here.


summary(score_long$score)

################################################################################
# Problem 3
################################################################################

# Part 1
student_score <- tibble(
  name = c("sal", "ann", "bob", "dan", "sue", "tom", "art"),
  original_score = c(70, 55, 85, 60, 85, 45, 40),
  adjustment_factor = c(1.2, 1.2, 1.1, 1.2, 1.1, 1.3, 1.3)
)

# Part 2
# TODO: add all of your code for this part here.

final_score <- map2_dbl(
  student_score$original_score, student_score$adjustment_factor, 
  ~ .x * .y
)

student_score <- cbind(student_score, final_score)

print(student_score, nrow(student_score))

################################################################################
# Problem 4
################################################################################

# Part 1
dept <- tibble(dept_cd = c("AC", "CM", "LO", "PL", "SA", "SV", "Sc"),
               dept_name = c("Accounting", "Compliance", "Logistics", 
                             "Planning", "Sales", "Service", "Security"))

empl_basic <- tibble(empl_num = c(100, 301, 197, 123, 250, 276, 139, 173),
                     empl_name = c("Ann", "Art",  "Bob", "Dan", "Fay", "Jon", "Liz", "Mia"),
                     dept_cd = c("AC", "CM", "LO", "PL", "SA", "SV", "AC", "LO"))

empl_income <-   tibble(empl_num = c(100, 301, 197, 123, 250, 276, 139, 173),
                        income_2019 = c(73, 60, 90, 100, 85, 65, 95, 110),
                        income_2020 = c(73, 100, 90, 98, 85, 69, 99, 110))

# Part 2
all_tables <- empl_basic %>% inner_join(dept, by = "dept_cd") %>% 
  inner_join(empl_income, by = "empl_num")

# Part 3
# TODO: add all of your code for this part here.

all_tables %>% 
  count(dept_name)

# Part 4
# TODO: add all of your code for this part here.

full_join(dept, empl_basic, by = "dept_cd") %>% 
  filter(is.na(empl_num)) %>% 
  select(dept_name)

# Part 5
all_tables_dept_summaries <- all_tables %>% group_by(dept_name) %>% 
  summarise(avg_income_2019 = mean(income_2019),
            avg_income_2020 = mean(income_2020)) %>% 
  mutate(avg_income_difference = avg_income_2020 - avg_income_2019) %>% 
  arrange(desc(avg_income_difference))

# Part 6
# TODO: add all of your code for this part here.


high_avg_inc_dep <- all_tables_dept_summaries %>% 
  filter(avg_income_difference == max(avg_income_difference)) %>% 
  pull(dept_name)

str_interp(
  "The ${high_avg_inc_dep} department had the highest average income increase."
)

################################################################################
# Problem 5
################################################################################

# Part 1
student_info <- tibble(email =  c("jon@xyz.edu", "michael@abcd.org",
                                  "peggy@jlkmn.com", "jeff@stuvwx.io"),
                       introduction = c(
                         "I am a 25 year old freshman.",
                         "I'm a sophomore who is 20.",
                         "Junior is my class. I am a 23 years old",
                         "My age is 30 and I am a senior."
                       ))

# Part 2
# TODO: add all of your code for this part here.

student_name_vec <- student_info %>% 
  separate(email, into = c("first_name", "server"), sep = "@") %>% 
  mutate(first_name = str_to_title(first_name)) %>% 
  pull(first_name)

student_name_vec

# Part 3
class_word_regex_pattern <- "\\b(freshman|sophomore|junior|senior)\\b"

for (i in 1:nrow(student_info)) {
  if (i == 1) {
    student_age_vec <- str_extract(student_info$introduction[i], "\\d{2}") 
    student_class_vec <- str_extract(student_info$introduction[i],
                                     regex(class_word_regex_pattern, ignore_case = TRUE))
  } else {
    student_age_vec <- c(student_age_vec, 
                         str_extract(student_info$introduction[i], "\\d{2}"))
    this_student_class <- str_extract(student_info$introduction[i], 
                                      regex(class_word_regex_pattern, ignore_case = TRUE))
    # student_class_vec <- c(student_class_vec, this_student_class)
    student_class_vec <- str_to_lower(c(student_class_vec, this_student_class))
  }
}

# Part 4
# TODO: add all of your code for this part here.

student <- tibble(
  name = student_name_vec, 
  age = student_age_vec, 
  class = student_class_vec
)

student

# Part 5
student_introduction_message <- student %>% mutate(student_intro_message = 
                         pmap_chr(list(student$name, student$age, student$class), 
                         function(x, y, z) paste0(x, " is a ", y, " years old ", z,"."))) %>% 
  select(student_intro_message)

student_introduction_message


