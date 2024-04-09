library(tidyverse)

# we'll be looking at data on Groundhog predictions
groundhogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/groundhogs.csv')
predictions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/predictions.csv')

head(predictions)

# find groundhog predictions from 2020
filter(predictions, year == 2020)

# find groundhog predictions from 2020 and 2021
filter(predictions, year == 2020 | year == 2021)
filter(predictions, year %in% c(2020, 2021))

# find predictions between 1900 and 2000
predictions |>
  filter(year >= 1900 & year <= 2000)

# create a subset of your data where "shadow" has a value of either TRUE or FALSE. Make sure there are no duplicate rows, and sort the result by descending year.
predictions <- predictions |>
  filter(shadow %in% c(TRUE, FALSE)) |>
  distinct(year, id, .keep_all = TRUE) |>
  arrange(desc(year))

# group predictions by year 
predictions |>
  group_by(year)

# how many predictions were made in each year?
predictions |>
  group_by(year) |>
  summarize(n_predictions = n())

# How many different groundhogs made predictions each year?
predictions |>
  group_by(year) |>
  summarize(n_groundhogs = n_distinct(id)) |>
  arrange(desc(n_groundhogs))

# What is the first year each groundhog made a prediction?
predictions |>
  group_by(id) |>
  summarize(first_prediction = min(year))

# Let's return to our dataframe with the number of predictions in each year. 
# How would we add a column for the number of shadows seen in each year?
predictions |>
  group_by(year) |>
  summarize(n_predictions = n(),
            n_shadows = sum(shadow == TRUE)) 

# Create a dataframe with 3 variables: 
# groundhog id
# the number of total predictions each groundhog has made
# the number of times each groundhog has seen its shadow
predictions |>
  group_by(id) |>
  summarize(n_predictions = n(),
            n_shadows = sum(shadow == TRUE))

# calculate how many characters are in the details field and put the variable after id
predictions |>
  mutate(details_length = nchar(details), .after = id)

# create a column that indicates whether the prediction was made by Punxatawney Phil
predictions |> 
  mutate(phil = if_else(id == 1, 'TRUE', 'FALSE'))

# create a column that indicates the century of the predictions
predictions |> 
  mutate(century = case_when(year < 1900 ~ 19,
                             year < 2000 & year >= 1900 ~ 20,
                             year >= 2000 ~ 21))

# Working off of our table with the number of predictions and number of shadows seen per groundhog, lets:
# Add a column called shadow_percent that gives the percentage of time each groundhog sees its shadow
# Filter for groundhogs with more than 5 predictions
# Keep only the variables id and shadow_percent, and rename id to groundhog_id 
# Assign the result to a variable groundhog_predictions 
groundhog_predictions <- predictions |>
  group_by(id) |>
  summarize(n_predictions = n(),
            n_shadows = sum(shadow == TRUE)) |>
  mutate(shadow_percent = n_shadows/n_predictions) |>
  filter(n_predictions > 5) |>
  select(id, shadow_percent) |>
  rename(groundhog_id = id)

# add the variables from groundhogs to our groundhog_predictions table
left_join(groundhog_predictions, groundhogs, join_by(groundhog_id == id))

# add the variables from groundhog_predictions to the groundhogs table 
right_join(groundhog_predictions, groundhogs, join_by(groundhog_id == id))
left_join(groundhogs, groundhog_predictions, join_by(id == groundhog_id))

# add variables from groundhogs to groundhog_predictions where keys appear in both tables
inner_join(groundhog_predictions, groundhogs, join_by(groundhog_id == id))

# add variables from groundhogs to groundhog_predictions. Add rows even if the groundhog isn't in groundhog_predictions
full_join(groundhog_predictions, groundhogs, join_by(groundhog_id == id))


  
