library(dplyr)
library(lintr)
library(styler)


data <- read.csv(file = "data/shootings-2018.csv", stringsAsFactors = FALSE)

shootings_occured <- nrow(data)

# total lives lost
total_lives_lost <- data %>%
  summarise(most_killed = sum(num_killed)) %>%
  filter(most_killed == max(most_killed)) %>%
  pull()

# total injuries
total_injuries <- data %>%
  summarise(most_injured = sum(num_injured)) %>%
  filter(most_injured == max(most_injured)) %>%
  pull()

# finds out which state has most kills in the given year
state_most_num_killed <- data %>%
  group_by(state) %>%
  summarise(most_killed = sum(num_killed)) %>%
  filter(most_killed == max(most_killed)) %>%
  pull(state)

# finds out the number of people killed in the state
num_most_num_killed <- data %>%
  group_by(state) %>%
  summarise(most_killed = sum(num_killed)) %>%
  filter(most_killed == max(most_killed)) %>%
  pull()


# finds out which state has most injuries in the given year
state_most_num_injured <- data %>%
  group_by(state) %>%
  summarise(most_injured = sum(num_injured)) %>%
  filter(most_injured == max(most_injured)) %>%
  pull(state)

# num of injuries in the state
num_most_num_injured <- data %>%
  group_by(state) %>%
  summarise(most_injured = sum(num_injured)) %>%
  filter(most_injured == max(most_injured)) %>%
  pull()

# adds new column and sums up number of deaths and injured victims
total_victims_involved <- mutate(
  data,
  total_victims = num_killed + num_injured
)

# finds the city with max sum of both death and injured victims
city_most_impacted <- total_victims_involved %>%
  group_by(city) %>%
  summarise(most_sum = sum(total_victims)) %>%
  filter(most_sum == max(most_sum)) %>%
  pull(city)

# returns the number of the sum
num_city_most_impacted <- total_victims_involved %>%
  group_by(city) %>%
  summarise(most_sum = sum(total_victims)) %>%
  filter(most_sum == max(most_sum)) %>%
  pull()

summary_table <- total_victims_involved %>%
  group_by(state) %>%
  summarise(
    Deaths = sum(num_killed),
    Injuries = sum(num_injured),
    Total_Victims = sum(total_victims)
  )
