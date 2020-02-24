library(dplyr)
library(lintr)
library(styler)
library(leaflet)
library(ggplot2)
library(plotly)

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

# summary table
summary_table <- total_victims_involved %>%
  group_by(state) %>%
  summarise(
    Deaths = sum(num_killed),
    Injuries = sum(num_injured),
    Total_Victims = sum(total_victims)
  )

# particular incident (Thousand Oaks)
date <- data %>% 
 filter(city == "Thousand Oaks") %>% 
  pull(date)

# address
address <-  data %>% 
  filter(city == "Thousand Oaks") %>% 
  pull(address)

# state 
state <- data %>% 
  filter(city == "Thousand Oaks") %>% 
  pull(state)

# impacted 
impacted <- total_victims_involved %>% 
  filter(city == "Thousand Oaks") %>% 
  pull(total_victims)

# number dead 
dead <- total_victims_involved %>% 
  filter(city == "Thousand Oaks") %>% 
  pull(num_killed)

# number injured
injured <- total_victims_involved %>% 
  filter(city == "Thousand Oaks") %>% 
  pull(num_injured)

# radius on each city to be sized proportionally to the impact of
# mass shootings to the city 
new_radius <- total_victims_involved %>% 
  mutate(radius = (total_victims/max(total_victims) * 10))

# creates a map with multiple features
map <- leaflet(data = new_radius) %>% 
  addTiles() %>% addCircleMarkers(
    lat = ~lat, 
    lng = ~long,
    popup = paste("City:", new_radius$city, "<br>", 
                  "Date:", new_radius$date, "<br>",
                  "Address:", new_radius$address, "<br>", 
                  "Deaths:", new_radius$num_killed, "<br>",
                  "Injuries:", new_radius$num_injured), 
    radius = ~radius, 
  )

# sorts it in different months of the year 
mass_shooting_in_months <- total_victims_involved %>% 
  mutate(months = format(as.Date(date, "%B %d", "%Y"), "%B")) %>% 
  group_by(months) %>% 
  summarise(total_impact = sum(total_victims)) %>% 
  arrange(match(months, month.name)) %>% mutate(month = 1:12) 

# creates a bar chart 
bar_chart <- ggplot(data = mass_shooting_in_months) +
  geom_col(mapping = aes(x = month, y = total_impact)) +
  ggtitle("Total Numbers of Victims (Dead + Injured) Impacted Each Month in Given Year") +
  scale_x_continuous(breaks=1:12, 
                     labels=c("Jan","Feb","Mar","Apr",
                              "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

# creates an interactive bar chart 
new_bar_chart <- ggplotly(bar_chart)

