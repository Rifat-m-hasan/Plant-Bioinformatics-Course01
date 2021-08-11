# Basic code ---------------------------

library("tidyverse")
library("readxl")
library("lubridate")

transport_data <- read_xlsx("transit-data.xlsx", 
                            sheet = "transport data", skip = 1) 

colnames(transport_data) <- tolower(make.names(colnames(transport_data)))


# Dummy data for explanation

x <- ymd("2009-08-03")
x + days(1) + hours(6) + minutes(30)
x + days(100) - hours(8)


transport_data <- transport_data %>%
  mutate(date = if_else(grepl("-", date), 
                        ymd(date), 
                        ymd("1900-01-01") + days(date)))



# Lets talk about wide vs long datasheet -----------------------------

pew <- read_csv("http://594442.youcanlearnit.net/pew.csv")

pew.long <- gather(pew, income, freq, -religion)


weather <- read_csv("http://594442.youcanlearnit.net/mexicanweather.csv")

weather.wide <- spread(weather, element, value)


data_url <- "https://goo.gl/ioc2Td"
gapminder <-read_csv(data_url)
head(gapminder)


gapminder_life <- gapminder %>% 
  select(continent,country,starts_with("life"))
head(gapminder_life)


gapminder_life %>% 
  pivot_longer(-c(continent,country), names_to = "year", values_to = "lifeExp")

gapminder_tidy %>% 
  pivot_wider(names_from = year, values_from = lifeExp)


# Coal datasheet Case Study ------------------------------

# Load the tidyverse
library(tidyverse)

# Read in the coal datasheet
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv")
glimpse(coal)

# Skip the first two lines
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv", skip=2)
glimpse (coal)

# Rename the first column as region
colnames(coal)[1] <- "region"
summary(coal)

# Convert from a wide datasheet to a long datasheet using gather
coal_long <- gather(coal, 'year', 'coal_consumption', -region)
glimpse(coal_long)

# Convert years to integers
coal_long$year <- as.integer(coal_long$year)
summary(coal_long)

# Convert coal consumption to numeric
coal_long$coal_consumption <- as.numeric(coal_long$coal_consumption)
summary(coal_long)

# Look at region values - they contain both continents and countries
unique(coal_long$region)

# Create a vector of "non country" values that appear in the region variable
noncountries <- c("North America", "Central & South America", 
                  "Antarctica", "Europe", "Eurasia", 
                  "Middle East", "Africa", "Asia & Oceania", "World")

# Look for matches
matches <- which(!is.na(match(coal_long$region, noncountries)))

# create a tibble of country values
coal_country <- coal_long[-matches,]

# create a tibble of regional values
coal_region <- coal_long[matches,]

# check them out
unique(coal_region$region)
unique(coal_country$region)








  