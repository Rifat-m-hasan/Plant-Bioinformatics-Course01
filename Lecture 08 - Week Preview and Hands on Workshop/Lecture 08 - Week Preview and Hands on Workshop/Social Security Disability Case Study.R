# Data Wrangling in R
# Social Security Disability Case Study

# Load the tidyverse
library(tidyverse)
library(lubridate)
library(stringr)

# Read in the datasheet
ssa <- read_csv("http://594442.youcanlearnit.net/ssadisability.csv")

# Take a look at how this was imported
glimpse(ssa)

# Make the datasheet long
ssa_long <- pivot_longer(data = ssa, 
                         cols = !Fiscal_Year, 
                         names_to = 'month',
                         values_to = 'applications')

# And what do we get?
print(ssa_long, n=20)

# Split the month and application type
ssa_long <- ssa_long %>%
  separate(col = month, c("month", "application_method"), sep = "_")


# What does that look like?
print(ssa_long, n=20)


# What values do we have for months?
unique(ssa_long$month)


# Convert month to standard abbreviations
ssa_long <- ssa_long %>%
  mutate(month = substr(month, start = 1, stop = 3))


# What values do we now have for months and years?
unique(ssa_long$month)
unique(ssa_long$Fiscal_Year)


# Convert Fiscal_Year from alphanumeric strings to actual years
ssa_long <- ssa_long %>%
  mutate(Fiscal_Year = str_replace( string = Fiscal_Year, 
                                    pattern = "FY", 
                                    replacement = "20"))


# What values do we now have for years?
unique(ssa_long$Fiscal_Year)


# Build a date string using the first day of the month
paste('01', ssa_long$month, ssa_long$Fiscal_Year)

ssa_long <- ssa_long %>%
  mutate(date = dmy(paste("01", ssa_long$month, ssa$Fiscal_Year)))


# What do those look like?
unique(ssa_long$date)

glimpse(ssa_long)


# Government fiscal years differ from calendar years in that they are named for the calendar year where they end.  The government fiscal year begins in October.So October 2016 is actually in FY17.

# We need to convert these to calendar dates before we try to plot them, so we need to find months >=10 and subtract one year from them Let's find the affected rows and decerement the years by one

ssa_long <- ssa_long %>%
  mutate(Fiscal_Year = as.numeric(Fiscal_Year)) %>%
  mutate(Fiscal_Year = ifelse(test = month(date)>=10, 
                              yes = Fiscal_Year - 1,
                              no = Fiscal_Year)) %>%
  mutate(date = dmy(paste("01", month, Fiscal_Year)))


# Convert application_method to a factor
ssa_long <- ssa_long %>%
  mutate(application_method = as.factor(application_method))


# Widen the final datasheet
ssa <- pivot_wider(ssa_long, 
                   names_from = application_method, 
                   values_from = applications)

glimpse(ssa)

summary(ssa)

ssa <- ssa %>% 
  mutate( internet.use.percent = Internet*100 / Total)


ssa_complete <- ssa %>%
  filter(complete.cases(Total))


ssa_complete <- ssa_complete %>%
  select(-c(Fiscal_Year, month))

