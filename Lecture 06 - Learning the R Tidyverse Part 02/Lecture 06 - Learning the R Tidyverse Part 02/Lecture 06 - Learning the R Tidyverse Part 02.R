library(tidyverse)
library(readxl)
library(Rcpp)
library(lubridate)

# importing data -------------------------

info <- read_excel("transit-data.xlsx",
                   sheet = "info",
                   range = cell_cols("B:C"))

write_csv(info, path = "timeperiods.csv")


# Reading and writting --------------------------

timeperiods_data <- read_xlsx("transit-data.xlsx", sheet = "info")

colnames(timeperiods_data) <- tolower(make.names(colnames(timeperiods_data)))



write_csv(timeperiods_data, "timeperiods_data.csv")


transport_data <- read_xlsx("transit-data.xlsx", 
                            sheet = "transport data", 
                            skip = 1)


colnames(transport_data) <- tolower(make.names(colnames(transport_data)))


# write_csv(transport_data, "transport_data.csv")

# Lets manipulate date -------------------------------

timeperiods_data <- read_xlsx("transit-data.xlsx", sheet = "info")


colnames(timeperiods_data) <- timeperiods_data |>
  colnames() |>
  make.names() |>
  tolower()


# Date formatting
# Mutate:: it is used to make new column in the data frame

timeperiods_data <- timeperiods_data %>%
  mutate(period.start = ymd(paste0(period.start, "-01-01")),
         period.end = ymd(paste0(period.end, "-12-31")))


# Separate:: it is used to separate columns

transport_data <- read_xlsx("transit-data.xlsx", 
                            sheet = "transport data", 
                            skip = 1)


colnames(transport_data) <- transport_data |>
  colnames() |>
  make.names() |>
  tolower()


transport_data %>%
  separate(sender.location, 
           c("sender.country", "sender.city"), 
           sep = ",")


transport_data |>
  select(sender.location) |>
  slice(c(316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 
          327, 328, 329, 330, 331))


transport_data <- transport_data %>%
  separate(sender.location, 
           c("sender.country", "sender.city"), 
           sep = ",", 
           extra = "merge")


transport_data <- transport_data %>%
  separate(receiver.location, 
           c("receiver.country", "receiver.city"), 
           sep = ",", 
           extra = "merge")


transport_data <- transport_data %>%
  separate(receiver.city, 
           c("receiver.city", "receiver.state"), 
           sep = "\\(", 
           fill = "right") %>%
  mutate(receiver.state = gsub("\\)", "", receiver.state)) %>%
  separate(sender.city, 
           c("sender.city", "sender.state"), 
           sep = "\\(", 
           fill = "right") %>%
  mutate(sender.state = gsub("\\)", "", sender.state))


# Lets deal with missing values


transport_data %>%
  filter(is.na(sender.state))

transport_data <- transport_data %>%
  filter(complete.cases(.))


# Group_by() -------------------------------

transport_data %>%
  filter(complete.cases(.)) %>% 
  group_by(sender.country, sender.state) %>%
  arrange(sender.city) 


transport_data %>%
  group_by(receiver.country, receiver.city) %>%
  select(date, number.of.items) |>
  mutate(percent = 100* number.of.items / sum(number.of.items)) |>
  mutate(cheack = sum(percent)) |>
  sample_frac(0.1) |>
  View()

# Let's code less for same result

transport_data |>
  group_by(sender.country, sender.city) |>
  select(sender.country, sender.city) |>
  mutate(number = n()) |>
  unique() |>
  View()


transport_data |>
  group_by(sender.country, sender.city) |>
  summarise(Obs = n()) |>
  view()











