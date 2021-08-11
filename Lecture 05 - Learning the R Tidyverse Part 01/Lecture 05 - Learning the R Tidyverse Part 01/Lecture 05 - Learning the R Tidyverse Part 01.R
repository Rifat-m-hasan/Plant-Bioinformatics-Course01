library(tidyverse)

# piping operation -----------------------------------

data <- c(1, 3, 5, 7, 11, 13, 17)

mean(diff(data^2))

data^2 %>%
  diff() %>%
  mean()

# importing data -------------------------

library("readxl")

info <- read_excel("transit-data.xlsx",
                   sheet = "info",
                   range = cell_cols("B:C"))

write_csv(info, path = "timeperiods.csv")

transport_data <- read_excel("transit-data.xlsx",
                             sheet = "transport data",
                             skip = 1)

colnames(transport_data) <- make.names(colnames(transport_data))

colnames(transport_data)

write_csv(transport_data, path = "transport_data.csv")

# Difference between DataFrame vs Tibble -----------------

baseR_dataframe <- read.csv("timeperiods.csv")

tidyverse_tibble <- read_csv("timeperiods.csv")

# Reading and writting --------------------------

timeperiods_data <- read_xlsx("data-raw/transit-data.xlsx", sheet = "info")


colnames(timeperiods_data) <- tolower(make.names(colnames(timeperiods_data)))



write_csv(timeperiods_data, "timeperiods_data.csv")


transport_data <- read_xlsx("transit-data.xlsx", 
                            sheet = "transport data", 
                            skip = 1)


colnames(transport_data) <- tolower(make.names(colnames(transport_data)))


write_csv(transport_data, "transport_data.csv")

# Lets manupulate date -------------------------------


library("lubridate")


transport_data <- read_xlsx("transit-data.xlsx", 
                            sheet = "transport data", 
                            skip = 1)


colnames(transport_data) <- tolower(make.names(colnames(transport_data)))


timeperiods_data <- read_xlsx("transit-data.xlsx", sheet = "info")


colnames(timeperiods_data) <- tolower(make.names(colnames(timeperiods_data)))

#timeperiods_data <- timeperiods_data %>%
  #mutate(period.start = ymd(paste0(period.start, "-01-01")),
         #period.end = ymd(paste0(period.end, "-12-31")))

transport_data <- transport_data %>%
  mutate(date = if_else(grepl("-", date), 
                        ymd(date), 
                        ymd("1900-01-01") + days(date))) %>%
  mutate(percent.of.all.items = 100 * number.of.items / sum(number.of.items))

transport_data <- transport_data %>%
  separate(sender.location, 
           c("sender.country", "sender.city"), 
           sep = ",", 
           extra = "merge") %>%
  separate(receiver.location, 
           c("receiver.country", "receiver.city"), 
           sep = ",", 
           extra = "merge") %>%
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


transport_data <- transport_data %>%
  filter(!complete.cases(.))


transport_data %>%
  write_csv("transport_data.csv")

# Same but different -----------------------------

transport_data <- read_xlsx("transit-data.xlsx", 
                            sheet = "transport data", skip = 1)


colnames(transport_data) <- tolower(make.names(colnames(transport_data)))


transport_data <- transport_data %>%
  mutate(date = if_else(grepl("-", date), 
                        ymd(date), 
                        ymd("1900-01-01") + days(date))) %>%
  mutate(percent.of.all.items = 100 * number.of.items / sum(number.of.items))


transport_data <- transport_data %>%
  separate(sender.location, 
           c("sender.country", "sender.city"), sep = ",", 
           extra = "merge") %>%
  separate(receiver.location, c("receiver.country", "receiver.city"), 
           sep = ",", 
           extra = "merge") %>%
  separate(receiver.city, c("receiver.city", "receiver.state"), 
           sep = "\\(", 
           fill = "right") %>%
  mutate(receiver.state = gsub("\\)", "", receiver.state)) %>%
  separate(sender.city, 
           c("sender.city", "sender.state"), 
           sep = "\\(", 
           fill = "right") %>%
  mutate(sender.state = gsub("\\)", "", sender.state))

transport_data <- transport_data %>%
  filter(!complete.cases(.)) %>% 
  group_by(sender.country, sender.state) %>%
  arrange(sender.city)






