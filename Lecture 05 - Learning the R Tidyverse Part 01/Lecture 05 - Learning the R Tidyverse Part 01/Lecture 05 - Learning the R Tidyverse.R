library("tidyverse")

transport_data <- read_csv("transport_data.csv")

load("grouped_transport_data.rdata")

grouped_transport_data

transport_data %>%
  sample_frac(0.5)

transport_data %>%
  sample_n(50)

grouped_transport_data %>%
  sample_frac(0.5)

# arranging data

transport_data %>%
  arrange(sender.location,sender.latitude) %>%
  tail()
  


# group_by() ----------------------------------------------------------------

transport_data <- transport_data %>%
  separate(receiver.location, c("receiver.country", "receiver.city"), 
           sep = ",", extra = "merge")

gdata <- transport_data %>%
  group_by(receiver.country, receiver.city) %>%
  select(date, number.of.items)

gdata <- gdata %>%
  sample_frac(0.1) %>%
  mutate(percent.item = 100*number.of.items / sum(number.of.items)) %>%
  view()

gdata <- gdata %>%
  group_by(receiver.country, receiver.city) %>%
  select(date, number.of.items) %>%
  sample_frac(0.1)

gdata %>%
  mutate(member = n()) %>%
  select(-date, -number.of.items) %>%
  unique()

gdata %>%
  n_groups()


-----------------------------------------------------------------------------

transport_data <- read_csv("C:/Users/hbshi/Desktop/R Code/Ex_Files_Learning_R_Tidyverse/Exercise Files/05_04/data/transport_data.csv")

transport_data %>%
  mutate(cum.sum.item = cumsum(number.of.items)) %>%
  view()


transport_data %>%
  arrange(date) %>%
  group_by(receiver.city) %>%
  mutate(cum.sum.item = cumsum(number.of.items)) %>%
  ggplot(aes(x=date, y = cum.sum.item, color = receiver.city)) +
  geom_line()


transport_data %>%
  arrange(date) %>%
  group_by(receiver.city) %>%
  mutate(cum.sum.item = cummean(number.of.items)) %>%
  ggplot(aes(x=date, y = cum.sum.item, color = receiver.city)) +
  geom_line()


transport_data %>%
  ggplot(aes(x = 1, y = number.of.items)) +
  geom_violin()


quantile(transport_data$number.of.items)


# cumany() function used for show or filter data after a specific point

transport_data %>%
  arrange(date) %>%
  group_by(receiver.city) %>%
  mutate(cum.sum.item = cummean(number.of.items)) %>%
  filter(cumany(number.of.items > 453)) %>%
  summarise(events.after.peak = n()) %>%
  ggplot(aes(x = receiver.city, y = events.after.peak)) +
  geom_col(fill = "red") + coord_flip()


# cumall() function used for show or filter data before a specific point

transport_data %>%
  arrange(date) %>%
  group_by(receiver.city) %>%
  mutate(cum.sum.item = cummean(number.of.items)) %>%
  filter(cumall(number.of.items < 453)) %>%
  summarise(events.before.peak = n()) %>%
  ggplot(aes(x = receiver.city, y = events.before.peak)) +
  geom_col(fill = "red") + coord_flip()


-----------------------------------------------------------------------------

  
transport_data <- read_csv("C:/Users/hbshi/Desktop/R Code/Ex_Files_Learning_R_Tidyverse/Exercise Files/05_05/data/transport_data.csv")%>%
  group_by(receiver.country, receiver.city) %>%
  select(date, number.of.items) %>%
  view()

transport_data %>%
  mutate(observation = n()) %>%
  select(-date, -number.of.items) %>%
  unique() %>%
  view()


# summary() function is a speacial function which remove every columns in the the table except the grouped columns and the column calls with in summary() function....

transport_data %>%
  summarise(observation = n()) %>%
  view()


# Dummy file to explore the power of summary() func.

dummy_data <- data_frame(
  name = c(letters, LETTERS),
  group = rep(c("one", "two", "three", "four"), each = 13),
  value = 1:52,
  normal.var = rnorm(52, mean = 100, sd = 5),
  uniform.var = runif(52, min = 1000, max = 2000),
  logical.var = sample(c(TRUE, FALSE), size = 52, replace = TRUE)
)

# summarise_all() function apply the given function to the all colunms in the dataframe

dummy_data %>%
  group_by(group) %>%
  summarise_all(mean) %>%
  view()

# summarise_if() function used for logical application an apply the function which full fill the condition which is given

dummy_data %>%
  group_by(group) %>%
  summarise_if(is.numeric, mean) %>%
  view()

--------------------------------------------------------------------------------
  
library(readr)

transport_data <- read_csv("C:/Users/hbshi/Desktop/R Code/Ex_Files_Learning_R_Tidyverse/Exercise Files/05_06/data/transport_data.csv")


by_receive_city <- transport_data %>%
  group_by(receiver.country, receiver.city) %>%
  select(date, number.of.items) 


by_receive_city %>%
  mutate(percent.of.items = {number.of.items/sum(number.of.items)} * 100)


by_receive_city %>%
  filter(cumany(number.of.items > 453))


transport_data %>%
  group_by(receiver.city,receiver.country) %>%
  mutate(received.observation = n()) %>%
  group_by(sender.country,sender.city) %>%
  mutate(sent.observation = n()) %>%
  select(received.observation, sent.observation) %>%
  unique() %>%
  ungroup() %>%
  select(received.observation) %>%
  .[[1]]


# My dummy thinking code

by_receive_city <- transport_data %>%
  group_by(receiver.country, receiver.city) %>%
  select(date, number.of.items) %>%
  summarise(observation = n()) %>%
  arrange() %>%
  ggplot(aes(x = observation, y = receiver.city)) +
  geom_col(fill = "red")


--------------------------------------------------------------------------------




  
  
  
  
  
  
  


























