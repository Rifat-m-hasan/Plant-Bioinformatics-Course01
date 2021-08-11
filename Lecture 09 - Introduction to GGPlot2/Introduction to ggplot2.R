# Load the Tidyverse
library(tidyverse)


# Read the college datasheet
college <- read_csv('http://672258.youcanlearnit.net/college.csv')


# Take a look at the data
summary(college)


# Convert state, region, highest_degree, control, and gender to factors
college <- college %>%
  mutate(state = as.factor(state), 
         region = as.factor(region),
         highest_degree = as.factor(highest_degree),
         control = as.factor(control), 
         gender = as.factor(gender))


# Take a look at the data
summary(college)


# What's going on with loan_default_rate?
unique(college$loan_default_rate)


# Let's just force that to numeric and the "NULL" will convert to N/A
college <- college %>%
  mutate(loan_default_rate = as.numeric(loan_default_rate))


# Take a look at the data
summary(college)


# Calling ggplot() along just creates a blank plot
ggplot()


# I need to tell ggplot what data to use
ggplot(data=college)


# And then give it some instructions using the grammar of graphics.
# Let's build a simple scatterplot with tuition on the x-axis and average SAT score on the y axis

ggplot(data = college) +
  geom_point(mapping = aes(x = tuition, 
                           y = sat_avg))


# Let's try representing a different dimension.  
# What if we want to differentiate public vs. private schools?
# We can do this using the shape attribute

ggplot(data=college) +
  geom_point(mapping = aes(x = tuition, 
                           y = sat_avg, 
                           shape = control))



# That's hard to see the difference.  What if we try color instead?

ggplot(data=college) +
  geom_point(mapping = aes(x = tuition, 
                           y = sat_avg, 
                           color = control))

# I can also alter point size.  Let's do that to represent the number of students
ggplot(data=college) +
  geom_point(mapping=aes(x = tuition, 
                         y = sat_avg, 
                         color = control, 
                         size = undergrads))



# And, lastly, let's add some transparency so we can see through those points a bit
# Experiment with the alpha value a bit.

ggplot(data=college) +
  geom_point(mapping = aes(x = tuition, 
                           y = sat_avg, 
                           color = control, 
                           size = undergrads), 
             alpha = 1)



ggplot(data=college) +
  geom_point(mapping = aes(x = tuition, 
                           y = sat_avg, 
                           color = control, 
                           size = undergrads), 
             alpha = 1/100)



ggplot(data=college) +
  geom_point(mapping=aes(x = tuition, 
                         y = sat_avg, 
                         color = control, 
                         size = undergrads), 
             alpha = 1/3)


# What if we wanted to convert this to a line graph?
ggplot(data=college) +
  geom_line(mapping=aes(x=tuition, 
                        y=sat_avg, 
                        color=control))


# Wow, that's really noisy.  Let's add the points back in
ggplot(data=college) +
  geom_line(mapping=aes(x=tuition, 
                        y=sat_avg, 
                        color=control)) +
  geom_point(mapping=aes(x=tuition, 
                         y=sat_avg, 
                         color=control))



# I can also write this a different way
ggplot(data=college, mapping=aes(x=tuition, 
                                 y=sat_avg, 
                                 color=control)) +
  geom_line() +
  geom_point()



# I can use the geom_smooth geometry to fit a line instead of connecting every point

ggplot(data=college, mapping=aes(x=tuition, 
                                 y=sat_avg, 
                                 color=control)) +
  geom_smooth() +
  geom_point()



# Maybe add some transparency to just the points to make the line stand out more

ggplot(data=college, mapping=aes(x=tuition, 
                                 y=sat_avg, 
                                 color=control)) +
  geom_smooth() +
  geom_point(alpha=1/2)




# Try more transparency
ggplot(data=college, mapping=aes(x=tuition, 
                                 y=sat_avg, 
                                 color=control)) +
  geom_smooth() +
  geom_point(alpha=1/5)




# And remove the confidence interval from the smoother
ggplot(data=college, mapping=aes(x=tuition, 
                                 y=sat_avg, 
                                 color=control)) +
  geom_smooth(se=FALSE) +
  geom_point(alpha=1/5)



# How many schools are in each region?
# This calls for a bar graph!

ggplot(data = college) +
  geom_bar(mapping = aes(x = region))



# Break it out by public vs. private
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, 
                       color=control))



# Well, that's unsatisfying!  Try fill instead of color
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, 
                       fill=control))



# How about average tuition by region?
# First, I'll use some dplyr to create the right tibble

college %>%
  group_by(region) %>%
  summarize(average_tuition=mean(tuition))


# And I can pipe that straight into ggplot
college %>%
  group_by(region) %>%
  summarize(average_tuition = mean(tuition)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = region, 
                         y = average_tuition))



# But I need to use a column graph instead of a bar graph to specify my own y

college %>%
  group_by(region) %>%
  summarize(average_tuition=mean(tuition)) %>%
  ggplot() +
  geom_col(mapping=aes(x = region, 
                       y = average_tuition))



# Histograms can help us by binning results
ggplot(data=college) +
  geom_histogram(mapping=aes(x=undergrads), 
                 origin=0)



# What if we want fewer groups? Let's ask for 4 bins
ggplot(data=college) +
  geom_histogram(mapping=aes(x=undergrads), bins=4, origin=0)



# Or 10 bins.
ggplot(data=college) +
  geom_histogram(mapping=aes(x=undergrads), bins=10, origin=0)



# Or we can specify the width of the bins instead
ggplot(data=college) +
  geom_histogram(mapping=aes(x=undergrads), binwidth=1000, origin=0)

ggplot(data=college) +
  geom_histogram(mapping = aes(x = undergrads), 
                 bins = 50, 
                 boundary = 0)


ggplot(data=college) +
  geom_freqpoly(mapping = aes(x=undergrads))


# Let's try looking at tuition vs. institutional control
ggplot(data=college) +
  geom_point(mapping=aes(x=control, y=tuition))



# One way I could visualize this better is by adding some jitter
ggplot(data=college) +
  geom_jitter(mapping=aes(x=control, y=tuition))



# But an even better way is with a boxplot
ggplot(data=college) +
  geom_boxplot(mapping=aes(x=control, y=tuition))
































































































































