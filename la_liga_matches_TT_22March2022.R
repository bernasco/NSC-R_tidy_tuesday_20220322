# NSC-R Tidy Tuesday March 22, 2022
# Wim Bernasco
# Franziska Yasrebi-de Kom provided fertile suggestions on code and
#    comments

# Install packages, the admin reuqired to get going 
install.packages('tidyverse')
install.packages('lubridate')
install.packages('devtools')
library(devtools)
install_github("jalapic/engsoccerdata")

# Load libraries
library(tidyverse)
library(lubridate)
library(engsoccerdata)

# engsoccerdata is a package that includes data
# Let us see what data are included (list the data in this package)
data(package="engsoccerdata")    

# If you like sports data, checkout https://statsbomb.com/ for
# free datasets

# What type of dataset is 'spain'. I hope a data.frame.
spain %>% class()
# [1] "data.frame"
# Great, it is a data.frame

# What are the names of the variables. I hope they are self-explanatory
spain %>% names()


# Let us peek into the data
spain %>% glimpse()
# You should see something like this:
# Rows: 25,435
# Columns: 12
# $ Date    <date> 1929-02-10, 1929-02-10, 1929-02-10, 1929-02-10, 1929-02-12, 1929-02-~
# $ Season  <dbl> 1928, 1928, 1928, 1928, 1928, 1928, 1928, 1928, 1928, 1928, 1928, 192~
# $ home    <chr> "Arenas de Getxo"


# This is much more intuitive, but it only works interactively,
#   Do not include the View function in your script

spain %>% View()

# Alternatively, a quick look at the first 10 rows
spain %>% 
  select(Date, home, visitor, FT) %>% 
  head(n = 10)

# Frequencies of some variables

# How many teams 
spain %>% count(home)

# Frequencies of variable-combinations

# How many times did team A host team B?
spain %>% count(home, visitor)

# Frequencies of match outcomes
spain %>% count(hgoal, vgoal)

# Same, but using the combined FT variable
spain %>% count(FT)

# Frequencies of a sum (total goals in match)
spain %>% count(hgoal + vgoal)

# Frequencies of function work as well (year of match date)
spain %>% count(year(Date))
# Note: this works because R knows that 'Date' is a date
#       How can you know that R knows?
#       Either: (1) type 'spain %>% glimpse' and observe that the
#                   class of 'Date' is a <date>
#               (2) type 'spain %>% pull(Date) %>% class()' to 
#                   obtain that information (pull returns a single 
#                   variable from a dataframe)
spain %>% pull(Date) %>% class()


# What is this 'round' variable?
spain %>% count(round)

# Apparently 90 matches ('phase2') are not regular La Liga matches

# In subsequent analyses, we will not use these 90 matches
# So we create a new dataframe excluding these 90 matches

spain_league <-
  spain %>%
  filter(round=="league")


# We will try to answer a couple of simple questions:
#   1. Is it true that there are less goals today than in earlier days?
#   2. Is the number of goals related to the season of the year?
#   3. Is playing home really an advantage?
#   4. If so, has this advantage changed over time?

# 1. Less goals today?

# Key variables, just to check we have the variables we need and 
#     they look OK. Just list the first 10 cases
spain_league %>%
  select(Date, home, visitor, hgoal, vgoal) %>%
  head(n=10)

#  Same, but this time a random sample of rows
spain_league %>%
  select(Date, home, visitor, hgoal, vgoal) %>%
  slice_sample(n=10)


# Once more, number of goals in match
spain_league %>% count(hgoal + vgoal)


# How did the number of goals per match develop over time?

spain_league %>%
  # number of goals per match, and year of the match
  mutate(goals = hgoal+vgoal,
         year  = year(Date)
  ) %>%
  ggplot() + 
  geom_point(aes(x=year, y=goals))

# Oeps, this was not what I had in mind. I need to aggregate first!

# The 'group_by' function does group the data per year, so that we can 
#    then use the 'summarize' function to obtain the mean goals scored 
#    per match (mean_goals_pm) per year.
spain_league %>%
  mutate(goals = hgoal + vgoal,
         year  = year(Date)
  ) %>%
  group_by(year) %>%
  summarize(mean_goals_pm = mean(goals)) %>%
  ggplot() + 
  geom_line(aes(x=year, y=mean_goals_pm))

# Yes, there were more goals back in the old days (before 1950)


# Funny pattern. What about England?

# Do we have the same variables (yes!)
england %>% names()

england %>%
  # number of goals per match, and year of the match
  mutate(goals = hgoal+vgoal,
         year  = year(Date)
  ) %>%
  group_by(year) %>%
  summarize(mean_goals_pm = mean(goals)) %>%
  ggplot() + 
  geom_line(aes(x=year, y=mean_goals_pm))

# England is were the game was invented (they say), so they have 
#   a longer history going back to 1888

# Let us now combine England and Spain

spain_series <-
  spain_league %>%
  mutate(goals  = hgoal+vgoal,
         year   = year(Date)
  ) %>%
  group_by(year) %>%
  summarize(mean_goals_pm = mean(goals),
            # Define a constant for Spain
            country       = "Spain")

# What des this look like?
#View(spain_series)  

england_series <-
  england %>%
  # number of goals per match, and year of the match
  mutate(goals  = hgoal+vgoal,
         year   = year(Date)
  ) %>%
  group_by(year) %>%
  summarize(mean_goals_pm = mean(goals),
            # Define a constant for England
            country       = "England")

# Stack both datasets on top of each other
 
series <- bind_rows(spain_series, england_series)

# Take a look (note that England started 1888, Spain in 1929)
series %>% 
  arrange(year, country) %>% 
  View()

# Plot development in Spain and England in the same graph
series %>%
  ggplot() +
  geom_line(aes(x=year, y=mean_goals_pm, color=country))


# Back to Spain

# 2. Is the number of goals related to the season of the year?
# We use month of the year as a season indicator

spain_league %>%
  # number of goals per match, and year of the match
  mutate(goals = hgoal+vgoal,
         month  = month(Date)
  ) %>%
  group_by(month) %>%
  summarize(mean_goals_pm = mean(goals)) %>%
  ggplot() + 
  geom_point(aes(x=month, y=mean_goals_pm)) +
  scale_x_continuous(breaks=1:12)

# Let us check how many we have in summer (June-August)


# Numbers of games
spain_league %>%
  mutate(goals = hgoal+vgoal,
         month  = month(Date)
  ) %>%
  group_by(month) %>%
  # This is how we count nr of rows (matches) per month
  summarize(number_of_matches = n()) %>%
  ggplot() + 
  geom_point(aes(x=month, y=number_of_matches)) +
  # this gives us a scale nicely labeled 1..12
  scale_x_continuous(breaks=1:12)

# Let us ignore June-August
spain_league %>%
  mutate(goals = hgoal+vgoal,
         month  = month(Date)
  ) %>%
  # this removes months 6,7 and 8 (June, July, August)
  #   the exclamation mark (!) means NOT , i.e.
  # !x means the same as (x == FALSE)  
  filter(!(month %in% c(6:8))) %>%
  group_by(month) %>%
  # This is how we count nr of rows (matches) per month
  summarize(number_of_matches = n()) %>%
  ggplot() + 
  geom_point(aes(x=month, y=number_of_matches)) +
  scale_x_continuous(breaks=1:12)

# However, the differences are pretty small. This becomes
#    clear when we set the scale of the Y axis 
spain_league %>%
  mutate(goals = hgoal+vgoal,
         month  = month(Date) ) %>%
  filter(!(month %in% c(6:8))) %>%
  group_by(month) %>%
  summarize(mean_goals_pm = mean(goals)) %>%
  ggplot() + 
  geom_point(aes(x=month, y=mean_goals_pm)) +
  scale_x_continuous(breaks=1:12) + 
  # Y axis range is between 0 and 3 goals
  ylim(0,3)

# Or as a bar chart
spain_league %>%
  mutate(goals = hgoal+vgoal,
         month  = month(Date) ) %>%
  filter(!(month %in% c(6:8))) %>%
  group_by(month) %>%
  summarize(mean_goals_pm = mean(goals)) %>%
  ggplot() + 
  # geom_col rather than geom_point
  geom_col(aes(x=month, y=mean_goals_pm)) +
  scale_x_continuous(breaks=1:12) + 
  # Y axis range is between 0 and 3 goals
  ylim(0,3)

# The differences are negligible!



# 3 Is playing home really an advantage?

# Let us create a custom version of the dataset for this analysis
spain_extended <- spain_league %>% 
  # Adding '-' in front of a variable means 'throw it away'
  #   or, in other words, 'do not select it'. If you use the -
  #   in select, variables not mentioned are retained.
  select( -Season, -HT, -FT, -tier, -round, -group, -notes ) %>%
  mutate(goals = hgoal+vgoal,
         year  = year(Date),
         month  = month(Date),
         # Continuous version : difference in goals
         goals_difference = hgoal - vgoal,
         # Discrete version: home wins, visitors win, equal split
         result_discrete = case_when(goals_difference > 0  ~ "Hosts wins",
                                     goals_difference < 0  ~ "Visitors win",
                                     goals_difference == 0 ~ "Equal split") )


# Frequency of wins, losses and equal splits
spain_extended %>% count(result_discrete)

# Intermezzo (added after the workshop meeting)

# You may be familiar with the 'classic R' table function to create
# this table (printed horizontally):
table(spain_extended$result_discrete)

# If you prefer the classic table, the below will NOT work
#   because there must always be a function after the 
#   pipe symbol (%>%) as the pipe symbol means "whatever
#   is returned by the function before %>% will become the
#   first argument of the function after the "%>%".
#  And 'result_discrete' is not a function, but an object.
spain_extended %>% result_discrete %>% table() 

# But this will work, because pull is a function that returns
#   the variable 'result_discrete' as a vector):
spain_extended %>% pull(result_discrete) %>% table() 




# Frequency table with proportions
spain_extended %>%
  group_by(result_discrete) %>%
  summarize(frequency = n()) %>%
  mutate(proportion = frequency / sum(frequency))

# Same thing, but now as a bar chart
spain_extended %>%
  group_by(result_discrete) %>%
  summarize(frequency = n()) %>%
  mutate(proportion = frequency / sum(frequency)) %>%
  ggplot() +
  geom_col(aes(x=result_discrete, y=proportion), fill="blue")

# What does goal difference home-visitors look like

# Key descriptive variables
spain_extended %>% 
  select(goals_difference) %>% 
  summary()
# Mean: On average, home teams score .78 goals more per match than visitors
# Median: In half of the matches, the home team scores more than 1 goal
#         more than the visitors

# Box plot
spain_extended %>% 
  ggplot() + 
  geom_boxplot(aes(x=goals_difference)) + 
  scale_x_continuous(breaks = seq(-10, 10,1))

# Home teams clearly have an advantage!


# 4. Has this advantage changed over time?

# Proportions of matches won, lost, equal split
spain_extended %>%
  group_by(year, result_discrete) %>%
  summarize(frequency = n()) %>%
  mutate(proportion = frequency / sum(frequency)) %>%
  ggplot() +
  geom_line(aes(x=year, y=proportion, color=result_discrete))


# Goal count differences between home team and visitors
spain_extended %>%
  group_by(year) %>%
  summarize(home_visitors_difference = mean(goals_difference)) %>%
  ggplot() +
  geom_line(aes(x=year, y=home_visitors_difference))

# Both graphs strongly suggest that home advantage decreased 
#   over the past century 






