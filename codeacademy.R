
#============================ 
# Title: codeacademy R
# By: Zane Dax
# Date: May 28, 2021
#===========================

#------- adding a column to df using mutate()

#-- add column with sales tax
# df %>%
#   mutate(sales_tax = price * 0.075)

# df_avg_ht = df %>% 
#   mutate(avg_height = (height_low_inches+height_high_inches) /2)

# -- add multiple columns
# df_avg_ht_wt = df %>%
#   mutate(avg_height = (height_low_inches + height_high_inches)/2,
#          avg_weight = (weight_low_lbs + weight_high_lbs)/2,
#          rank_change_13_to_16 = rank_2016 - rank_2013)

# --- transmute columns
# keep only the new columns you made
# returns a df with new columns only

# df_avg_ht_wt = df %>%
#   transmute(avg_height = (height_low_inches + height_high_inches)/2,
#          avg_weight = (weight_low_lbs + weight_high_lbs)/2,
#          rank_change_13_to_16 = rank_2016 - rank_2013)


# ---- rename colummns
# rename(new_column_name = old_column_name)
# 
# df %>%
#   rename(book_title = name,
#          author = written_by)
# 
#



# ========= data cleaning







# ====== dealing with multiple files
# get all of the relevant information into 
# one table so that you can analyze the aggregate data.

# files <- list.files(pattern = "file_.*csv")
# df_list <- lapply(files,read_csv)
# df <- bind_rows(df_list)

# student_files = list.files(pattern= 'exams_.*csv')
# df_list = lapply(student_files, read_csv)
# students = bind_rows(df_list)
# nrow_students = nrow(students)


# ======= reshape your data
# Each variable as a separate column
# Each row as a separate observation

# df %>%
#   gather('col_1_to_gather',
#          'col_2_to_gather',
#          key='col_name_variables', 
#          value='col_name_values')
# 


# ===== deal with duplicates
# To check for duplicates, we can use the base R function duplicated()
# We can use the dplyr distinct() function to remove 
# all rows of a data frame that are duplicates of another row.

# If we wanted to remove every row with a duplicate value 
# in the item column, we could specify a subset
# fruits %>%
#   distinct(item,.keep_all=TRUE)

# table() is a base R function that takes any R object as an argument 
# and returns a table with the counts of each unique value in the object.

# updated_duplicates <- students %>%
#   duplicated() %>%
#   table()
# updated_duplicates



# ======= splitting by index
# Let’s say we have a column “birthday” with data 
# formatted in MMDDYYYY format. In other words, “11011993” 
# represents a birthday of November 1, 1993. We want to 
# split this data into day, month, and year

# Create the 'month' column, index 1,2
df %>%
  mutate(month = str_sub(birthday,1,2))

# Create the 'day' column
df %>%
  mutate(day = str_sub(birthday,3,4))

# Create the 'year' column
df %>%
  mutate(year = str_sub(birthday,5))

students = students %>% mutate(gender = str_sub(gender_age,1,1))
head(students)


# add gender and age columns
students <- students %>%
  mutate(gender = str_sub(gender_age,1,1),
         age = str_sub(gender_age,2))
head(students)

students <- students %>%
  select(-gender_age)


# ============ splitting by char
# Create the 'user_type' and 'country' columns
# "user_Kenya" ==> "Kenya", "user"
# df %>%
#   separate(type,c('user_type','country'),'_')

# df %>%
#   separate(col_to_separate, 
#            c('new_col_name_1',
#              'new_col_name_2'), 
#            'character_to_split_on', 
#            extra ='merge')

students = students %>%
  separate(full_name, c('first_name','last_name'), ' ', extra='merge')


# ======= looking for data types
# want to convert between types so that we can do better analysis. 
# Calling str() with a data frame as an argument will return a variety of information


# ===== string parsing 
# use a regular expression, a sequence of characters that 
# describe a pattern of text to be matched, to remove all 
# of the dollar signs. The base R function gsub() will 
# remove the $ from the price column,

fruit %>%
  mutate(price=gsub('\\$','',price))

# use the base R function as.numeric() to convert character 
# strings containing numerical values to numeric:
fruit %>%
  mutate(price = as.numeric(price))

# remove % from score column
students = students %>% mutate(score = gsub('%','',score))

students = students %>% mutate(score = as.numeric(score))




# ================================== Data Visualization
# geometries, shortened to “geoms”, describe the shapes that represent our data
# aesthetics, or the visual attributes of the plot, including the scales on the 
# axes, the color, the fill, and other attributes concerning appearance

viz <- ggplot(data=movies, 
              aes(x=imdbRating, 
                  y=nrOfWins)) +
  geom_point(aes(color=nrOfGenre), alpha=0.5) + 
  labs(title="Movie Ratings Vs Award Wins", 
       subtitle="From IMDB dataset", 
       y="Number of Award Wins", 
       x="Move Rating", 
       color = "Number of Genre")


# create a ggplot object is invoke the ggplot() function the “canvas”
#  add aesthetics layer to canvas
# add geoms (the shape) geom_point()
# completing a line of best fit is the geom_smooth() layer. 
# This layer, by nature, comes with a gray error band

# viz <- ggplot(data=df, 
#               aes(x=col1,y=col2)) +
#   geom_point() + 
#   geom_smooth()

# change an aesthetic based on visual preference and not data
# you could simply pass in a color parameter with a manual value

# viz <- ggplot(data=airquality, aes(x=Ozone, y=Temp)) +
#   geom_point(color="darkred")  

# --- labels
# to customize your labels, 
# you can add a labs() function call to your ggplot object.
# can provide new labels for the x and y axes as well as a title, subtitle, or caption


# Bar charts are great for showing the distribution of categorical data. 
# Typically, one of the axes on a bar chart will have numerical values 
# and the other will have the names of the different categories you wish to understand.
# geom_bar()

bar <- ggplot(books, aes(x=Language)) + geom_bar( aes(fill= class )) +
  labs()
bar
ggsave("bar-example.png")

# types of vehicles in the dataset, so provide the canvas, or the ggplot() 
# object with an aesthetic mapping aes() that makes the x axis represent 
# the categorical values of the class column in the dataframe.



# ========= Aggregates
# aggregate statistic is a way of creating a 
# single number that describes a group of numbers. 
# Common aggregate statistics include mean, median, and standard deviation.

average_price <- orders %>% 
  summarize(mean_price = mean(price, na.rm = TRUE))
average_price

# page visits by UTM source
click_source <- page_visits %>%
  group_by(utm_source) %>%
  summarize(count = n())
click_source

# page visits by UTM source and month
click_source_by_month <- page_visits %>%
  group_by(utm_source,month) %>%
  summarize(count = n())
click_source_by_month

# define num_colors here:
num_colors = orders %>%
  summarize(num_colors = n_distinct(shoe_color))

# combine all of the values from a column for a single calculation. 
# This can be done with the help of the dplyr function summarize()
# 

# group_by()
grades <- df %>%
  group_by(student) %>%
  summarize(mean_grade = mean(grade))

# mean(), median(), sd(), var(), min(), max(), IQR() and n_distinct()), another helpful summary function

# To get the count of the rows in each group of students
grades <- df %>%
  group_by(student) %>%
  summarize(count = n())



# define pricey_shoes here:
pricey_shoes <- orders %>%
  group_by(shoe_type) %>%
  summarize(max_price = max(price, na.rm = TRUE))
pricey_shoes

# define shoes_sold here:
shoes_sold <- orders %>%
  group_by(shoe_type) %>%
  summarize(count = n())
shoes_sold

# define shoe_counts here:
shoe_counts = orders %>% group_by(shoe_type, shoe_color ) %>%
  summarize(count= n())

# define shoe_prices here:
shoe_prices <- orders %>%
  group_by(shoe_type, shoe_material) %>%
  summarize(mean_price = mean(price, na.rm = TRUE))
shoe_prices






# Combining Grouping with Filter
enrollments %>%
  group_by(course) %>%
  filter(mean(quiz_score) < 80)

# define most_pop_orders here:
most_pop_orders = orders %>%
  group_by(shoe_type) %>%
  filter(n() >16)




# Combining Grouping with Mutate
# add a new column to the data frame that stores the difference between a row’s
enrollments %>% 
  group_by(course) %>% 
  mutate(diff_from_course_mean = quiz_score - mean(quiz_score))

# define diff_from_mean here:
diff_from_mean = orders %>%
  group_by(shoe_type) %>%
  mutate(diff_from_shoe_type_mean = price - mean(price, na.rm = TRUE))



# define average_price here:
average_price <- orders %>% 
  summarize(mean_price = mean(price, na.rm = TRUE))
average_price

# define click_source here:
click_source <- page_visits %>%
  group_by(utm_source) %>%
  summarize(count = n())
click_source

# define click_source_by_month here:
click_source_by_month <- page_visits %>%
  group_by(utm_source,month) %>%
  summarize(count = n())
click_source_by_month



# ===== joining tables
orders <- read_csv("orders.csv")
customers <- read_csv("customers.csv")
products <- read_csv("products.csv")

# inner_join() method looks for columns that are common 
# between two data frames and then looks for rows where 
# those columns’ values are the same. It then combines 
# the matching rows into a single row in a new table

# joined_df <- orders %>%
#   inner_join(customers)

# Create a new data frame sales_vs_targets 
# which contains the inner_join() of sales and targets.
sales <- read_csv("sales.csv")
targets <- read_csv("targets.csv")

sales_vs_targets = sales %>% inner_join(targets)

crushing_it = sales_vs_targets %>%
  filter(revenue > target)


# inner_join() to join two data frames together,
# we can use the pipe %>% to join multiple data frames together at once.
# big_df <- orders %>%
#   inner_join(customers) %>%
#   inner_join(products)
all_data = sales %>% 
  inner_join(targets) %>%
  inner_join(men_women)

results = all_data %>%
  filter( revenue > target,
          women > men)



#===== Join on Specific Columns
# the id columns would mean something different in each table, 
# our default joins would be wrong, use the dplyr function rename() 
# to rename the columns for our joins.

# customers <- customers %>%
#   rename(customer_id = id)
# inner_join(orders, customers)

products = products %>%
  rename(product_id = id)

orders_products = orders %>%
  inner_join(products)


# We can add the by argument when calling inner_join() 
# to specify which columns we want to join on.
# The new column names id_x and id_y aren’t very helpful for us 
# when we read the table. We can help make them more useful by using the keyword suffix

# orders %>% 
#   inner_join(customers,
#              by = c('customer_id' = 'id'),
#              suffix = c('_order','_customer'))

orders_products = orders %>% 
  inner_join(products, by= c('product_id' = 'id'))

products_orders = products %>% 
  inner_join(orders, by= c('id' = 'product_id'),
             suffix = c('_product','_order'))


#====== Full Join
# Company A and Company B have just merged. 
# They each have a list of customers, but they keep slightly different data.
# Company A has each customer’s name and email. Company B has each customer’s 
# name and phone number. They have some customers in common, but some are different.

# If we wanted to combine the data from both companies without 
# losing the customers who are missing from one of the tables, we could use a Full Join

# full_joined_dfs <- company_a %>%
#   full_join(company_b)

library(readr)
library(dplyr)
store_a <- read_csv("store_a.csv")
store_b <- read_csv("store_b.csv")

store_a_b_full = store_a  %>%
  full_join(store_b)


# ========= LEFT / RIGHT JOIN
# want to identify which customers are missing phone information. 
# We would want a list of all customers who have email, but don’t have phone

# Left Join includes all rows from the first (left) table, 
# but only rows from the second (right) table that match the first table.

# If the first data frame is company_a and we do a left join, 
# we’ll only end up with rows that appear in company_a
# we get all customers from Company A, and only customers 
# from Company B who are also customers of Company A.

left_joined_df <- company_a %>%
  left_join(company_b)

right_joined_df <- company_a %>%
  right_join(company_b)


#======= Concatenate Data Frames
# a dataset is broken into multiple tables. For instance, 
# data is often split into multiple CSV files so that each download is smaller
# need to reconstruct a single data frame from multiple smaller data frames 
# his method only works if all of the columns are the same in all of the data frames. 

concatenated_dfs <- df1 %>%
  bind_rows(df_2)








# ========================== mean
library(readr)
library(dplyr)
library(ggplot2)

# load data frame
greatest_books <- read_csv('top-hundred-books.csv')

#plot data
hist <- qplot(greatest_books$Ages,
              geom='histogram',
              binwidth = 3,  
              main = 'Age of Top 100 Novel Authors at Publication', 
              xlab = 'Publication Age',
              ylab = 'Count',
              fill=I("blue"), 
              col=I("red"), 
              alpha=I(.2)
              ) +
  geom_vline(aes(xintercept=mean(greatest_books$Ages),
                 color="mean"), linetype="solid",
             size=1) +
  scale_color_manual(name = "statistics", values = c(median = "blue", 
                                                     mean = "red",
                                                     mode="green"))

hist


greatest_books = read_csv("top-hundred-books.csv")
author_ages <- greatest_books$Ages
average_age = mean(author_ages)
average_age


# median (middle value)
author_ages <- greatest_books$Ages
median_age <- median(author_ages)


# mode (most freq item)
# R package DescTools includes a handy Mode() function 
library(DescTools)
author_ages <- greatest_books$Ages
mode_age = Mode(author_ages)

# variance (spread of data)
teacher_one_variance <- variance(teacher_one_grades)

# std
nba_standard_deviation <- sd(nba_data)

# quartiles
dataset <- c(50, 10, 4, -3, 4, -20, 2)
third_quartile <- quantile(dataset, 0.75)

# quantiles = points that split a dataset into groups of equal size.

# Create a variable named twenty_third_percentile that 
# contains the value that splits the first 23% of the data from the rest of the data.
twenty_third_percentile = quantile(songs, 0.23)

dataset <- c(5, 10, -20, 42, -9, 10)
ten_percent <- quantile(dataset, c(0.2, 0.4, 0.6, 0.8))
quartiles = quantile(songs, c(0.25,0.5,0.75))
deciles= quantile(songs, c(0.10,0.20,0.3,0.4,0.5,0.6,0.7,0.8,0.9))


# range = the difference between the maximum and minimum values.

#  IQR
dataset = c(4, 10, 38, 85, 193)
interquartile_range = IQR(dataset)











# ============ hypothesis testing
# Suppose you want to know the average height of an oak 
# tree in your local park. On Monday, you measure 10 trees 
# and get an average height of 32 ft. You continue this all week 
# A sample is a subset of the entire population (all the oak trees in the park). 
# The mean of each sample is a sample mean and it is an estimate of the population mean.

# generate random population
population <- rnorm(300, mean=65, sd=3.5)
population_mean <- mean(population)

# sampling error, and occurs when a sample is not representative of the population it comes from. 


# base R intersect() function can take two vectors as arguments 
# and returns a vector containing the common elements

# define type_i_errors and type_ii_errors here:
type_i_errors = intersect(experimental_positive,actual_negative)
type_i_errors

type_ii_errors = intersect(experimental_negative, actual_positive )
type_ii_errors

# P-values help determine how confident you can be in validating the null hypothesis. 
# In this context, a p-value is the probability that, assuming the null hypothesis is true
# 
# p = 0.2 , "There is a 20% chance that the difference in average weight of 
# green and red apples is due to random sampling."

# significance level of 0.05 or less, meaning that there is a 
# 5% or less chance that your result is due to sampling error.



# === t-test
# R has a function called t.test() in the stats 
# package which can perform a One Sample T-Test for you.

# One Sample T-Test compares a sample mean to a hypothetical population mean. 
# It answers the question “What is the probability that the sample came 
# from a distribution with the desired mean?”

results <- t.test(sample_distribution, mu = expected_mean)
#  ages data set
ages_mean = mean(ages)
results = t.test(ages, mu= 30)


# Two Sample T-Test compares two sets of data, which are both approximately normally distributed.
results <- t.test(distribution_1, distribution_2)

prob_error <- (1-(0.95**number_of_t_tests)) # example we had 3 t.tests() for datasets A, B and C

# ANOVA
# best way to preserve a Type I error probability of 0.05 is to use ANOVA. 
# ANOVA (Analysis of Variance) tests the null hypothesis that all of 
# the datasets you are considering have the same mean. If you reject 
# the null hypothesis with ANOVA, you’re saying that at least one of
# the sets has a different mean; however, it does not tell you which datasets are different.
# 

# use the stats package function aov() to perform ANOVA on multiple datasets. 

# if you were comparing scores on a video game between math majors, writing majors, 
# and psychology majors, you could format the data in a data frame df_scores
results <- aov(score ~ group, data = df_scores)
summary(results) # returns p-value

# perform anova on stores here:
results <- aov(sales ~ store, data = stores)
results
summary(results)















































