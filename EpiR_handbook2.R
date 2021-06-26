
# =========== Epidemiology R Handbook 

library(dplyr)
pacman::p_load(
  rio,        # importing data, handles various file types
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)


# import data
# linelist_raw <- import("linelist_raw.xlsx")

head(linelist_raw)

# review data
# skim = skimr::skim(linelist_raw)


# column names
names(linelist_raw)





# =========================== Cleaning Pipeline



# automatic column name cleaning === janitor package 
# pipe the raw dataset through the function clean_names(), assign result as "linelist"  
df.clean <- linelist_raw %>% 
  janitor::clean_names()

# see the new column names
names(df.clean)


# === manually rename() columns
# can also use the select() method for same result
df.clean.2 = df.clean %>% 
  janitor::clean_names() %>% 
  # new_name = old_name
  rename(date_infection = infection_date,
         date_hospitalization = hosp_date)


# Excel data has merged cells => problems, use this:
# df_raw <- openxlsx::readWorkbook("linelist_raw.xlsx", fillMergedCells = TRUE)
# duplicated column names => errors, manually fix them


# --- select column you want
selected_columns = df.clean.2 %>% 
  select(case_id, date_onset, date_hospitalization, fever) 

selected_columns


# ---- organize columns order with ''everything()''
# name the columns you want to have priority then everything() afterwards
df.clean.2 %>% 
  select( date_onset, date_hospitalization, everything()) %>% 
  names()


# --- select columns based on class using where()
# get columns with numeric class 
df.clean.3 %>% select(
  where(is.numeric)) %>% 
  names()

# --- get columns with "date" in them
df.clean.2 %>% 
  select(contains("date")) %>% 
  names()


# -- remove columns
df.clean.3 = df.clean.2 %>% 
  select( c(-row_num, -merged_header, -x28))


# --- remove duplicates
df.clean.3 = df.clean.3 %>% distinct()
  
# --- mutate a new column
df.clean.3 = df.clean.3 %>% 
  mutate( BMI = wt_kg / (ht_cm/100)^2)


# --- data type class
class(df.clean.3$age) # char
# mutate the class to numeric
df.clean.3 = df.clean.3 %>% 
  mutate(age = as.numeric(age))


# ----- NORMALIZE data using mutate
# normalize age to mean for all rows
df.clean.3 = df.clean.3 %>% 
  mutate(age.norm = age / mean(age, na.rm=T)) 

# normalize hospital group ages
df.clean.3 = df.clean.3 %>%   
  group_by(hospital) %>% 
  mutate(age.hosp.norm = age / mean(age, na.rm=T))


# --- apply transformations all at once to columns using across()


# --- change values manually you can use the recode() function within the mutate() function.
# 
# fix incorrect values                   # old value       # new value
# df <- df %>% 
#   mutate(date_onset = recode(date_onset, "2014-14-15" = "2014-04-15"))


# --- replace()  
# mutate(col_to_change = replace(col_to_change, criteria for rows, new value))

df.clean.3 = df.clean.3 %>% 
  mutate(gender = replace(gender, case_id == "2195", "Transgender"))



# --- replace NAs use replace_na()
df.clean.3 = df.clean.3 %>% 
  mutate(hospital = replace_na(hospital, "Missing"))

# -- faster method is use forcats function: fct_explicit_na()
df.clean.2 %>% 
  mutate(hospital = fct_explicit_na(hospital))



# -- review age_years column distribution
hist(df.clean.3$age)

summary(df.clean.3$age, na.rm=T)


# --- age categories with break points for factor
# base R : use cut() to do same thing
library(epikit)

df.clean.3 = df.clean.3 %>% 
  mutate(age_cat = age_categories(age, 
                                  breakers = c(0,5,10,15,20,30,
                                               40,50,60,70,80,90)))
  

# show table
table(df.clean.3$age_cat, useNA = "always")



# ---- age quantiles
quantile(df.clean.3$age,
         probs = c(0,.25,.5,.75,.9,.95), # percentiles
         na.rm = T)


# -- edit Hospital name
df.clean.3 = df.clean.3 %>% 
  mutate(hospital = recode(hospital,
                           "Central Hopital" = "Port Hospital"))


# -- quantiles/ deciles --------------- code gives errors
# df.clean.3 = df.clean.3 %>% 
#   mutate(deciles = cut(age,
#                        breaks= quantile(age,
#                                         probs= seq(0,1, by=0.1),
#                                         na.rm=T))) %>% 
#   janitor::tabyl(deciles)




# ----------- filter for condition
gender.f = df.clean.3 %>%
  filter(gender == "f") %>%
  drop_na(case_id, age )






# --- hist date onset with 50 breaks
hist(df.clean.3$lon, breaks = 50)







# -- design a filter
table(Hospital = df.clean.3$hospital,
      YearOnset = lubridate::year(df.clean.3$date_onset),
      useNA = "always")

# --- exclude criteria
# rows with onset in 2012 and 2013 at either hospital A, B, or Port:
nrow(df.clean.3 %>% 
       filter(hospital %in% c("Hospital A", 
                              "Hospital B") | date_onset < as.Date("2013-06-01"))) 

# rows from Hospitals A & B with missing onset dates
nrow(df.clean.3 %>% 
       filter(hospital %in% c('Hospital A', 'Hospital B') & is.na(date_onset))) 

df.clean.3 = df.clean.3 %>% 
  filter(date_onset > as.Date("2013-06-01") | (is.na(date_onset) & !hospital 
                                               %in% c("Hospital A", "Hospital B")))
nrow(df.clean.3)

table(Hospital  = df.clean.3$hospital,                     # hospital name
      YearOnset = lubridate::year(df.clean.3$date_onset),  # year of date_onset
      useNA     = "always")  



# ======== end of cleaning pipeline ==================







# ================= working with dates

pacman::p_load(
  lubridate,  # general package for handling and converting dates  
  # linelist,   # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  tidyverse,  # data management and visualization  
  rio)  

# pacman::p_install_gh("appliedepi/epirhandbook")
# pacman::p_load(epirhandbook)

# download only the linelist example data into a folder on your computer
# get_data(file = "linelist_cleaned.rds")


ebola <- import("linelist_cleaned.xlsx")

# get system date
Sys.Date()
# get system time
Sys.time()

# ------------- convert df dates to date class
ebola = ebola %>% 
  mutate(date_onset = as.Date(date_onset, format= "%d/%m/%Y"),
         date_hospitalisation = as.Date(date_hospitalisation, format= "%d/%m/%Y"),
         date_outcome = as.Date(date_outcome, format= "%d/%m/%Y"),
         date_infection = as.Date(date_infection, format= "%d/%m/%Y"))


# lubridate :: floor_date()  unit="week" week_start= 1 (Monday) 

# weekly counts
weekly_counts = ebola %>% 
  drop_na(date_onset) %>% 
  mutate(weekly_cases = floor_date(
    date_onset,
    unit = "week"
  )) %>% 
  count(weekly_cases) %>%   # group data by week and count rows per group (creates column 'n')
  tidyr::complete(      # ensure all weeks are present, even those with no cases reported
    weekly_cases = seq.Date(
      from = min(weekly_cases),
      to = max(weekly_cases),
      by= "week"
    ),
    fill =  list(n=0) # fill NA in the n counts with 0
  )

# lubridate also has functions week(), epiweek(), and isoweek(), 
# each of which has slightly different start dates and other nuances

library(aweek)
library(lubridate)
# -- deal with different timezones to standardize datetimes 
time_now <- Sys.time()
time_now

# use with_tz() to assign a new timezone to the column, while CHANGING the clock time
time_london_now <- with_tz(time_now, "Europe/London")
# use force_tz() to assign a new timezone to the column, while KEEPING the clock time
time_london_local <- force_tz(time_now, "Europe/London")

t.delta = time_london_now - time_london_local
t.delta



# ====== lag() and lead() functions for case counts per week calculations
# counts = ebola %>% 
#   mutate(cases_prev_wk = lag(cases_wk, n = 1),
#          case_diff = cases_wk - cases_prev_wk)



# ============================ strings & char
library(stringr)

str_c("String1", "String2", "String3")
str_c("String1", "String2", "String3", sep = ", ")

first_names <- c("abdul", "fahruk", "janice") 
last_names  <- c("hussein", "akinleye", "okeke")
str_c(first_names, last_names, sep = " ", collapse = ";  ")
cat(str_c(first_names, last_names, sep = " ", collapse = ";\n"))

str_glue("Data include {nrow(ebola)} cases and are 
         current to {format(Sys.Date(), '%d %b %Y')}.")


str_glue("ebola as of {current_date}.\nLast case hospitalized on {last_hospital}.\n{n_missing_onset} cases are missing date of onset and not shown",
         current_date = format(Sys.Date(), '%b %d %Y'),
         last_hospital = format(as.Date(max(ebola$date_hospitalisation, na.rm=T)), '%b %d %Y'),
         n_missing_onset = nrow(ebola %>% filter(is.na(ebola$date_onset)))
)






# == new df
case_table = data.frame(
  # columns =         << rows >>
  zone = c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5"),
  new_cases = c(3, 7, 1, 0, 15),
  total_cases = c(40,4,25,10,103)
)
# Use str_glue_data(), which is specially made for taking data from data frame rows:
case_table %>% 
  str_glue_data("{zone}: {new_cases} ({total_cases} total cases)")

# or have on 1-line
str_c(case_table$zone, case_table$new_cases, sep = " = ", collapse = ";  ")


library(tidyr)
library(tidyverse)
# combine columns into a df
symptoms.df <- data.frame(
  case_ID = c(1:6),
  symptoms  = c("jaundice, fever, chills",     # patient 1
                "chills, aches, pains",        # patient 2 
                "fever",                       # patient 3
                "vomiting, diarrhoea",         # patient 4
                "bleeding from gums, fever",   # patient 5
                "rapid pulse, headache"),      # patient 6
  outcome = c("Recover", "Death", "Death", "Recover", "Recover", "Recover"))

df_split <- separate(df, symptoms, into = c("sym_1", "sym_2", "sym_3"), extra = "merge")


str_split(string = "jaundice, fever, chills",
          pattern = ",")

symptoms <- c("jaundice, fever, chills",     # patient 1
              "chills, aches, pains",        # patient 2 
              "fever",                       # patient 3
              "vomiting, diarrhoea",         # patient 4
              "bleeding from gums, fever",   # patient 5
              "rapid pulse, headache")       # patient 6

str_split(symptoms, ",")        


# --arrange by alphabet
# strings
health_zones <- c("Alba", "Takota", "Delta")

# return the alphabetical order
str_order(health_zones)

# - truncate
original <- "Symptom onset on 4/3/2020 with vomiting"
str_trunc(original, 10, "center")

stringy = "  whitespace  "
str_trim(stringy )
str_squish(stringy)



# search terms
occupation_med_frontline <- str_c("medical", "medicine", "hcw", "healthcare", "home care", "home health",
                                  "surgeon", "doctor", "doc", "physician", "surgery", "peds", "pediatrician",
                                  "intensivist", "cardiologist", "coroner", "nurse", "nursing", "rn", "lpn",
                                  "cna", "pa", "physician assistant", "mental health",
                                  "emergency department technician", "resp therapist", "respiratory",
                                  "phlebotomist", "pharmacy", "pharmacist", "hospital", "snf", "rehabilitation",
                                  "rehab", "activity", "elderly", "subacute", "sub acute",
                                  "clinic", "post acute", "therapist", "extended care",
                                  "dental", "dential", "dentist", sep = "|")

occupation_med_frontline

sum(str_detect(string = occupation_med_frontline, pattern = "subacute"))
sum(str_detect(string = occupation_med_frontline, pattern = "nurse|nursing|rn"))

str_extract_all(occupation_med_frontline, pattern = "medical|medicine")

# -- replace
outcome <- c("Karl: dead",
             "Samantha: dead",
             "Marco: not dead")

str_replace_all(string = outcome, pattern = "dead", replacement = "deceased")



# ==================================== regex
# Character set	    Matches for
# "[A-Z]"	          any single capital letter
# "[a-z]"	          any single lowercase letter
# "[0-9]"	          any digit
# [:alnum:]	        any alphanumeric character
# [:digit:]	        any numeric digit
# [:alpha:]	        any letter (upper or lowercase)
# [:upper:]	        any uppercase letter
# [:lower:]	        any lowercase letter

# Meta character	Represents
# "\\s"	          a single space
# "\\w"	          any single alphanumeric character (A-Z, a-z, or 0-9)
# "\\d"	          any single numeric digit (0-9)

test <- "A-AA-AAA-AAAA"

str_extract_all(test, "A{2}")
str_extract_all(test, "A{2,4}")
str_extract_all(test, "A+") # groups 1+


pt_note <- "Patient arrived at Broward Hospital emergency ward at 18:00 on 6/12/2005. Patient presented with radiating abdominal pain from LR quadrant. Patient skin was pale, cool, and clammy. Patient temperature was 99.8 degrees farinheit. Patient pulse rate was 100 bpm and thready. Respiratory rate was 29 per minute."

str_extract_all(pt_note, "[A-Za-z]+")

# expression "[0-9]{1,2}" matches to consecutive numbers that are 1 or 2 digits in length. 
# It could also be written "\\d{1,2}", or "[:digit:]{1,2}"

str_extract_all(pt_note, "[0-9]{1,2}")





# ===================== FACTORS 
# convert a column from character or numeric class to a factor if you want to 
# set an intrinsic order to the values (“levels”) so they can be displayed 
# non-alphabetically in plots and tables. Another common use of factors is 
# to standardise the legends of plots so they do not fluctuate if certain 
# values are temporarily absent from the data.
# 
library(forcats)
pacman::p_load(
  lubridate,  # general package for handling and converting dates  
  # linelist,   # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  tidyverse,  # data management and visualization  
  rio) 

ebola <- import("linelist_cleaned.xlsx")

# --- change to datetimes
ebola = ebola %>% 
  mutate(date_hospitalisation = as.Date(date_hospitalisation, format= "%m/%d/%Y"),
         date_onset = as.Date(date_onset, format="%m/%d/%Y" ),
         date_outcome = as.Date(date_outcome, format = "%m/%d/%Y" ),
         date_infection = as.Date(date_infection, format = "%m/%d/%Y" )
         )


ebola <- ebola %>% 
  mutate(delay_cat = case_when(
    # criteria                                   # new value if TRUE
    days_onset_hosp < 2                        ~ "<2 days",
    days_onset_hosp >= 2 & days_onset_hosp < 5 ~ "2-5 days",
    days_onset_hosp >= 5                       ~ ">5 days",
    is.na(days_onset_hosp)                     ~ NA_character_,
    TRUE                                       ~ "Check me"))  


# new column delay_cat is a categorical column of class Character - not yet a factor. 
# Thus, in a frequency table, we see that the unique values appear 
# in a default alpha-numeric order - an order that does not make much intuitive sense:

# table uses the cross-classifying factors to build a contingency table 
# of the counts at each combination of factor levels.
table(ebola$delay_cat, useNA = "always")

ggplot(data= ebola) +
  geom_bar(mapping =  aes(x= delay_cat))


# --- convert to a factor, from chr
ebola = ebola %>% 
  mutate(delay_cat = fct_relevel(delay_cat,"<2 days", "2-5 days", ">5 days"))

# unique “values” in this column are now considered “levels” of the factor. 
# The levels have an order, which can be printed with the base R function levels(), 
# or alternatively viewed in a count table via table() from base R or tabyl() from janitor.

levels(ebola$delay_cat)

ggplot(data = ebola) +
  geom_bar(mapping = aes(x= delay_cat))

# -- add/drop levels
ebola %>% 
  mutate(delay_cat = fct_expand(delay_cat, "Not admitted","Patient Transferred")) %>% 
  tabyl(delay_cat)

# -- drop NAs 
ebola %>% 
  mutate(delay_cat = fct_drop(delay_cat)) %>%
  tabyl(delay_cat)


# --- factor gender column
ebola = ebola %>% 
  mutate(gender = as.factor(gender))



# ordered by frequency
ggplot(data = ebola, aes(x = fct_infreq(delay_cat)))+
  geom_bar()+
  labs(x = "Delay onset to admission (days)",
       title = "Ordered by frequency")

# reversed frequency
ggplot(data = ebola, aes(x = fct_rev(fct_infreq(delay_cat))))+
  geom_bar()+
  labs(x = "Delay onset to admission (days)",
       title = "Reverse of order by frequency")




# boxplots ordered by original factor levels
ggplot(data = ebola)+
  geom_boxplot(
    aes(x = delay_cat,
        y = ct_blood, 
        fill = delay_cat))+
  labs(x = "Delay onset to admission (days)",
       title = "Ordered by original alpha-numeric levels")+
  theme_classic()+
  theme(legend.position = "none")


# boxplots ordered by median CT value
ggplot(data = ebola)+
  geom_boxplot(
    aes(x = fct_reorder(delay_cat, ct_blood, "median"),
        y = ct_blood,
        fill = delay_cat))+
  labs(x = "Delay onset to admission (days)",
       title = "Ordered by median CT blood value in group")+
  theme_classic()+
  theme(legend.position = "none")



epidemic_data <- ebola %>%         # begin with the ebola  
  filter(date_onset < as.Date("2014-09-21")) %>%    # cut-off date, for visual clarity
  count(                                            # get case counts per week and by hospital
    epiweek = lubridate::floor_date(date_onset, "week"),  
    hospital                                            
  ) 
ggplot(data = epidemic_data)+                       # start plot
  geom_line(                                        # make lines
    aes(
      x = epiweek,                                  # x-axis epiweek
      y = n,                                        # height is number of cases per week
      color = fct_reorder2(hospital, epiweek, n)))+ # data grouped and colored by hospital, with factor order by height at end of plot
  labs(title = "Factor levels (and legend display) by line height at end of plot",
       color = "Hospital")                          # change legend title



ebola %>% 
  mutate(gender = fct_recode(
    gender,
    'Female' = "f",
    "Male" = "m"
  )) %>% 
  # replace_na(gender, "Transgender") %>% 
  tabyl(gender)




ebola %>% 
  mutate(hospital = fct_other(
    hospital,
    keep =  c("Port Hospital","Central Hospital"),
    other_level = "Other Hospital"
  )) %>% 
  tabyl(hospital)

ebola %>% 
  mutate(hospital = fct_lump(
    hospital,
    n=2, # keep top 2 levels
    other_level = "Other Hospital"
  )) %>% 
  tabyl(hospital)


ggplot(data = ebola)+
  geom_bar(mapping = aes(x = hospital, fill = age_cat)) +
  scale_fill_discrete(drop = FALSE)+                        # show all age groups in the legend, even those not present
  labs(
    title = "All age groups will appear in legend, even if not present in data")


ebola %>% 
  mutate(epiweek_date = floor_date(date_onset, "week")) %>%  # create week column
  ggplot()+                                                  # begin ggplot
  geom_histogram(mapping = aes(x = epiweek_date))+           # histogram of date of onset
  scale_x_date(date_labels = "%Y-W%W")                       # adjust display of dates to be YYYY-WWw















# ============ PIVOT !
# pivot tables, which are tables of statistics that summarise the data of a more extensive table
# conversion of a table from long to wide format, or vice versa.

library(tidyverse)
library(rio)
library(here)

ebola <- import("linelist_cleaned.xlsx")
count_data <- import("malaria_facility_count_data.rds")


count_data$data_date %>% min()
count_data$data_date %>% max()


# basic initial look at data
ggplot(count_data) +
  geom_col(aes(x = data_date, y = malaria_tot), width = 1)


#------------- pivot longer() 
# It accepts a range of columns to transform (specified to cols = ). 
# Therefore, it can operate on only a part of a dataset. 
# This is useful for the malaria data, as we only want to pivot the case count columns.

df_long <- count_data %>% 
  pivot_longer(
    cols = c(`malaria_rdt_0-4`, `malaria_rdt_5-14`, `malaria_rdt_15`, `malaria_tot`)
  )

df_long
# the df now has 4x's the rows


# provide column with a tidy select helper function
df_long = count_data %>% 
  pivot_longer(
    cols = starts_with("malaria_"),
    names_to = "age_group",
    values_to = "counts"
  )

df_long


# We can now pass this new dataset to ggplot2, and map the new column count to the 
# y-axis and new column age_group to the fill = argument (the column internal color). 
# This will display the malaria counts in a stacked bar chart, by age group:

ggplot(data = df_long) +
  geom_col(
    mapping = aes(x = data_date, y = counts, fill = age_group),
    width = 1
  )

# ! we have also included the total counts from the malaria_tot column, 
#   so the magnitude of each bar in the plot is twice as high as it should be.

df_long %>% 
  filter(age_group != "malaria_tot") %>%  # filter out the total column
  ggplot() +
  geom_col(
    aes(x = data_date, y = counts, fill = age_group),
    width = 1
  )



# ---- pivoting data of different class types
# you will encounter will be the need to pivot columns that contain different classes of data. 
# This pivot will result in storing these different data types in a single column, which is not a good situation. 
# There are various approaches one can take to separate out the mess this creates

# In order to work with these data, we need to transform the data frame to long format, 
# but keeping the separation between a date column and a character (status) column, for each observation for each item.

# Example columns: id | obs1_date | obs1_status ...    < very wide dataset >
# df %>% 
#   pivot_longer(
#     cols = -id,
#     names_to = c("observation",".value"),
#     names_sep = "_"
#   )

# df_long <- 
#   df_long %>% 
#   mutate(
#     date = date %>% lubridate::as_date(),  # convert dates into date class
#     observation = 
#       observation %>% 
#       str_remove_all("obs") %>%     # remove the obs from obs1
#       as.numeric()                   # convert char to numeric
#   )





# ------- long to wide table, pivot_wider() 
# A typical use-case is when we want to transform the results of an analysis 
# into a format which is more digestible for the reader (such as a Table for presentation).


# Suppose that we want to know the counts of individuals in the different age groups, by gender:
# df_wide <- 
#   df %>% 
#   count(age_cat, gender)
# 
# df_wide

# a long dataset that is great for producing visualizations in ggplot2, but not ideal for presentation in a table:
# ggplot(df_wide) +
#   geom_col(aes(x = age_cat, y = n, fill = gender))

# argument names_from specifies the column from which to generate the new column names, 
# while the argument values_from specifies the column from which to take the values to populate the cells. 
# The argument id_cols = is optional, but can be provided a vector of column names that should not be pivoted, 
# and will thus identify each row.

# table_wide <- 
#   df_wide %>% 
#   pivot_wider(
#     id_cols = age_cat,
#     names_from = gender,
#     values_from = n
#   )

# table_wide %>% 
#   janitor::adorn_totals(c("row", "col")) %>% # adds row and column totals
#   knitr::kable() %>% 
#   kableExtra::row_spec(row = 10, bold = TRUE) %>% 
#   kableExtra::column_spec(column = 5, bold = TRUE) 





# ======= fill a dataset 
df1 <- 
  tibble::tribble(
    ~Measurement, ~Facility, ~Cases,
    1,  "Hosp 1",     66,
    2,  "Hosp 1",     26,
    3,  "Hosp 1",      8,
    1,  "Hosp 2",     71,
    2,  "Hosp 2",     62,
    3,  "Hosp 2",     70,
    1,  "Hosp 3",     47,
    2,  "Hosp 3",     70,
    3,  "Hosp 3",     38,
  )

df1 

# -- year column
df2 <- 
  tibble::tribble(
    ~Year, ~Measurement, ~Facility, ~Cases,
    2000,            1,  "Hosp 4",     82,
    2001,            2,  "Hosp 4",     87,
    2002,            3,  "Hosp 4",     46
  )

df2


# -- join tables with bind_rows()
df_combined <- 
  bind_rows(df1, df2) %>% 
  arrange(Measurement, Facility)

df_combined

# === fill()
df_combined %>% 
  fill(Year, .direction = "up") # "down"

# --
df_combined <- 
  df_combined %>% 
  arrange(Measurement, desc(Facility))

df_combined <- 
  df_combined %>% 
  fill(Year, .direction = "down")
df_combined

ggplot(df_combined) +
  aes(Year, Cases, fill = Facility) +
  geom_col()

# --- end of pivoting data








# ------------------ grouping data for descriptive stats
library(tidyverse)


ebola <- import("linelist_cleaned.xlsx")

# -- group_by() 
# group df column outcome
df.outcome = ebola %>% 
  group_by(outcome)

df.outcome
# Note that there is no perceptible change to the dataset after running group_by(), 
# until another dplyr verb such as mutate(), summarise(), or arrange() is applied on the “grouped” data frame.


# --- unique groups
# To see the groups and the number of rows in each group, 
# pass the grouped data to tally(). 
# To see just the unique groups without counts you can pass to group_keys().

# based on outcome column groups, get the number of rows for death
nrow(ebola %>%  filter(outcome =="Death"))
nrow(ebola %>%  filter(outcome =="Recover"))
nrow(ebola %>%  filter(is.na(outcome))) # how many NAs in group outcome


# You can group by more than one column. Below, the data frame is grouped by outcome and gender, and then tallied
ebola %>% 
  group_by(outcome, gender) %>% 
  tally()




# --- add new columns within a group_by()  like a mutate()
ebola %>% 
  group_by(
    age_class = ifelse(age >= 18, "adult", "child")) %>% 
  tally(sort= T)



# -- add/drop grouping columns
# group_by() on data that are already grouped, the old groups will be removed and the new one(s) will apply. 
# If you want to add new groups to the existing ones, include the argument .add = TRUE.

# group by outcome
by_outcome = ebola %>% 
  group_by(outcome)


# Add grouping by gender in addition
by_outcome_gender <- by_outcome %>% 
  group_by(gender, .add = TRUE)


# -- ungroup()
ebola %>% 
  group_by(outcome, gender) %>% 
  tally() %>% 
  ungroup()




# ------------- summarize()
# summarize()) takes a data frame and converts it into a new summary data frame, 
# with columns containing summary statistics that you define. 
# On an ungrouped data frame, the summary statistics will be calculated from all rows. 
# Applying summarise() to grouped data produces those summary statistics for each group.

# summary statistics on ungrouped linelist
ebola %>% 
  summarise(
    n_cases  = n(),
    mean_age = mean(age_years, na.rm=T),
    max_age  = max(age_years, na.rm=T),
    min_age  = min(age_years, na.rm=T),
    n_males  = sum(gender == "m", na.rm=T))

# summary statistics on grouped linelist
ebola %>% 
  group_by(outcome) %>% 
  summarise(
    n_cases  = n(),
    mean_age = mean(age_years, na.rm=T),
    max_age  = max(age_years, na.rm=T),
    min_age  = min(age_years, na.rm=T),
    n_females  = sum(gender == "f", na.rm=T))



# ---- tally()
# summarise(n = n()), and does not group data. Thus, to achieve grouped tally it must follow a group_by()

ebola %>% 
  group_by(outcome) %>% 
  tally(sort = TRUE)


# ---- count()
ebola %>% count(outcome)

ebola %>% 
  count(age_class = ifelse(age >= 18, "adult", "child"), sort = T)


#  to summarise the number of hospitals present for each gender
ebola %>% 
  # produce counts by unique outcome-gender groups
  count(gender, hospital) %>% 
  # gather rows by gender (3) and count number of hospitals per gender (6)
  count(gender, name = "hospitals per gender" ) 


ebola %>% 
  as_tibble() %>%                   # convert to tibble for nicer printing 
  add_count(hospital) %>%           # add column n with counts by hospital
  select(hospital, n, everything()) # re-arrange for demo purposes



# -- add totals
library(janitor)

ebola  %>%                                    # df ebola
  tabyl(age_cat, gender) %>%                  # cross-tabulate counts of two columns
  adorn_totals(where = "row") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions with column denominator
  adorn_pct_formatting() %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age Category",
    col_name = "Gender")



# --- group by date
# When grouping data by date, you must have (or create) a column for the date unit of interest - for example “day”, “epiweek”, “month”, etc
# “fill-in” any dates in the sequence that are not present in the data. 
# Use complete() from tidyr so that the aggregated date series is complete 
# including all possible date units within the range. Without this step, a 
# week with no cases reported might not appear in your data!

# Within complete() you re-define your date column as a sequence of dates seq.Date() 
# from the minimum to the maximum - thus the dates are expanded. By default, the case count values 
# in any new “expanded” rows will be NA. You can set them to 0 using the fill = argument of complete(), 
# which expects a named list (if your counts column is named n, provide fill = list(n = 0)


# ------------------------------
library(lubridate)

ebola = ebola %>% 
  mutate(date_infection = as.Date(date_infection, format= "%m/%d/%Y"),
         date_onset = as.Date(date_onset, format= "%m/%d/%Y"),
         date_hospitalisation = as.Date(date_hospitalisation, format= "%m/%d/%Y"),
         date_outcome = as.Date(date_outcome, format= "%m/%d/%Y")
         )
# ------------------------------


# ---------------------- cases by day
daily_counts <- ebola %>% 
  drop_na(date_onset) %>%                 # remove case missing date_onset
  count(date_onset) %>%                   # count number of rows per unique date
  complete(                               # ensure all days appear even if no cases
    date_onset = seq.Date(                # re-define date column as daily sequence of dates
      from = min(date_onset, na.rm=T), 
      to = max(date_onset, na.rm=T),
      by = "day"),
    fill = list(n = 0))                   # set new filled-in rows to display 0 in column n (not NA as default) 


# ------------------- cases by weeks
weekly_counts <- ebola %>% 
  drop_na(date_onset) %>%                 # remove cases missing date_onset
  mutate(week = lubridate::floor_date(date_onset, unit = "week")) %>%  # new column of week of onset
  count(week) %>%                         # group data by week and count rows per group
  complete(                               # ensure all days appear even if no cases
    week = seq.Date(                      # re-define date column as daily sequence of dates
      from = min(week, na.rm=T), 
      to = max(week, na.rm=T),
      by = "week"),
    fill = list(n = 0))                   # set new filled-in rows to display 0 in column n (not NA as default) 



# ------------------ cases by months

# To aggregate cases into months, again use floor_date() from the lubridate package, 
# but with the argument unit = "months". This rounds each date down to the 1st of its month

monthly_counts <- ebola %>% 
  drop_na(date_onset) %>% 
  mutate(month = lubridate::floor_date(date_onset, unit = "months")) %>%  # new column, 1st of month of onset
  count(month) %>%                          # count cases by month
  complete(
    month = seq.Date(
      min(month, na.rm=T),     # include all months with no cases reported
      max(month, na.rm=T),
      by="month"),
    fill = list(n = 0))

monthly_counts





# --------- slice rows per group
# dplyr function slice(), which filters rows based on their position in the data, can also be applied per group. 
# Remember to account for sorting the data within each group to get the desired “slice”.

# retrieve only the latest 5 admissions from each hospital:
#   - Group the ebola by column hospital
#   - Arrange the records from latest to earliest date_hospitalization within each hospital group
#   - Slice to retrieve the first 5 rows from each hospital

ebola %>%
  group_by(hospital) %>%
  arrange(hospital, date_hospitalisation) %>%
  slice_head(n = 5) %>% 
  arrange(hospital) %>%                            # for display
  select(case_id, hospital, date_hospitalisation)  # for display


# slice_head()   - selects n rows from the top
# slice_tail()   - selects n rows from the end
# slice_sample() - randomly selects n rows
# slice_min()    - selects n rows with highest values in order_by = column, use with_ties = TRUE to keep ties
# slice_max()    - selects n rows with lowest values in order_by = column, use with_ties = TRUE to keep ties
# 



# ------ filter on group size
# function add_count() adds a column n to the original data giving the number of rows in that row’s group.

ebola %>% 
  as_tibble() %>% 
  add_count(hospital) %>%          # add "number of rows admitted to same hospital as this row" 
  select(hospital, n, everything())




# ----- mutate grouped data
# calculate the difference between a row’s delay-to-admission and the median delay for their hospital.
  # Group the data by hospital
  # Use the column days_onset_hosp (delay to hospitalization) to create a new column containing the mean delay at the hospital of that row
  # Calculate the difference between the two columns
  
ebola %>% 
  # group data by hospital (no change to ebola df yet)
  group_by(hospital) %>% 
  
  # new columns
  mutate(
    # mean days to admission per hospital (rounded to 1 decimal)
    group_delay_admit = round( mean(days_onset_hosp, na.rm=T), 1),
    
    # difference between row's delay and mean delay at their hospital (rounded to 1 decimal)
    diff_to_group     = round( days_onset_hosp - group_delay_admit, 1)) %>%
  
  # select certain rows only - for demonstration/viewing purposes
  select(case_id, hospital, days_onset_hosp, group_delay_admit, diff_to_group)


# ============================== end of grouping data 






# ========================================================== joining data 
pacman::p_load(
  rio,            # import and export
  here,           # locate files 
  tidyverse,      # data management and visualization
  RecordLinkage,  # probabilistic matches
  fastLink        # probabilistic matches
)

ebola <- import("linelist_cleaned.xlsx")


df_mini <- ebola %>%                 # start with original ebola df
  select(case_id, date_onset, hospital) %>%   # select columns
  head(10)    


df_mini = df_mini %>% 
  mutate(date_onset = as.Date(date_onset, format= "%m/%d/%Y"))

# -- create new df of hospital info
# Make the hospital information data frame
hosp_info = data.frame(
  hosp_name     = c("central hospital", "military", "military", "port", "St. Mark's", "ignace", "sisters"),
  catchment_pop = c(1950280, 40500, 10000, 50280, 12000, 5000, 4200),
  level         = c("Tertiary", "Secondary", "Primary", "Secondary", "Secondary", "Primary", "Primary")
)


# --- data clean, find differences between data frames
unique(df_mini$hospital)

unique(hosp_info$hosp_name)

hosp_info <- hosp_info %>% 
  mutate(
    hosp_name = case_when(
      # criteria                         # new value
      hosp_name == "military"          ~ "Military Hospital",
      hosp_name == "port"              ~ "Port Hospital",
      hosp_name == "St. Mark's"        ~ "St. Mark's Maternity Hospital (SMMH)",
      hosp_name == "central hospital"  ~ "Central Hospital",
      TRUE                             ~ hosp_name
    )
  )

unique(hosp_info$hosp_name)
# -----------------------------



# - JOINS ARE CASE SENSITIVE 
# Join based on common values between column "ID" (first data frame) and column "identifier" (second data frame)
joined_data <- left_join(df1, df2, by = c("ID" = "identifier"))

# Joint based on common values in column "ID" in both data frames
joined_data <- left_join(df1, df2, by = "ID")

# join based on same first name, last name, and age. 
# joins rows if the values in three columns in each dataset align exactly.
joined_data <- left_join(df1, df2, by = c("name" = "firstname", "surname" = "lastname", "Age" = "age"))



df_mini %>% 
  left_join(hosp_info, by = c("hospital" = "hosp_name"))

# “Should I use a right join, or a left join?” == whatever df has all the rows you want to keep

right_join(hosp_info, df_mini, by = c("hosp_name" = "hospital"))


# A full join is the most inclusive of the joins - it returns all rows from both data frames.
df_mini %>% 
  full_join(hosp_info, by = c("hospital" = "hosp_name"))




#  inner join is the most restrictive of the joins - it returns only rows with matches across both data frames.
df_mini %>% 
  inner_join(hosp_info, by = c("hospital" = "hosp_name"))


# semi-join keeps all observations in the baseline data frame that have a match in the secondary data frame 
# (but does not add new columns nor duplicate any rows for multiple matches).
hosp_info %>% 
  semi_join(df_mini, by = c("hosp_name" = "hospital"))


#  anti join is another “filtering join” that returns rows in the 
# baseline data frame that do not have a match in the secondary data frame.
hosp_info %>% 
  anti_join(df_mini, by = c("hosp_name" = "hospital"))




#----------------------- Probabilistic matching
# If you do not have a unique identifier common across datasets to join on, 
# consider using a probabilistic matching algorithm. This would find matches between records based on similarity



# ----- bind rows, 
# very inclusive, so any column present in either data frame will be included in the output.
# dplyr :: bind_rows() does not require that the order of columns be the same in both data frames. 
# As long as the column names are spelled identically,

hospital_summary = ebola %>% 
  group_by(hospital) %>% 
  summarise(
    cases = n(),
    ct_value_median = median(ct_blood, na.rm = T))

hospital_summary

hosp.totals = ebola %>% 
  summarise(cases= n(), ct_blood_median = median(ct_blood, na.rm = T))


# Case information
case_info <- ebola %>% 
  group_by(hospital) %>% 
  summarise(
    cases = n(),
    deaths = sum(outcome == "Death", na.rm=T)
  )

case_info


# Use match() to align ordering
# use match() from base R to align the rows of a data frame in the same order as in another.

# ------------------------------












# ========================== de-duplication
pacman::p_load(
  tidyverse,   # deduplication, grouping, and slicing functions
  janitor,     # function for reviewing duplicates
  stringr)      # for string searches, can be used in "rolling-up" values

covid.19 <- data.frame(
  recordID  = c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),
  personID  = c(1,1,2,2,3,2,4,5,6,7,2,1,3,3,4,5,5,7,8),
  name      = c("adam", "adam", "amrish", "amrish", "mariah", "amrish", "nikhil", "brian", "smita", "raquel", "amrish",
                "adam", "mariah", "mariah", "nikhil", "brian", "brian", "raquel", "natalie"),
  date      = c("1/1/2020", "1/1/2020", "2/1/2020", "2/1/2020", "5/1/2020", "5/1/2020", "5/1/2020", "5/1/2020", "5/1/2020","5/1/2020", "2/1/2020",
                "5/1/2020", "6/1/2020", "6/1/2020", "6/1/2020", "6/1/2020", "7/1/2020", "7/1/2020", "7/1/2020"),
  time      = c("09:00", "09:00", "14:20", "14:20", "12:00", "16:10", "13:01", "15:20", "14:20", "12:30", "10:24",
                "09:40", "07:25", "08:32", "15:36", "15:31", "07:59", "11:13", "17:12"),
  encounter = c(1,1,1,1,1,3,1,1,1,1,2,
                2,2,3,2,2,3,2,1),
  purpose   = c("contact", "contact", "contact", "contact", "case", "case", "contact", "contact", "contact", "contact", "contact",
                "case", "contact", "contact", "contact", "contact", "case", "contact", "case"),
  symptoms_ever = c(NA, NA, "No", "No", "No", "Yes", "Yes", "No", "Yes", NA, "Yes",
                    "No", "No", "No", "Yes", "Yes", "No","No", "No")) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"))


covid.19 %>% 
  tabyl(name, purpose)

# ------ janitor package to find duplicates
covid.19 %>%
  janitor::get_dupes()

# Duplicates when column recordID is not considered
covid.19 %>%
  janitor::get_dupes(-recordID)

# duplicates based on name and purpose columns ONLY
covid.19 %>%
  janitor::get_dupes(name, purpose)


# --------- only unique rows
distinct(covid.19)
#  or
covid.19 %>% 
  distinct( across( -recordID),
            .keep_all = T) %>% 
  arrange(name)



# ================= slicing
# basic slice() function accepts numbers and returns rows in those positions. 
# If the numbers provided are positive, only they are returned. 
# If negative, those rows are not returned. 

covid.19 %>% slice(4)

covid.19 %>% slice( c(2,4,6)) # rows 2,4,6

covid.19 %>% slice( c(2:6)) # rows 2,3,4,5,6

covid.19 %>% slice_max(encounter, n = 1)  # return rows with the largest encounter number


# ----------------------------




# ============================================== loops & lists
pacman::p_load(
  rio,            # import and export
  here,           # locate files 
  tidyverse,      # data management and visualization
  purrr
)

ebola <- import("linelist_cleaned.xlsx")

# for loop
for (x in c(1:9)) {
  print(x *3)
}

#  loop through hospital names
H_names = unique(ebola$hospital)
for (h in H_names){
  print(h)
}


# create a container to store results
case_demographics = vector(mode="character", length = nrow(ebola))

for (i in 1: nrow(ebola)) {
  row_gender = ebola$gender[[i]]
  row_age = ebola$age[[i]]
  
  case_demographics[[i]] = str_c(row_gender, row_age, sep = " , ")
}

head(case_demographics, 10)


# ------ looping plots

library(incidence2)

ebola = ebola %>% 
  mutate(date_infection = as.Date(date_infection, format="%m/%d/%Y"),
         date_onset = as.Date(date_onset, format="%m/%d/%Y"),
         date_hospitalisation= as.Date(date_hospitalisation, format="%m/%d/%Y"),
         date_outcome = as.Date(date_outcome, format="%m/%d/%Y"),
         gender = as.factor(gender)
         )


# create 'incidence' object
outbreak <- incidence2::incidence(   
  x = ebola,                      # dataframe - complete ebola
  date_index = date_onset,        # date column
  interval = "week",              # aggregate counts weekly
  groups = gender,                # group values by gender
  na_as_group = TRUE)             # missing gender is own group
outbreak

# plot epi curve
plot(outbreak,                       # name of incidence object
     fill = "gender",                # color bars by gender
     color = "black",                # outline color of bars
     title = "Ebola outbreak of ALL cases" # title
)




# make vector of the hospital names
hospital_names <- unique(ebola$hospital)
hospital_names

# ========================================================
















# ==================================== descriptive tables

pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable     # converting tables to pretty images
)

ebola <- import("linelist_cleaned.xlsx")


ebola = ebola %>% 
  mutate(date_infection = as.Date(date_infection, format= "%m/%d/%Y"),
         date_onset = as.Date(date_onset, format= "%m/%d/%Y"),
         date_hospitalisation = as.Date(date_hospitalisation, format= "%m/%d/%Y"),
         date_outcome = as.Date(date_outcome, format= "%m/%d/%Y"),
         gender = as.factor(gender)
         )

skim(ebola)


summary(ebola$age)
summary(ebola$age)[[2]] # get 2nd element


ebola  %>% 
  get_summary_stats(
    age, wt_kg, ht_cm, ct_blood, temp,  # columns to calculate for
    type = "common")   



#----------- Cross-tabulation counts
ebola %>%  tabyl(age_cat, gender)


#  janitor’s “adorn” functions to add totals or convert to proportions, percents, or otherwise adjust the display

# Function	              Outcome
# adorn_totals()	        Adds totals (where = “row”, “col”, or “both”). Set name = for “Total”.
# adorn_percentages()	    Convert counts to proportions, with denominator = “row”, “col”, or “all”
# adorn_pct_formatting()	Converts proportions to percents. Specify digits =. Remove the “%” symbol with affix_sign = FALSE.
# adorn_rounding()	      To round proportions to digits = places. To round percents use adorn_pct_formatting() with digits =.



ebola %>%               
  tabyl(age_cat) %>%       # tabulate counts and proportions by age category
  adorn_pct_formatting()   # convert proportions to percents


# total row  and row percents
ebola %>%                                  
  tabyl(age_cat, gender) %>%                  # counts by age and gender
  adorn_totals(where = "row") %>%             # add total row
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1)            # convert proportions to percents


#--------------------- cross tab counts and percents
ebola %>%                               
  tabyl(age_cat, gender) %>%                  # cross-tabulate counts
  adorn_totals(where = "row") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting() %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age Category",
    col_name = "Gender")
# --------------------------



# --- get counts for each hospital
ebola %>% 
  count(hospital) %>%   # dplyr function
  adorn_totals()        # janitor function



# --- save the tabyl
# flextable:: save_as_html(), save_as_word(), save_as_ppt(), and save_as_image()

ebola %>%
  tabyl(age_cat, gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Age Category",
    col_name = "Gender",
    placement = "combined") %>% 
  flextable::flextable() %>%                     # convert to image
  flextable::autofit() %>%   
  flextable::save_as_html(path = "ebola-data.html")



# stats, using package tabyl, like chisq.test() or fisher.test() f

outcome_by_age = ebola %>% 
  tabyl(outcome, age_cat, show_na= F)

chisq.test(outcome_by_age)



#========================== proportions
age_summary <- ebola%>% 
  count(age_cat) %>%                     # group and count by gender (produces "n" column)
  mutate(                                # create percent of column - note the denominator
    percent = scales::percent(n / sum(n))) 

# print
age_summary
# =========================


# =========== method to calculate proportions within groups. 
# It relies on different levels of data grouping being selectively applied and removed.

age_by_outcome <- ebola %>%                 
  group_by(outcome) %>%                         # group by outcome 
  count(age_cat) %>%                            # group and count by age_cat, and then remove age_cat grouping
  mutate(percent = scales::percent(n / sum(n))) # calculate percent - note the denominator is by outcome group

age_by_outcome



ebola %>%                  
  count(age_cat, outcome) %>%     # group and tabulate counts by two columns
  ggplot()+                       # pass new data frame to ggplot
  geom_col(                     # create bar plot
    mapping = aes(   
      x = outcome,              # map outcome to x-axis
      fill = age_cat,           # map age_cat to the fill
      y = n))                   # map the counts column `n` to the height



# describe the days delay from symptom onset to hospital admission (column days_onset_hosp), by hospital.
summary_table <- ebola %>%                                        
  group_by(hospital) %>%                                             # group all calculations by hospital
  summarise(                                                         # only the below summary columns will be returned
    cases       = n(),                                                # number of rows per group
    delay_max   = max(days_onset_hosp, na.rm = T),                    # max delay
    delay_mean  = round(mean(days_onset_hosp, na.rm=T), digits = 1),  # mean delay, rounded
    delay_sd    = round(sd(days_onset_hosp, na.rm = T), digits = 1),  # standard deviation of delays, rounded
    delay_3     = sum(days_onset_hosp >= 3, na.rm = T),               # number of rows with delay of 3 or more days
    pct_delay_3 = scales::percent(delay_3 / cases)                    # convert previously-defined delay column to percent 
  )

summary_table  # print



# --- conditionals
ebola %>% 
  group_by(hospital) %>% 
  summarise(
    max_temp_fvr = max(temp[fever == "yes"], na.rm = T),
    max_temp_no = max(temp[fever == "no"], na.rm = T)
  )



# percentiles 
# get default percentile values of age (0%, 25%, 50%, 75%, 100%)
ebola %>% 
  summarise(age_percentiles = quantile(age_years, na.rm = TRUE))


# get manually-specified percentile values of age (5%, 50%, 75%, 98%)
ebola %>% 
  group_by(hospital) %>% 
  summarise(
    p05 = quantile(age_years, probs = 0.05, na.rm=T),
    p50 = quantile(age_years, probs = 0.5, na.rm=T),
    p75 = quantile(age_years, probs = 0.75, na.rm=T),
    p98 = quantile(age_years, probs = 0.98, na.rm=T)
  )



# rstatix package.
ebola %>% 
  group_by(hospital) %>% 
  rstatix::get_summary_stats(age, type = "quantile")


# Summarise aggregated data
ebola_agg = ebola %>% 
  drop_na(gender, outcome) %>% 
  count(outcome, gender)

ebola_agg



# To sum the counts (in column n) by group you can use summarise() 
# but set the new column equal to sum(n, na.rm=T). 
# To add a conditional element to the sum operation, 
# you can use the subset bracket [ ] syntax on the counts column.

ebola_agg %>% 
  group_by(outcome) %>% 
  summarise(
    total_cases  = sum(n, na.rm=T),
    male_cases   = sum(n[gender == "m"], na.rm=T),
    female_cases = sum(n[gender == "f"], na.rm=T))


#  use summarise() across multiple columns using across(). 
#  want to calculate the same statistics for many columns.
ebola %>% 
  group_by(outcome) %>% 
  summarise(across(.cols = c(age_years, temp, wt_kg, ht_cm),  # columns
                   .fns = list("mean" = mean, "sd" = sd),     # functions
                   na.rm=T))                                  # extra arguments



# Janitor adorn_totals()
# grouped by gender and summarised into a table that described the number of cases with known outcome, 
# deaths, and recovered. Piping the table to adorn_totals() adds a total row at the bottom reflecting the sum of each column.

ebola %>% 
  group_by(gender) %>%
  summarise(
    known_outcome = sum(!is.na(outcome)),           # Number of rows in group where outcome is not missing
    n_death  = sum(outcome == "Death", na.rm=T),    # Number of rows in group where outcome is Death
    n_recover = sum(outcome == "Recover", na.rm=T), # Number of rows in group where outcome is Recovered
  ) %>% 
  adorn_totals() %>%                                # Adorn total row (sums of each numeric column)
  adorn_percentages("col") %>%                      # Get column proportions
  adorn_pct_formatting() %>%                        # Convert proportions to percents
  adorn_ns(position = "front")                      # display % and counts (with counts in front)



# summary table of outcome by hospital with group_by() and summarise()
by_hospital <-ebola %>% 
  filter(!is.na(outcome) & hospital != "Missing") %>%  # Remove cases with missing outcome or hospital
  group_by(hospital, outcome) %>%                      # Group data
  summarise(                                           # Create new summary columns of indicators of interest
    N = n(),                                            # Number of rows per hospital-outcome group     
    ct_value = median(ct_blood, na.rm=T))               # median CT value per group

by_hospital # print table




#  summary
ebola %>% 
  select(age_years, 
         gender, 
         outcome, 
         fever, 
         temp, 
         hospital) %>%  # keep only the columns of interest
  tbl_summary()      




age_by_outcome <- table(ebola$age_cat, 
                        ebola$outcome, 
                        useNA = "always") # save table as object
age_by_outcome   # print table

# ----------- Base R proportions, prop.table() %>% round(2)  
# round to 2 digits
# margins = argument to specify whether you want the proportions to be of 
# rows (1), of columns (2), or of the whole table (3)

# get proportions of table defined above, by rows, rounded
prop.table(age_by_outcome, 1) %>% round(2)

# ---------- end chapter 17 Descriptive stats





















# ------------ chap 18 simple stat tests

pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  rstatix,      # statistics
  corrr,        # correlation analysis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable     # converting tables to HTML
)

ebola <- import("linelist_cleaned.xlsx")

# ==================================== t-test
# typically used to determine if there is a significant difference 
# between the means of some numeric variable between two groups. 
# numeric column on the Left side of the equation and the categorical column on the Right side.

## compare mean age by outcome group with a t-test
t.test(age_years ~ gender, data = ebola)

ebola %>% 
  t_test(age_years ~ gender)

# Shapiro-Wilk test can be used to determine whether a sample came from a normally-distributed population 
# (an assumption of many other tests and analysis, such as the t-test). 
# However, this can only be used on a sample between 3 and 5000 observations. 

# sample size must be between 3 and 5000
# shapiro.test(ebola$age_years)
ebola %>% 
  head(500) %>%            # first 500 rows of case ebola, for example only
  shapiro_test(age_years)

# Wilcoxon rank sum test, also called the Mann–Whitney U test, 
# is often used to help determine if two numeric samples are from the same distribution 
# when their populations are not normally distributed or have unequal variance.

## compare age distribution by outcome group with a wilcox test
wilcox.test(age_years ~ outcome, data = ebola)

# The Kruskal-Wallis test is an extension of the Wilcoxon rank sum test 
# that can be used to test for differences in the distribution of more than two samples. 
# When only two samples are used it gives identical results to the Wilcoxon rank sum test.

## compare age distribution by outcome group with a kruskal-wallis test
kruskal.test(age_years ~ outcome, ebola)

ebola %>% 
  kruskal_test(age_years ~ outcome)

# Pearson’s Chi-squared test is used in testing for significant differences between categorical croups.
## compare the proportions in each group with a chi-squared test
chisq.test(ebola$gender, ebola$outcome)

ebola %>% 
  tabyl(gender, outcome) %>% 
  select(-1) %>% 
  chisq_test()

# chi-square test 
ebola %>% 
  select(gender, outcome) %>%    # keep variables of interest
  tbl_summary(by = outcome) %>%  # produce summary table and specify grouping variable
  add_p()  

#  t-test 
ebola %>% 
  select(age_years, outcome) %>%             # keep variables of interest
  tbl_summary(                               # produce summary table
    statistic = age_years ~ "{mean} ({sd})", # specify what statistics to show
    by = outcome) %>%                        # specify the grouping variable
  add_p(age_years ~ "t.test")                # specify what tests to perform


# wilcoxon test 
ebola %>% 
  select(age_years, outcome) %>%                       # keep variables of interest
  tbl_summary(                                         # produce summary table
    statistic = age_years ~ "{median} ({p25}, {p75})", # specify what statistic to show (this is default so could remove)
    by = outcome) %>%                                  # specify the grouping variable
  add_p(age_years ~ "wilcox.test")                     # specify what test to perform (default so could leave brackets empty)


# kruskal-wallis
ebola %>% 
  select(age_years, outcome) %>%                       # keep variables of interest
  tbl_summary(                                         # produce summary table
    statistic = age_years ~ "{median} ({p25}, {p75})", # specify what statistic to show (default, so could remove)
    by = outcome) %>%                                  # specify the grouping variable
  add_p(age_years ~ "kruskal.test")                    # specify what test to perform



# get_summary_stats() is a quick way to return summary statistics.
# By default, a full range of summary statistics are returned: 
    # n, max, min, median, 25%, 75%, IQR, 
    # median absolute deviation (mad), mean, 
    # standard deviation (sd), 
    # standard error (se), 
    # confidence interval (ci) of the mean.
ebola %>% 
  rstatix::get_summary_stats(age, temp)


ebola %>%
  group_by(hospital) %>%
  rstatix::get_summary_stats(age, temp, type = "common")



#--------------------------- correlations
# tidyverse: compute correlations using Pearson, Kendall tau or Spearman rho. 

correlation_tab <- ebola %>% 
  select(generation, age, ct_blood, days_onset_hosp, wt_kg, ht_cm) %>%   # keep numeric variables of interest
  correlate()      # create correlation table (using default pearson)

correlation_tab    # print

## remove duplicate entries (the table above is mirrored) 
correlation_tab <- correlation_tab %>% 
  shave()

## view correlation table 
correlation_tab

## plot correlations 
rplot(correlation_tab)

# ---------------- end of chap 18












# ================================ chap 19 uni/multivar regression
pacman::p_load(
  rio,          # File import
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics, 
  stringr,      # manipulate text strings 
  purrr,        # loop over objects in a tidy way
  gtsummary,    # summary statistics and tests 
  broom,        # tidy up results from regressions
  lmtest,       # likelihood-ratio tests
  parameters,   # alternative to tidy up results from regressions
  see          # alternative to visualise forest plots
)

# import the linelist
ebola <- import("linelist_cleaned.rds")


## define variables of interest 
explanatory_vars <- c("gender", "fever", "chills", "cough", "aches", "vomit")


# convert the explanatory columns from “yes”/“no”, “m”/“f”, and “dead”/“alive” to 1 / 0, 
# to cooperate with the expectations of logistic regression models. 
# To do this efficiently, used across() from dplyr to transform multiple columns at one time. 
# The function we apply to each column is case_when() (also dplyr) which applies logic to convert specified values to 1’s and 0’s.

## convert dichotomous variables to 0/1 
ebola <- ebola %>%  
  mutate(across(                                      
    .cols = all_of(c(explanatory_vars, "outcome")),  ## for each column listed and "outcome"
    .fns = ~case_when(                              
      . %in% c("f", "yes", "Death")   ~ 1,           ## recode female, yes and death to 1
      . %in% c("m", "no",  "Recover") ~ 0,           ## male, no and recover to 0
      TRUE                            ~ NA_real_)    ## otherwise set to missing
  )
  )


# To drop rows with missing values, can use the tidyr function drop_na(). 
# However, we only want to do this for rows that are missing values in the columns of interest.
nrow(ebola)

## add in age_category to the explanatory vars 
explanatory_vars <- c(explanatory_vars, "age_cat")

## drop rows with missing information for variables of interest 
ebola <- ebola %>% 
  drop_na(any_of(c("outcome", explanatory_vars)))


# --------- linear regression
# perform linear regression, assessing the relationship between numeric 
# response and explanatory variables that are assumed to have a linear relationship.

LinReg_results <- lm(ht_cm ~ age, data = ebola)
summary(LinReg_results)

## pull the regression points and observed data in to one dataset
points <- augment(LinReg_results)

## plot the data using age as the x-axis 
ggplot(points, aes(x = age)) + 
  ## add points for height 
  geom_point(aes(y = ht_cm)) + 
  ## add your regression line 
  geom_line(aes(y = .fitted), colour = "red")


## add your data to a plot 
ggplot(ebola, aes(x = age, y = ht_cm)) + 
  ## show points
  geom_point() + 
  ## add a linear regression 
  geom_smooth(method = "lm", se = FALSE)



# --------------- logistic regression / Gen Linear Models
# uni/multivariate regression

# Family	            Default link function
# ------------------------------
# "binomial"	          (link = "logit")
# "gaussian"	          (link = "identity")
# "Gamma"	              (link = "inverse")
# "inverse.gaussian"	  (link = "1/mu^2")
# "poisson"	            (link = "log")
# "quasi"	              (link = "identity", variance = "constant")
# "quasibinomial"     	(link = "logit")
# "quasipoisson"	      (link = "log")


# -- univariate glm()
# assessing the association between different age categories and the outcome of death 
uni_model <- glm(outcome ~ age_cat, family = "binomial", data = ebola)
summary(uni_model)

uni_model <- glm(outcome ~ age_cat, family = "binomial", data = ebola) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns


# gtsummary
univ_tab <- ebola %>% 
  dplyr::select(explanatory_vars, outcome) %>% ## select variables of interest
  
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = outcome,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )

## view univariate results table 
univ_tab




# ----- multivariate regression

mv_reg <- glm(outcome ~ gender + fever + chills + cough + aches + vomit + age_cat, family = "binomial", data = ebola)

summary(mv_reg)


## show results table of final regression 
mv_tab <- tbl_regression(mv_reg, exponentiate = TRUE)
mv_tab



pacman::p_load(easystats)

## remove the intercept term from your multivariable results
mv_reg %>% 
  model_parameters(exponentiate = TRUE) %>% 
  plot()

# ------------------------- end of chap 19










# ------------------- chap 20 missing data
pacman::p_load(
  rio,           # import/export
  tidyverse,     # data mgmt and viz
  naniar,        # assess and visualize missingness
  mice           # missing data imputation
)
# install and/or load package
pacman::p_load(naniar)

ebola <- import("linelist_cleaned.rds")

my_vector <- c(1, 4, 56, NA, 5, NA, 22)

mean(my_vector)     
mean(my_vector, na.rm = TRUE)

# percent of ALL data frame values that are missing
pct_miss(ebola)

# Percent of rows with any value missing
pct_miss_case(ebola)   # use n_complete() for counts

# Percent of rows that are complete (no values missing)  
pct_complete_case(ebola) # use n_complete() for counts


#------- Visualizing missingness
gg_miss_var(ebola, show_pct = TRUE) # shows percents 

gg_miss_var(ebola, show_pct = F) # shows counts


ebola %>% 
  gg_miss_var(show_pct = TRUE, facet = outcome)


ggplot(
  data = ebola,
  mapping = aes(x = age_years, y = temp)) +     
  geom_miss_point()



ebola %>% 
  drop_na() %>%     # remove rows with ANY missing values
  nrow()

ebola %>% 
  drop_na(date_onset) %>%     # remove rows with ANY missing values
  nrow()


# ========================================= NA in factors
pacman::p_load(forcats)   # load package

ebola = ebola %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "trans"))

levels(ebola$gender)
# =========================================

# ------------------------------------------











# =================================== chap 21 standardized rates
pacman::p_load(
  rio,                 # import/export data
  here,                # locate files
  tidyverse,           # data management and visualization
  stringr,             # cleaning characters and strings
  frailtypack,         # needed for dsr, for frailty models
  dsr,                 # standardise rates
  PHEindicatormethods) # alternative for rate standardisation

# packageurl <- "https://cran.r-project.org/src/contrib/Archive/dsr/dsr_0.2.2.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")


# import demographics for country A directly from Github
A_demo <- import("https://github.com/epirhandbook/Epi_R_handbook/raw/master/data/standardization/country_demographics.csv")

# import deaths for country A directly from Github
A_deaths <- import("https://github.com/epirhandbook/Epi_R_handbook/raw/master/data/standardization/deaths_countryA.csv")

# import demographics for country B directly from Github
B_demo <- import("https://github.com/epirhandbook/Epi_R_handbook/raw/master/data/standardization/country_demographics_2.csv")

# import deaths for country B directly from Github
B_deaths <- import("https://github.com/epirhandbook/Epi_R_handbook/raw/master/data/standardization/deaths_countryB.csv")

# import demographics for country B directly from Github
standard_pop_data <- import("https://github.com/epirhandbook/Epi_R_handbook/raw/master/data/standardization/world_standard_population_by_sex.csv")


A_demo



pop_countries <- A_demo %>%  # begin with country A dataset
  bind_rows(B_demo) %>%        # bind rows, because cols are identically named
  pivot_longer(                       # pivot longer
    cols = c(m, f),                   # columns to combine into one
    names_to = "Sex",                 # name for new column containing the category ("m" or "f") 
    values_to = "Population") %>%     # name for new column containing the numeric values pivoted
  mutate(Sex = recode(Sex,            # re-code values for clarity
                      "m" = "Male",
                      "f" = "Female"))

pop_countries


#  did not finish this fucking chapter 

# ------------------------------------










# --------------------------- chap 22 moving averages 
pacman::p_load(
  tidyverse,      # for data management and viz
  slider,         # for calculating moving averages
  tidyquant       # for calculating moving averages within ggplot
)


# import the linelist
ebola <- import("linelist_cleaned.xlsx")


#  objective is to calculate a rolling 7-day incidence - the sum of cases using a rolling 7-day window.
# make dataset of daily counts
daily_counts <- ebola %>% 
  count(date_hospitalisation, name = "new_cases")

# standard rolling function (like slide_dbl() would use a window of 7 rows, not 7 days. 
# So, if there are any absent dates, some windows will actually extend more than 7 calendar days!
rolling <- daily_counts %>% 
  mutate(                                # create new columns
    # Using slide_dbl()
    ###################
    reg_7day = slide_dbl(
      new_cases,                         # calculate on new_cases
      .f = ~sum(.x, na.rm = T),          # function is sum() with missing values removed
      .before = 6),                      # window is the ROW and 6 prior ROWS
    
    # Using slide_index_dbl()
    #########################
    indexed_7day = slide_index_dbl(
      new_cases,                       # calculate on new_cases
      .i = date_hospitalisation,       # indexed with date_onset 
      .f = ~sum(.x, na.rm = TRUE),     # function is sum() with missing values removed
      .before = days(6))               # window is the DAY and 6 prior DAYS
  )

ggplot(data = rolling)+
  geom_line(mapping = aes(x = date_hospitalisation, y = indexed_7day), size = 1)


# ----------------------------------------








# ----------------------------------------chap 23 time series

pacman::p_load(rio,          # File import
               here,         # File locator
               tidyverse,    # data management + ggplot2 graphics
               tsibble,      # handle time series datasets
               slider,       # for calculating moving averages
               imputeTS,     # for filling in missing values
               feasts,       # for time series decomposition and autocorrelation
               forecast,     # fit sin and cosin terms to data (note: must load after feasts)
               trending,     # fit and assess models 
               tmaptools,    # for getting geocoordinates (lon/lat) based on place names
               ecmwfr,       # for interacting with copernicus sateliate CDS API
               stars,        # for reading in .nc (climate data) files
               units,        # for defining units of measurement (climate data)
               yardstick,    # for looking at model accuracy
               surveillance  # for aberration detection
)

# import the counts into R
counts <- rio::import("campylobacter_germany.xlsx")


## ensure the date column is in the appropriate format
counts$date <- as.Date(counts$date)

## create a calendar week variable 
## fitting ISO definitons of weeks starting on a monday
counts <- counts %>% 
  mutate(epiweek = yearweek(date, week_start = 1))


#  did not finish 23

# ---------------------------------








# ---------------------------------- chap 24 epi modeling 
pacman::p_load(
  rio,          # File import
  here,         # File locator
  tidyverse,    # Data management + ggplot2 graphics
  epicontacts,  # Analysing transmission networks
  EpiNow2,      # Rt estimation
  EpiEstim,     # Rt estimation
  projections,  # Incidence projections
  incidence2,   # Handling incidence data
  epitrix,      # Useful epi functions
  distcrete     # Discrete delay distributions
)

# reproduction number R is a measure of the transmissibility of a disease and is defined 
# as the expected number of secondary cases per infected case. In a fully susceptible population, 
# this value represents the basic reproduction number R0. However, as the number of susceptible 
# individuals in a population changes over the course of an outbreak or pandemic, and as various 
# response measures are implemented, the most commonly used measure of transmissibility is the 
# effective reproduction number Rt; this is defined as the expected number of secondary cases 
# per infected case at a given time t.














# ------------------------- chap 28 GIS basics

# Vector Data - The most common format of spatial data used in GIS, vector data are comprised of geometric 
#             features of vertices and paths. Vector spatial data can be further divided into three widely-used types:
  
#     Points - A point consists of a coordinate pair (x,y) representing a specific location in a coordinate system. 
      # Points are the most basic form of spatial data, and may be used to denote a case (i.e. patient home) or a location (i.e. hospital) on a map.

#     Lines - A line is composed of two connected points. Lines have a length, and may be used to denote things like roads or rivers.

#     Polygons - A polygon is composed of at least three line segments connected by points. Polygon features have a length (i.e. 
          # the perimeter of the area) as well as an area measurement. Polygons may be used to note an area (i.e. a village) or a structure (i.e. the actual area of a hospital).

# Shapefiles - A shapefile is a common data format for storing “vector” spatial data consisting or lines, points, or polygons. 
# A single shapefile is actually a collection of at least three files - .shp, .shx, and .dbf. 

# If your dataset is not in a spatial format you will also need a reference dataset. Reference data consists of the spatial representation of the data
# When in doubt, a good place to start is to Google “[regions] shapefile”

pacman::p_load(
  rio,           # to import data
  here,          # to locate files
  tidyverse,     # to clean, handle, and plot the data (includes ggplot2 package)
  sf,            # to manage spatial data using a Simple Feature format
  tmap,          # to produce simple maps, works for both interactive and static maps
  janitor,       # to clean column names
  OpenStreetMap, # to add OSM basemap in ggplot map
  spdep          # spatial statistics
) 

# generate 1000 random row numbers, from the number of rows
sample_rows <- sample(nrow(ebola), 1000)

# subset ebola to keep only the sample rows, and all columns
ebola <- ebola[sample_rows,]

# convert  class dataframe, to an object of class “sf” (spatial features). 
# Given that the ebola has two columns “lon” and “lat” representing the longitude and latitude 
# of each case’s residence, this will be easy.
# lon and lat have been designated as coordinate columns, and a coordinate reference system (CRS) 
# has been assigned for when the points are displayed. 4326 identifies our coordinates as based on 
# the World Geodetic System 1984 (WGS84) 
ebola_sf = ebola %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)



#  didn't finish chapter











pacman::p_load(
  tidyverse,      # includes ggplot2 and other data management tools
  rio,            # import/export
  here,           # file locator
  stringr         # working with characters   
)


# make display version of columns with more friendly names
ebola <- ebola %>%
  mutate(
    gender_disp = case_when(gender == "m" ~ "Male",        # m to Male 
                            gender == "f" ~ "Female",      # f to Female,
                            is.na(gender) ~ "Trans"),    # NA to Unknown
    
    outcome_disp = replace_na(outcome, "Unknown")          # replace NA outcome with "unknown"
  )



symptoms_data <- ebola %>% 
  select(c(case_id, fever, chills, cough, aches, vomit))




ggplot(data = ebola,
       mapping = aes(x = age, y = wt_kg, color = gender))+
  geom_point(alpha = 0.5)



# A) Jitter plot by group
ggplot(data = ebola %>% drop_na(outcome),      # remove missing values
       mapping = aes(y = age,                     # Continuous variable
                     x = outcome,                           # Grouping variable
                     color = outcome))+                     # Color variable
  geom_jitter()+                                  # Create the violin plot
  labs(title = "A) jitter plot by gender")     



# B) Outcomes in all cases by hosptial
ggplot(ebola %>% drop_na(outcome)) + 
  geom_bar(aes(y = fct_rev(hospital), fill = outcome), width = 0.7) +
  theme_minimal()+
  theme(legend.position = "bottom") +
  labs(title = "B) Number of recovered and dead Ebola cases, by hospital",
       y = "Hospital")












# ------------------------------------------ dashboards w/ R-markdown
pacman::p_load(
  rio,             # data import/export     
  here,            # locate files
  tidyverse,       # data management and visualization
  flexdashboard,   # dashboard versions of R Markdown reports
  shiny,           # interactive figures
  plotly           # interactive figures
)

# File > New file > R Markdown.
# select “From Template” and select the “Flex Dashboard” template. 




























