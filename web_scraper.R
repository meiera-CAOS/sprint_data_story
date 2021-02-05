# install.packages('rvest')
library(rvest)

url = 'https://en.wikipedia.org/wiki/100_metres#100_metres_per_age_category'
# url2 = 'html(https://en.wikipedia.org/wiki/100_metres#100_metres_per_age_category)'

# TODO: generalize for different types of data?
# load data from file directly if already scraped data and scrape and write csv file otherwise
load_data = function(data_name, data_xpath){
  data_filepath = paste('~/sprint/data/', data_name, '.csv', sep = '')
  # check if data file exists
  if (!file.exists(data_filepath)){
    # access data at url by XPATH
    loaded_data = url %>% 
      read_html() %>% 
      html_node(xpath = data_xpath) %>% 
      html_table(fill = TRUE)
    # export to csv
    write.csv(loaded_data, file=data_filepath, row.names = FALSE)
  } else {
    loaded_data = read.csv(data_filepath, check.names = FALSE)
  }
  return(loaded_data)
}

PAB_name = 'per_age_boys'
PAB_xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/div[9]/div[11]/table/tbody/tr/td[1]/table'

PAG_name = 'per_age_girls'
PAG_xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/div[9]/div[11]/table/tbody/tr/td[2]/table'

per_age_boys = load_data(PAB_name, PAB_xpath)
per_age_girls = load_data(PAG_name, PAG_xpath)

# rename ambiguous column
colnames(per_age_boys)[1] = 'Age_category'
colnames(per_age_girls)[1] = 'Age_category'

# construct one table with age categories, boys age at record, girls age at record
library(dplyr)
age_category_days_left_boys = select(per_age_boys, Age_category, Athlete, Age)
age_category_days_left_boys$Gender = 'male'
age_category_days_left_girls = select(per_age_girls, Age_category, Athlete, Age)
age_category_days_left_girls$Gender = 'female'

# combine into one table
age_category_days_left = rbind(age_category_days_left_boys, age_category_days_left_girls)
age_category_days_left$Days_remaining = NA

library(stringr)
# calculate remaining days where athlete is eligible to run in the age category where they set the corresponding record
for (idx in 1:length(age_category_days_left$Age_category)){
  age_days = as.numeric(str_extract(strsplit(toString(age_category_days_left$Age[idx]), ",")[[1]][2], "\\d+"))
  age_category_days_left$Days_remaining[idx] = 365 - age_days
  age_years = as.numeric(str_extract(strsplit(toString(age_category_days_left$Age[idx]), ",")[[1]][1], "\\d+"))
  # if over a year younger than age category
  if (age_years < age_category_days_left$Age_category[idx]){
    years_diff = age_category_days_left$Age_category[idx] - age_years
    age_category_days_left$Days_remaining[idx] = age_category_days_left$Days_remaining[idx] + 365*(years_diff)
  }
}

# graphic representing the time and age, with anomaly explanaition.

# state age hypothesis (with competition cuts)

# calc birthdays of participants, make sure to count every participant only once.