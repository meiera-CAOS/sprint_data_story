# install.packages("rvest")
library(rvest)

url = "https://en.wikipedia.org/wiki/100_metres#100_metres_per_age_category"
# url2 = "html(https://en.wikipedia.org/wiki/100_metres#100_metres_per_age_category)"

# TODO: generalize for different types of data?
# load data from file directly if already scraped data and scrape and write csv file otherwise
load_data = function(data_name, data_xpath){
  data_filepath = paste("~/sprint/data/", data_name, ".csv", sep = "")
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

PAB_name = "per_age_boys"
PAB_xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/div[9]/div[11]/table/tbody/tr/td[1]/table"

PAG_name = "per_age_girls"
PAG_xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/div[9]/div[11]/table/tbody/tr/td[2]/table"

per_age_boys = load_data(PAB_name, PAB_xpath)
per_age_girls = load_data(PAG_name, PAG_xpath)

# rename ambiguous column
colnames(per_age_boys)[1] = "Age_category"
colnames(per_age_girls)[1] = "Age_category"

# construct one table with age categories, boys age at record, girls age at record
library(dplyr)
age_category_days_left_boys = select(per_age_boys, Age_category, Athlete, Age)
age_category_days_left_boys$Gender = "male"
age_category_days_left_girls = select(per_age_girls, Age_category, Athlete, Age)
age_category_days_left_girls$Gender = "female"

# combine into one table
age_category_days_left = rbind(age_category_days_left_boys, age_category_days_left_girls)
age_category_days_left$Days_remaining = NA

library(stringr)
# calculate remaining days where athlete is eligible to run in the age category where they set the corresponding record
for (idx in 1:length(age_category_days_left$Age_category)){
  age_days = as.numeric(str_extract(strsplit(toString(age_category_days_left$Age[idx]), ",")[[1]][2], "\\d+"))
  age_category_days_left$Days_remaining[idx] = 365 - age_days
  age_years = as.numeric(str_extract(strsplit(toString(age_category_days_left$Age[idx]), ",")[[1]][1], "\\d+"))
  # if more than a year younger than age category
  if (age_years < age_category_days_left$Age_category[idx]){
    years_diff = age_category_days_left$Age_category[idx] - age_years
    age_category_days_left$Days_remaining[idx] = age_category_days_left$Days_remaining[idx] + 365*(years_diff)
  }
}

# graphic representing the time and age, with anomaly explanaition.
age_category_days_left$Gender = factor(age_category_days_left$Gender)
plot(age_category_days_left$Age_category, age_category_days_left$Days_remaining, 
     col=age_category_days_left$Gender,
     pch = 17,
     xlab = "Age category",
     ylab = "Days remaining",
     main = "Plot of age category vs athletes days remaining to compete in this category")
legend("topleft", legend=levels(age_category_days_left$Gender), col=c(1:2), pch = 17)

# explain outlier
# Holds records in 5 age categories, set records for 11 and 12 at age 11, 2019, 
# assumption: unable to compete in 2020 due to covid (and would have improved her time) 
outlier_per_age_girls = select(filter(per_age_girls, Athlete == "Payton Payne"), Age_category, Athlete, Date, Age)
print(outlier_per_age_girls)

# remove outlier and analyze (fit, avg,...) add half year avg. line
outlier_row = which(age_category_days_left$Days_remaining > 365)
age_category_days_left_cleaned = age_category_days_left[-c(outlier_row),]

# graphic representing the time and age, with anomaly explanaition.
age_category_days_left_cleaned$Gender = factor(age_category_days_left_cleaned$Gender)
plot(age_category_days_left_cleaned$Age_category, age_category_days_left_cleaned$Days_remaining, 
     col=age_category_days_left_cleaned$Gender,
     pch = 17,
     xlab = "Age category",
     ylab = "Days remaining",
     main = "Plot of age category vs athletes days remaining to compete in this category", 
     ylim = c(0, 365))
legend("topleft", legend=levels(age_category_days_left_cleaned$Gender), col=c(1:2), pch = 17)

# linear regression
lr_age_category_DL_cleaned = lm(Days_remaining ~ Age_category, data = age_category_days_left_cleaned) 
abline(h = 365/2, col = "purple")  # half a year line
abline(a = coef(lr_age_category_DL_cleaned)[1], b = coef(lr_age_category_DL_cleaned)[2], col="blue")

# state age hypothesis (with seasons and age categories)
# is there a "best" month to be born - cutoff hypothesis. find mean and standard deviation compare to normal distrib there
# calc birthdays of participants, make sure to count every participant only once.

# best age for records men and women
# xpath to all time top 25 athletes.
ATBW_name = "all_time_best_women"
ATBW_xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/div[9]/table[2]"

ATBM_name = "all_time_best_men"
ATBM_xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/div[9]/table[1]"

all_time_best_women = load_data(ATBW_name, ATBW_xpath)
all_time_best_men = load_data(ATBM_name, ATBM_xpath)

# clean tables - keeping every athlete exactly once.
all_time_best_women_cleaned = select(all_time_best_women[-c(26,27),], Athlete, Date)
all_time_best_men_cleaned = select(filter(all_time_best_men, !grepl("\\d", all_time_best_men$Athlete)), Athlete, Date)

# all href in table with tableNr
hrefs_in_table = function(tableNr){
  links = url %>%
    read_html() %>%
    html_nodes('table.wikitable') %>%
    .[[tableNr]] %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    paste0('https://en.wikipedia.org', .)
  (unique(links)) # no more duplicates
}

ATB_men_links = hrefs_in_table(2)
ATB_women_links = hrefs_in_table(3)

# add links column and match links to athlete
add_athlete_href = function(data_table, links){
  data_table$Link = NA
  for (idx in (1:length(data_table$Athlete))){
    athlete = data_table$Athlete[idx]
    name = strsplit(toString(athlete), " ")[[1]][1]
    if(name == "Sha'Carri"){
      data_table$Link[idx] = "https://en.wikipedia.org/wiki/Sha%27Carri_Richardson"
      next
    } 
    matching_link = links[grep(name, links)]
    if(is.null(matching_link)){print(athlete)}
    data_table$Link[idx] = links[grep(name, links)]
  }
  return(data_table)
}

all_time_best_men_cleaned2 = add_athlete_href(all_time_best_men_cleaned, ATB_men_links)
all_time_best_women_cleaned2 = add_athlete_href(all_time_best_women_cleaned, ATB_women_links)

# web scrawl for birthdays to be able to compute age at record

#all_time_best_men_cleaned$Born = NA
#all_time_best_men_cleaned$Age_at_record = NA

# # all links in ATB women
# ATB_men_links = url %>%
#   read_html() %>%
#   html_nodes('table.wikitable') %>%
#   .[[3]] %>%
#   html_nodes('a') %>%
#   html_attr('href') %>%
#   paste0('https://en.wikipedia.org', .)
# TODO, generalize load function with href flag

# follow href and find age.

