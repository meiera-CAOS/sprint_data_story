# install.packages("rvest")
library(rvest)

url = "https://en.wikipedia.org/wiki/100_metres#100_metres_per_age_category"

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

library(stringr)
age_category_days_left$Days_remaining = NA
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
  (unique(links)) # remove duplicate entries
}


# add links column and match links to athlete
add_athlete_href = function(data_table, links){
  data_table$Link = NA
  for (idx in (1:length(data_table$Athlete))){
    athlete = data_table$Athlete[idx]
    name = strsplit(toString(athlete), " ")[[1]][1]
    matching_link = links[grep(name, links)]
    if(identical(matching_link, character(0))){next}
    data_table$Link[idx] = links[grep(name, links)]
  }
  return(data_table)
}

ATB_men_links = hrefs_in_table(2)
ATB_women_links = hrefs_in_table(3)
all_time_best_men_cleaned2 = add_athlete_href(all_time_best_men_cleaned, ATB_men_links)
all_time_best_women_cleaned2 = add_athlete_href(all_time_best_women_cleaned, ATB_women_links)

# one link didn't match pattern - insert missing
missing_link = which(all_time_best_women_cleaned2$Athlete == "Sha'Carri Richardson")
all_time_best_women_cleaned2$Link[missing_link] = "https://en.wikipedia.org/wiki/Sha%27Carri_Richardson"

# web scrawl for birthdays to be able to compute age at record
add_athlete_birthday = function(data_table){
  data_table$Born = NA
  for (idx in (1:length(data_table$Athlete))){
    pg = read_html(data_table$Link[idx])
    info_box <- html_nodes(pg, xpath="//table[@class = 'infobox vcard']")
    bday_node <- html_nodes(info_box, xpath = "//span[@class = 'bday']")
    bday = html_text(bday_node)[1]
    data_table$Born[idx] = bday
  }
  return(data_table)
}

all_time_best_men_cleaned_bday = add_athlete_birthday(all_time_best_men_cleaned2)
all_time_best_women_cleaned_bday = add_athlete_birthday(all_time_best_women_cleaned2)

# manually adding missing entries for female birthdates, where birthdate not accessible in infobox:
all_time_best_women_cleaned_bday$Born[c(4, 9, 11, 15, 21, 25)] = 
  c("1986-12-27", "1984-04-16",  "1957-04-15", "1966-03-27", "1972-06-12", "1966-11-19")

# else some date conversions returned NA
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

# split functions
standart_date_format_2 = function(data_table){
  data_table[] = lapply(data_table, as.character)
  for (idx in (1:length(data_table$Date))){
    date = as.Date(data_table$Date[idx], format("%d %B %Y"))
    data_table$Date[idx] = toString(date)
  }
  return(data_table)
}

# add age at record: convert date of record into yyyy-mm-dd format and calc age.
standart_date_format = function(data_table){
  data_table[] = lapply(data_table, as.character)
  data_table$Age_at_record = NA
  for (idx in (1:length(data_table$Date))){
    date = as.Date(data_table$Date[idx], format("%d %B %Y"))
    data_table$Date[idx] = toString(date)
    data_table$Age_at_record[idx] = 
      as.numeric(difftime(date, as.Date(data_table$Born[idx]), units = "weeks"))/52.25
  }
  return(data_table)
}

GOATm_standardized = standart_date_format(all_time_best_men_cleaned_bday)
GOATf_standardized = standart_date_format(all_time_best_women_cleaned_bday)

# create common table (could do so earlier) and call functions once
GOATm_standardized$Gender = "male"
GOATf_standardized$Gender = "female"

# combine into one table, remove links column
GOAT = subset(rbind(GOATm_standardized, GOATf_standardized), select = -Link)

# analyze age at time of records
avg_age_f = mean(filter(GOAT, Gender == "female")$Age_at_record)
avg_age_m = mean(filter(GOAT, Gender == "male")$Age_at_record)
avg = mean(GOAT$Age_at_record)
stdev_f = sd(filter(GOAT, Gender == "female")$Age_at_record)
stdev_m = sd(filter(GOAT, Gender == "male")$Age_at_record)
stdev = sd(filter(GOAT, Gender == "female")$Age_at_record)


# boxplot(filter(GOAT, Gender == "female")$Age_at_record ~ filter(GOAT, Gender == "male")$Age_at_record)
boxplot(filter(GOAT, Gender == "female")$Age_at_record)
boxplot(filter(GOAT, Gender == "male")$Age_at_record)


# boxplots of math by prog, with jittered data points
# install.packages("ggplot2")
library(ggplot2)

ggplot(data=GOAT, aes(x=Gender, y=Age_at_record)) +
  geom_boxplot() +
  geom_jitter(width=.05)

# x = seq(0, 40, 0.5)
# plot(x, dnorm(x, mean = avg, sd = stdev), type = "l", ylim = c(0, 0.3), ylab = "", lwd = 2, col = "red")

# state age hypothesis (with seasons and age categories)
# is there a "best" month to be born - cutoff hypothesis. find mean and standard deviation compare to normal distrib there
# calc birthdays of participants, make sure to count every participant only once.

# get birthmonth
# install.packages("lubridate")
library(lubridate)
# month(as.Date(GOAT$Born[1]))
# TODO gather tables

per_age_girls_std = standart_date_format_2(per_age_girls)
per_age_boys_std = standart_date_format_2(per_age_boys)

juniors_birth_month = select(rbind(per_age_boys, per_age_girls), Athlete, Date, Age)
juniors_birth_month = standart_date_format_2(juniors_birth_month)

# set junior flag,  calc birthmonth, remove duplicates
juniors_birth_month$Month = NA
for (idx in (1:length(juniors_birth_month$Date))){
  date = as.Date(juniors_birth_month$Date[idx], format("%d %B %Y"))
  age_days = as.numeric(str_extract(strsplit(toString(juniors_birth_month$Age[idx]), ",")[[1]][2], "\\d+"))
  juniors_birth_month$Month[idx] = month(as.Date(juniors_birth_month$Date[idx])+(365-age_days))
}

juniors_just_month = unique(select(juniors_birth_month, Athlete, Month))
juniors_just_month$Category = "junior"

# calc age of young athletes and check if there's a trend in birthmonth
GOAT$Month = NA
for (idx in (1:length(GOAT$Born))){
  GOAT$Month[idx] = month(as.Date(GOAT$Born[idx]))
}
seniors_just_month = select(GOAT, Athlete, Month)
seniors_just_month$Category = "senior"

# common table
just_month = rbind(juniors_just_month, seniors_just_month)

# barplot birthmonth
tab = table(just_month$Category, just_month$Month)
barplot(tab, 
        legend.text = TRUE, xlab = "Birth month", ylab = "Athletes", args.legend = list(x = "topleft"))

# probability of getting this data with random birthdays
month_avg = mean(just_month$Month) # expected 6.5
month_var = var(just_month$Month) # expected 10.083
# install.packages("ExtDist")
# TODO: what is the takeaway?
library(ExtDist)
len = length(just_month$Month)
mle_uniform = eUniform(X = just_month$Month, method = "unbiased.MLE")

#TODO: remove intermediary dataframes (overwirte the same)



