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
    write.csv(loaded_data, file=data_filepath)
  } else {
    loaded_data = read.csv(data_filepath)
  }
  return(loaded_data)
}

PAB_name = 'per_age_boys'
PAB_xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/div[9]/div[11]/table/tbody/tr/td[1]/table'

PAG_name = 'per_age_girls'
PAG_xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/div[9]/div[11]/table/tbody/tr/td[2]/table'

per_age_boys = load_data(PAB_name, PAB_xpath)
per_age_girls = load_data(PAG_name, PAG_xpath)

