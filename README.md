## 100 meters sprint data story
# About
A data story about the 100 meters sprint records and the athletes ages. Data scraped from [Wikipedia](https://en.wikipedia.org/wiki/100_metres#100_metres_per_age_category).
- data_story.Rmd: The final code used to generate the data_story.html and pdf files. Contains cleaned up code from web_scraper.R
- web_scraper.R: Included just for completeness, code base for data_story.
- data_story.html / data_story.pdf: The report in two different formats
- data folder: contains csv of the scraped data, folder structure is required - data files not.
# Requirements
- anaconda 2020.11
- R 4.0.3 with packages:
    - rvest, dplyr, stringr, ggplot2, lubridate and ExtDist.