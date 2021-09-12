##########################################
# description:
# This script allows you to download all available 
# waves of the Census Household Pulse survey
# and bind them together into "long" format.
# Sophie Hill, 9/12/21
##########################################

##########################################
# load packages
##########################################
library(rvest) # for reading HTML
library(httr) # for GET request
library(purrr) # for binding lots of datasets together
library(fst) # efficient way to read/write large datasets

##########################################
# web scraping
##########################################

# get all the URLs on this page
census_urls <- GET("https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html") %>%
  read_html() %>% 
  html_elements("a.uscb-layout-align-start-start") %>% 
  html_attr("href") %>% 
  as.character()

# subset to just the CSV files
census_urls <- census_urls[str_detect(census_urls, "CSV.zip")]
census_urls <- paste0("https:", census_urls)

# file names
census_filenames <- unlist(lapply(str_split(census_urls, "HPS_"), function(x)(x[2])))

# download each file
for (i in 1:length(census_urls)){
  download.file(census_urls[i], 
                destfile = paste0("Data/Census_HPS/Raw/",census_filenames[i]))
}

# unzip
for (i in 1:length(census_urls)){
  unzip(paste0("Data/Census_HPS/Raw/",census_filenames[i]), exdir="Data/Census_HPS/Raw/")
}

# select the raw data
# (not the weights files or the data dictionaries)
file_list <- list.files("Data/Census_HPS/Raw/")
file_list <- file_list[str_detect(file_list, ".csv") & !str_detect(file_list, "repwgt")]

# bind together
hps <- paste0("Data/Census_HPS/Raw/", file_list) %>% 
  lapply(read_csv) %>% 
  bind_rows()

head(hps)
table(hps$WEEK)
names(hps)
table(hps$ANXIOUS)

hps %>% 
  select(WEEK, ANXIOUS) %>% 
  mutate(anxious_mostdays = case_when(ANXIOUS<0 ~ NA_real_,
                                          ANXIOUS %in% c(1,2) ~ 0,
                                          ANXIOUS %in% c(3,4) ~ 1)) %>%
  group_by(WEEK) %>%
  summarize(mean_anxious_mostdays = mean(anxious_mostdays, na.rm=TRUE)) %>%
  ggplot(aes(x=WEEK, y=mean_anxious_mostdays*100)) + 
  geom_line() +
  theme_minimal() +
  xlab("HPS wave") +
  ylab("") +
  ylim(0, 40) +
  labs(title = "% feeling anxious most days last week", 
       subtitle = "Unweighted (due to laziness)",
       caption = "Source: Census Household Pulse survey")
