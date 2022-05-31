##########################################
# description:
# This script allows you to download all available
# waves of the Census Household Pulse survey
# and bind them together into "long" format.
# Sophie Hill, 9/12/21
# Last updated 5/30/22
##########################################

##########################################
# load packages
##########################################
library(tidyverse)
library(rvest) # for reading HTML
library(httr) # for GET request
library(purrr) # for binding lots of datasets together
library(fst) # efficient way to read/write large datasets
library(srvyr) # for weighted means
library(stringr) # for parsing file names

##########################################
# web scraping
##########################################

# set file path - change to wherever you want to download the raw files
# (N.B. there are a lot so best to create a new subfolder!)
my_file_path <- "Data/Census_HPS/Raw/"

# First grab all the hyperlinks, then filter to the relevant ones
census_urls <-
  GET("https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html") %>%
  read_html() %>%
  html_elements("a") %>%
  html_attr("href") %>%
  as.character()

# subset to just the CSV files
census_urls <-
  census_urls[str_detect(census_urls, "CSV.zip")] %>% na.omit()
census_urls <- paste0("https:", census_urls)

# some URLs have start with "https:https://" instead of just "https://"
# let's fix that:
census_urls <- gsub("https:https://", "https://", census_urls)

# file names
census_filenames <-
  unlist(lapply(str_split(census_urls, "HPS_"), function(x)
    (x[2])))

# download each file
for (i in 1:length(census_urls)) {
  download.file(census_urls[i],
                destfile = paste0(my_file_path, census_filenames[i]))
}

# unzip
for (i in 1:length(census_urls)) {
  unzip(paste0(my_file_path, census_filenames[i]), exdir = my_file_path)
}

# select the raw data
# (not the weights files or the data dictionaries)
file_list <- list.files(my_file_path)
file_list <-
  file_list[str_detect(file_list, ".csv") &
              !str_detect(file_list, "repwgt")]

# bind together
hps <- paste0(my_file_path, file_list) %>%
  lapply(read_csv) %>%
  bind_rows()
# save file
write_fst(hps, "hps_combined.fst")

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
