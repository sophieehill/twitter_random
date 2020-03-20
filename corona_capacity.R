library(tidyverse) # to clean data
library(ggplot2) # to make plots
library(wbstats) # to access the World Bank data API
library(countrycode) # to match countries onto regions
library(ggrepel) # to label points on scatterplot

# first, you need to go to Our World In Data and download the csv files for two charts:
# (data collated by OWID from various country-specific sources, updated as of 17 March 2020)
# 1. https://ourworldindata.org/grapher/tests-vs-confirmed-cases-covid-19
# 2. https://ourworldindata.org/grapher/tests-vs-confirmed-cases-covid-19-per-million

# load the two csv files
# make sure to change the file path based on where you saved them
dt <- read_csv("/Users/sophiehill/Downloads/tests-vs-confirmed-cases-covid-19.csv")
dt2 <- read_csv("/Users/sophiehill/Downloads/tests-vs-confirmed-cases-covid-19-per-million.csv")

# take a peek
head(dt)
head(dt2)
# rename columns
colnames(dt) <- c("country", "ccode", "year", "tests.total", "cases.total")
colnames(dt2) <- c("country", "ccode", "year", "tests.per.mill", "cases.per.mill")
# remove rows with any missing values
dt <- na.omit(dt)
dt2 <- na.omit(dt2)
# merge
dtt <- merge(dt, dt2, by=c("country", "ccode"))
# drop the "year" variable, not even sure what this is?? (possibly indicates when the country-specific data was last updated? but there's no codebook...)
dtt <- dtt %>% select(-starts_with("year"))
# create the new "state capacity" variable: number of tests / number of cases
dtt$tests.per.case <- dtt$tests.total / dtt$cases.total
summary(dtt$tests.per.case)

# search for state capacity type variables in the World Bank dataset
# let's choose the "Government Effectiveness" indicator
state.cap.vars <- wbsearch(pattern = "effectiveness")
state.cap.vars$indicator
# save the World Bank ID for this indicator
gov.eff.id <- state.cap.vars$indicatorID[9]
gov.eff.id

# download the data for this ID
wb.data <- wb(indicator = gov.eff.id)
# looks like 2018 is the most recent update, let's filter
wb.data <- wb.data %>% filter(date==2018)
# select variables
wb.data.x <- wb.data %>% select(iso3c, value, country)
colnames(wb.data.x) <- c("iso3c", "gov.effectiveness.2018", "country.name.wb")

# merge
dtx <- merge(dtt, wb.data.x, by.x="ccode", by.y="iso3c", all.x=T, all.y=F)
head(dtx)

# add continent labels
dtx$region <- countrycode(dtx$ccode, origin="iso3c", destination="continent")
table(dtx$region)

dtx %>% filter(tests.per.case<400) %>% # remove outliers
  ggplot(aes(x=gov.effectiveness.2018, y=tests.per.case, color=region, label=country)) + 
  geom_point() + 
  geom_text_repel() + 
  theme_minimal() +
  theme(legend.position="none") +
  ggtitle("COVID-19 response vs measures of state capacity") +
  xlab("Government effectiveness (Source: World Bank, 2018)") +
  ylab("# COVID-19 tests / # COVID-19 confirmed cases")
  
# save graph
ggsave(filename="corona_capacity.png", width=8, height=4)
