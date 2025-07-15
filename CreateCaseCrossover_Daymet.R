# 1_create_data_crossover_synthetic.R


# Author : Kristen Cowan 
# Imports : rjutils [remotes::install_github(jeremieboudreault/rjutils)]


# Libraries --------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(remotes)
library(raster)
library(stringr)
library(dplyr)
library(sf)
library(rnaturalearth)
library(tidyr)

install_github("jeremieboudreault/rjutils", force=TRUE)

# PART A - Data exploration ----------------------------------------------------

# Load data.
load("SyntheticData_FullYear.Rda")

#Loading daymet data
daymet<-read.csv("daymet_data_byinstid_2009_2021.csv", sep=";")

daymet <- daymet %>% 
  rename(tmax_daymet = T_MAX)%>% 
  rename(tmean_daymet = T_MEAN)%>% 
  rename(tmin_daymet = T_MIN)%>% 
  rename(wvpm_daymet = WVP_MEAN)%>% 
  rename(relhm_daymet = RELH_MEAN)%>% 
  rename(date = DATE)

#removing variables that aren't needed
daymet <- subset(daymet, select = -c(MONTH, YEAR, DOY))
analytic_data_temp$date<-as.character(analytic_data_temp$date)

#Linking daymet data in with original case crossover data
merge<-merge(x = analytic_data_temp, y = daymet, by =c("InstID", "date"), all.x=TRUE)


# Convert to data.table to ease data manipulation.
x <- as.data.table(merge)

# Columns names.
colnames(x) 

# Remove duplicated lines (I don't know, but I have found some)
x[, .N]
x <- x[, .(death = sum(death)), by = c("InstID", "date", "month", "day", "year", "doy", "dow",  "tmean", "Region", "Medical_Center", "Long", "Lat_num", "tmean_daymet", "relhm_daymet")]
x[, .N]

# Prison locations.
x[, .N, by = c("InstID")]
length(unique(x$InstID)) # We have 162 different prison.

# Data span.
length(unique(x$year)) # We have 13 years.

# Month.
length(unique(x$month)) # January to December.

# Number of deaths across all prison.
x[, sum(death), by = "InstID"] # Some prison has no death. Should we remove them ?
x[, sum(death), by = "year"] # Approximately 400 deaths per year
x[, sum(death)] # A total of 5,182 cases


# PART B - Create dataset for case-crossover design ----------------------------

# Get rid of prison with no deaths.#No prisons have 0 deaths, this only occured in the synthetic data 
x <- x[InstID %in% x[, sum(death) > 0, by = "InstID"][V1 == TRUE, InstID], ]
length(unique(x$InstID)) # Remaining 162 prisons.
sum(x$death) # Still have 5182 deaths

# Extracts data with death and not death
x_death <- analytic_data_temp[death >= 1L, ]

# Summarize deaths by InstID, month, year, dow.
x_death <- x_death[, .(death = sum(death)), by = c("InstID", "date", "year", "month", "dow", "doy")]
x_death[, sum(death)] # 5182

# Extend the data set to each row is one death.
x_death <- x_death[rep(1:.N, death), ][, death := 1L][]
nrow(x_death) == sum(x_death$death)

# Add ID for each death.
x_death[, DEATH_ID := 1:.N]

#Subsetting to just dates and IDs
one<-subset(x_death, select=c(DEATH_ID, date))

# Select non-event days within the same calendar month and year as the event date
two <- one %>%
  mutate(
    date = as.Date(date), # Convert date to Date object
    minus28 = date - 28,
    minus21 = date - 21,
    minus14 = date - 14,
    minus7 = date - 7,
    plus0= date + 0,
    plus7 = date + 7,
    plus14 = date + 14,
    plus21 = date + 21,
    plus28 = date + 28
  )

# Transpose the data
three <- pivot_longer(two, cols = starts_with("plus") | starts_with("minus"), names_to = "event_offset", values_to = "date2")

# Filter out control dates not in the same month as the event
four <- three %>%
  mutate(
    date2 = as.Date(date2), # Convert date to Date object
    event_date = as.Date(date), # Convert event date to Date object
    same_month = month(date2) == month(event_date),
    event = ifelse(date2 == event_date, 1, 0)
  ) %>%
  dplyr::filter(same_month) %>%
  dplyr::select(-event_date, -same_month, -event_offset)

# Add a unique identifier for each row
five <- four %>%
  group_by(DEATH_ID) %>%
  mutate(num = row_number()) %>%
  mutate(study_id2 = paste(DEATH_ID, num, sep = "_")) %>%
  dplyr::select(-num)

#Merging in new rows with original data 
mortality_cacross<-merge(x = five, y = x_death, by = c("DEATH_ID", "date"), all.x=TRUE)

#Dropping unnecessary variables 
mortality_cacross<-subset(mortality_cacross, select=-c(study_id2, death))
mortality_cacross<-mortality_cacross %>%
  rename("death"="event")
mortality_cacross<-mortality_cacross %>%
  rename("dod"="date")
mortality_cacross<-mortality_cacross %>%
  rename("date"="date2")

# PART C - Bring lagged temperature data ---------------------------------------


# Extract temperature.
x_temp <- x[order(InstID, date), .(InstID, date, tmean_daymet, relhm_daymet)]

# Validate the resulting number of rows.
length(unique(x_temp$InstID)) * length(seq(as.Date("2009-01-01"), as.Date("2021-12-31"), by = 1L))
x_temp[, .N]

# Create lagged var up to 21 days prior to the observed period.
for (instid in unique(x_temp$InstID)) {
  print(instid)
  for (lag in 0:21) {
    x_temp[InstID == instid, paste0("tmean_daymet", lag) := rjutils::create_lagged_var(tmean_daymet, date, lag, lag, check_dates = FALSE)]
    x_temp[InstID == instid, paste0("relhm_daymet", lag) := rjutils::create_lagged_var(relhm_daymet, date, lag, lag, check_dates = FALSE)]
  }
}

# Get rid to <tmean>
x_temp[, tmean := NULL]

# Bring temperature to x_crossover.
x_crossover_temp <- merge(mortality_cacross, x_temp, by = c("InstID", "date"), all.x = TRUE)


# PART D - Add other information such as region and medical center -------------


# Summarize x by <InstID>
x_reginfo <- unique(x[, .(InstID, Region, Medical_Center)])

# Merge information.
case_cross_final <- merge(x_crossover_temp, x_reginfo, by = "InstID", all.x = TRUE)


# Export final dataset ---------------------------------------------------------


save(case_cross_final, file="TempMortalityCaseCrossoverFinal_Daymet.Rda")


