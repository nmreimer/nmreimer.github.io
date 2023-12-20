library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(geeM)

data1999 <- read_csv("Seychelles1999.csv", locale=locale(encoding="latin1"))
colnames <- data1999[1,]
colnames(data1999) <- colnames
data1999 <- data1999[-c(1),]


loadFolder <- function(foldername) {
  names <- list.files(path=foldername, pattern=".csv")
  data <- paste0(foldername, "/", names) %>%
    lapply(read_csv,locale=locale(encoding="latin1"),skip=1) %>%
    bind_rows
  colnames(data) <- colnames
  data <- data %>%
    mutate(Date = as.Date(`Date (MM/DD/YYYY)`, format = "%m/%d/%Y"))%>%
    mutate(Year = year(Date))%>%
    mutate(Month = month(Date))%>%
    select(Date, Year, Month, everything())%>%
    select(-`Date (MM/DD/YYYY)`)
  return(data)
}

toDailyData <- function(data, lat) {
  daily_data <- data %>% 
    group_by(Date) %>% 
    summarize(GHI_sum = sum(`GHI (W/m^2)`), 
              RHum_mean = mean(`RHum (%)`), 
              DryBulb_mean = mean(`Dry-bulb (C)`), 
              Wspd_mean = mean(`Wspd (m/s)`), 
              Alb_mean = mean(`Alb (unitless)`),
              RHum_max = max(`RHum (%)`),
              RHum_sd = sd(`RHum (%)`),
              DryBulb_max = max(`Dry-bulb (C)`), 
              Wspd_max = max(`Wspd (m/s)`), 
              Alb_max = max(`Alb (unitless)`)) %>%
    mutate(YDay = yday(Date)) %>%
    mutate(Year_Day = update(Date, year = 0),
           Latitude = lat) %>% 
    mutate(TotalDay = (interval(start = data$Date[1], end = Date)) / days(1) + 60,
           decimalDate = decimal_date(Date)) %>%
    mutate(Place = case_match(Latitude, -4.625 ~ "Victoria, Seychelles", 53.485 ~ "Manchester, England", -1.275 ~ "Nairobi, Kenya"))
  insolation_path <- paste0(lat,"_dailyinsolation.csv")
  insolation <- read_csv(insolation_path)
  insolation <- insolation %>% mutate(TotalDay = trunc(day))
  daily_data <- daily_data %>% left_join(insolation)
  daily_data <- daily_data[1:9018,]
  daily_data <- daily_data %>% mutate(notlost = 1 - (average_insolation*24 - GHI_sum) / (average_insolation*24))
  return(daily_data)
}

data <- loadFolder("SolarAnywhereData")
kenya_data <- loadFolder("Kenya")
england_data <- loadFolder("England")


daily_data <- toDailyData(data, -4.625)
daily_kenya_data <- toDailyData(kenya_data, -1.275)
daily_england_data <- toDailyData(england_data, 53.485)







