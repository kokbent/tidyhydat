library(tidyhydat)
library(dplyr)
library(ggplot2)
library(lubridate)
library(waterData)
library("viridis")


#station to analyze
station = '02323500'   
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01")


#Changing the format of the dates to be able to plot against time
dis$Date <- as.Date(dis$dates,origin= "1899-12-30")


dis_flow <- dis %>%
  mutate(dayofyear = yday(Date), Year = year(Date)) %>%
  filter(dayofyear %in% yday(seq.Date(from = (Sys.Date()-365), 
                                      to = Sys.Date(), by = "day"))) %>%
  group_by(dayofyear) %>%
  mutate(prctile = ecdf(val)(val)) %>%
  mutate(Date_no_year = dmy(paste0(day(Date),"-",month(Date),"-",year(Sys.Date())))) %>%
  ungroup()


dis_time<- dis%>%
  mutate(Date_day = as.Date(Date)) %>%
  filter(Date_day %in% day(seq.Date(from = (Sys.Date()-365), 
                                    to = Sys.Date(), by = "day"))) %>%
  group_by(Date_day) %>%
  summarise(val = mean(val, na.rm = TRUE)) %>%
  ungroup()  



ggplot(dis_flow, aes(x = Date_no_year, y = val)) +
  geom_point(aes(colour = prctile)) +
  #geom_line(data = dis_time, aes(x = Date_day), colour = "black") +
  #geom_point(data = dis_time, aes(x = Date_day, shape = factor(year(Date_day))), colour = "black") +
  scale_colour_gradientn(name = "Discharge Percentile", colours = rainbow(10)) +
  scale_shape_discrete(name = "Year") +
  theme_minimal() +
  labs(title = "Historical flow relative to current year",
       x = "Date", y = "Discharge (ft^3/s)")



ggplot(pct_flow, aes(x = Date_no_year, y = Value)) +
  geom_point(aes(colour = prctile)) +
  geom_line(data = nunavut_realtime, aes(x = Date_day), colour = "black") +
  geom_point(data = nunavut_realtime, aes(x = Date_day, shape = factor(year(Date_day))), colour = "black") +
  scale_colour_gradientn(name = "Discharge Percentile", colours = rainbow(10)) +
  scale_shape_discrete(name = "Year") +
  theme_minimal() +
  labs(title = "Historical flow relative to current year",
       subtitle = "Current year flows are displayed in black",
       caption = "Real time data is presents AS IS and represents unapproved data",
       x = "Date", y = "Discharge (m^3/s)")


dis %>%
  mutate(dayofyear = yday(Date), Year = year(Date)) %>%
  mutate(dayofyear_formatted = as.Date(dayofyear - 1, origin = "1950-01-01")) %>% ## leap year as placeholder
  ggplot(aes(x = dayofyear_formatted, y = val, colour = Year)) +
  #scale_colour_gradientn(name = "Discharge Percentile", colours = rainbow(10)) +
  geom_line() +
  scale_x_date(date_labels = "%b %d") +
  labs(y = "Discharge (ft^3/s)") +
  theme_minimal()



windows()
dis %>%
  mutate(dayofyear = yday(Date), Year = year(Date)) %>%
  mutate(dayofyear_formatted = as.Date(dayofyear - 1, origin = "1950-01-01")) %>% 
  ggplot(aes(x = dayofyear_formatted, y = Year, fill = val)) +
  geom_tile() +
  scale_x_date(date_labels = "%b") +
  scale_y_reverse(expand = c(0, 0), breaks= c(1950,1970,1990,2010)) +
  scale_fill_gradientn(name = "Discharge (ft^3/s) ", colours = rainbow(20)) +
  labs(y = "Year", x = "Date") +
  theme_minimal() +
  theme(legend.position="right")
