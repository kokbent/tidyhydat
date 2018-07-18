library(tidyhydat)
library(tidyverse)
library(lubridate)
library(waterData)

#station to analyze
station = '02323500'   
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
# dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01")

# importDVs is slow when it needed to pull so much data. So here's some code to dynamically update
# and store the data (TODO: write a wrapper function for it)
dis <- read_rds("data/dis1.csv")
if (max(dis$dates) < (Sys.Date() - 5)) {
  sdate <- max(dis$dates) + 1
  newdis <- importDVs(staid = station, code = '00060', stat = '00003', sdate= as.character(sdate))
  dis <- bind_rows(dis, newdis) %>%
    distinct() # In case there's repeated rows due to code failure
  write_rds(dis, "data/dis1.csv")
}

#Changing the format of the dates to be able to plot against time
# dates column in dis is already date object. Not needed, but can run it to double confirm
# dis$Date <- as.Date(dis$dates,origin= "1899-12-30")
dis$dates <- ymd(dis$dates)

# dis_flow <- dis %>%
#   mutate(dayofyear = yday(Date), Year = year(Date)) %>%
#   filter(dayofyear %in% yday(seq.Date(from = (Sys.Date()-365), 
#                                       to = Sys.Date(), by = "day"))) %>%
#   group_by(dayofyear) %>%
#   mutate(prctile = ecdf(val)(val)) %>%
#   mutate(Date_no_year = dmy(paste0(day(Date),"-",month(Date),"-",year(Sys.Date())))) %>%
#   ungroup()
# Depends if we're discarding Feb 29 or discarding 366th dat of the year. 
# Following code discard Feb 29.
dis_flow <- dis %>%
  filter(!(month(dates) == 2 & day(dates) == 29)) %>%
  mutate(md = strftime(dates, format = "%m-%d")) %>%
  group_by(md) %>%
  mutate(prctile = ecdf(val)(val)) %>%
  ungroup

# double check
dis_flow %>%
  group_by(md) %>%
  summarise(med = median(prctile))

k

dis_time<- dis%>%
  mutate(Date_day = as.Date(Date)) %>%
  filter(Date_day %in% day(seq.Date(from = (Sys.Date()-30), 
                                    to = Sys.Date(), by = "day"))) %>%
  group_by(Date_day) %>%
  summarise(val = mean(val, na.rm = TRUE)) %>%
  ungroup()  


windows()
ggplot(dis_flow, aes(x = dates, y = val)) +
  geom_point(aes(colour = prctile)) +
  #geom_line(data = dis_time, aes(x = Date_day), colour = "black") +
  #geom_point(data = dis_time, aes(x = Date_day, shape = factor(year(Date_day))), colour = "black") +
  scale_colour_gradientn(name = "Discharge Percentile", colours = rainbow(10)) +
  scale_shape_discrete(name = "Year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Date", y = "Discharge (ft^3/s)")



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




dis_flow <- dis %>%
  mutate(dayofyear = yday(Date), Year = year(Date)) %>%
  filter(dayofyear %in% yday(seq.Date(from = (Sys.Date()-365), 
                                      to = Sys.Date(), by = "day"))) %>%
  group_by(dayofyear) %>%
  mutate(prctile = ecdf(val)(val)) %>%
  mutate(Date_no_year = dmy(paste0(day(Date),"-",month(Date),"-",year(Sys.Date())))) %>%
  ungroup()


windows()
dis %>%
  mutate(dayofyear = yday(Date), Year = year(Date)) %>%
  mutate(dayofyear_formatted = as.Date(dayofyear - 1, origin = "1950-01-01")) %>% 
  mutate(prctile = ecdf(val)(val)) %>%
  ggplot(aes(x = dayofyear_formatted, y = Year, fill = prctile)) +
  geom_tile() +
  scale_x_date(date_labels = "%b") +
  scale_y_reverse(expand = c(0, 0), breaks= c(1950,1960,1970, 1980,1990,2000,2010)) +
  scale_fill_gradientn(name = "Percentile ", colours = rainbow(20)) +
  labs(y = "Year", x = "Date")+ 
  theme(text = element_text(size=20))

ggsave("quantile_tile.png", width= 10, height= 10)
