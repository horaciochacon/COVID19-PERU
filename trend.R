library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(ggthemes)

options(scipen=999)

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
url2 <- "https://github.com/jincio/COVID_19_PERU/blob/master/reportes_minsa.xlsx?raw=true"

p1f <- tempfile()
download.file(url2, p1f, mode="wb")

peru <- read_excel(p1f) %>%
  mutate(country = "Peru", Dia = ymd(Dia - days(44))) %>% 
  select(country, date = Dia, freq = Positivos)

covid <- read_csv(url) %>% 
  select(c(2,5:58)) %>% 
  rename(country = "Country/Region") %>% 
  gather("date","freq", -country ) %>% 
  filter(country %in% c("Italy", "Spain", "Korea, South", "US")) %>% 
  group_by(country, date) %>% 
  summarise(freq = sum(freq)) %>% 
  mutate(date = mdy(date), freq = ifelse(freq == 0, NA, freq)) %>% 
  arrange(country,date) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(min = min(date), max = date[which.min(is.na(freq))]) %>% 
  mutate(dias = as.integer(max-min)) %>% 
  mutate(lag.value = lead(freq, n = dias[1], default = NA)) %>%
  select(country, date, freq = lag.value) %>% 
  bind_rows(peru) %>% 
  mutate(days = as.numeric(date - dmy("22-01-20")))
  
  

ggplot(data = covid, aes(x = days, y = freq, group = country,
                         color = country)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) +
  scale_y_log10(breaks = c(1,2,5,10,20,50,100, 200, 500, 1000, 2000, 5000, 
                           10000, 20000, 50000))+
  ggthemes::scale_colour_wsj('colors6', '') +
  ggthemes::theme_hc() +
  ggtitle('Casos acumulados de COVID19') +
  xlab("Dias desde primer caso") +
  ylab("número de casos")
