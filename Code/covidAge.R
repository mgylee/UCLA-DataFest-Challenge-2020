library(readr)
library(tidyverse)
library(lubridate)
library(streamgraph)
library(htmlwidgets)

ue = read_csv("datafest_ue.csv")
age_data = ue[, 1:15]
age_data$rptdate = mdy(age_data$rptdate) # simplify your date column with lubridate

date1 = as.POSIXct("2018-12-01")
date2 = as.POSIXct("2020-05-01")
covid_interval = interval(date1, date2) # timeframe of recession was from 12/2007 to 6/2009
age2020 = age_data %>% filter(rptdate %within% covid_interval)

simplified = age2020 %>% group_by(rptdate) %>% 
  summarise(total = sum(c12, c13, c14, c15, c16, c17, c18, c19, c20),
            `< 22` = sum(c12),
            `23-34` = sum(c13) + sum(c14),
            `35-44` = sum(c15),
            `45-59` = sum(c16) + sum(c17),
            `>= 60` = sum(c18) + sum(c19),
            INA = sum(c20))
simpledata = pivot_longer(simplified, cols = `< 22`:`>= 60`, 
                          names_to = "AgeGroup", 
                          values_to = "Unemployed")
simpledata$proportion = simpledata$Unemployed/simpledata$total

# Streamgraph for 2019-present
levelstream = c(c(">= 60", "45-59", "35-44", "23-34", "< 22"))
streamorder = factor(simpledata$AgeGroup, levels = levelstream)
simpledata$AgeGroup2 = streamorder
mystream <- streamgraph(simpledata, key="AgeGroup2", 
                        value="Unemployed", date="rptdate",
                        offset="zero", interpolate = "linear",
                        height="700px", width="1000px", interactive = TRUE) %>% 
  sg_legend(show=TRUE, label = "Age Group: ") %>% 
  sg_axis_y(tick_count = 6, tick_format = "d") %>%
  sg_axis_x(tick_units = "rptdate",
            tick_format = "%b %Y")

mystream
saveWidget(mystream, file="covidstream.html")

#############################################################################################
# Great Recession Data
grdate1 = as.POSIXct("2007-12-01")
grdate2 = as.POSIXct("2009-07-01")
gr_interval = interval(grdate1, grdate2) # timeframe of recession was from 12/2007 to 6/2009
great_recession = age_data %>% filter(rptdate %within% gr_interval)

simplified2 = great_recession %>% 
  group_by(rptdate) %>% 
  summarise(total = sum(c12, c13, c14, c15, c16, c17, c18, c19, c20),
            `< 22` = sum(c12),
            `23-34` = sum(c13) + sum(c14),
            `35-44` = sum(c15),
            `45-59` = sum(c16) + sum(c17),
            `>= 60` = sum(c18) + sum(c19),
            INA = sum(c20))
simpledata2 = pivot_longer(simplified2, cols = `< 22`:`>= 60`, 
                           names_to = "AgeGroup", 
                           values_to = "Unemployed")
# Streamchart
levelstream = c(c(">= 60", "45-59", "35-44", "23-34", "< 22"))
streamorder = factor(simpledata2$AgeGroup, levels = levelstream)
simpledata2$AgeGroup2 = streamorder
mystream2 <- streamgraph(simpledata2, key="AgeGroup2", 
                         value="Unemployed", date="rptdate", 
                         offset="zero", interpolate = "linear", 
                         height="700px", width="1000px", interactive = TRUE) %>%
  sg_legend(show = TRUE, label = "Age Group: ") %>%
  sg_axis_x(19, "rptdate", "%b %Y") %>% 
  sg_axis_y(tick_format = "d") 

mystream2
saveWidget(mystream2, file="recession2008.html")

