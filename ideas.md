---
title: "Stats 112 Final Project"
author: "Edwin Reyes Herrera, Emma Higgins, Max Danielewicz, Will Hamlin"
date: "11/1/2019"
output: 
  html_document:
    keep_md: yes
    df_print: paged
    toc: true
    toc_float: yes
---


```r
library(gganimate)
library(ggmap)
library(ggridges)
library(ggthemes)
library(knitr)
library(leaflet)
library(lubridate)
library(plotly)
library(scales)
library(tidyverse)
```


## 1
(Max)

This is the College Scorecard Data Set. It has information on higher education institutions in the US, including: information about the institution, academics, admissions, costs, student body, financial aid, and completion. It also has data on earnings post-graduation. This is only the most recent data available (I belive it is 2017). There is another file that has a glossary for all of the terms, since they are abbreviated in this data set. I will work on uploading it. -Max

```r
library(readr)

Most_Recent_Cohorts_Scorecard_Elements <- 
  read_csv("C:/Users/maxda/Downloads/Most-Recent-Cohorts-Scorecard-Elements.csv")

View(Most_Recent_Cohorts_Scorecard_Elements)
```


## 2
(Edwin)

With the recent CA wildfires going on, I was interested in analyzing the history of wildfires in California. I found [this Buzzfeed article](https://www.buzzfeednews.com/article/peteraldhous/california-wildfires-people-climate) that did some analysis, but I think we would be able to expand it a bit more.

Buzzfeed provided the code to clean up and visualize the data the way they did [at this page](https://buzzfeednews.github.io/2018-07-wildfire-trends/) and they also provided their Github repo with their data [here](https://github.com/BuzzFeedNews/2018-07-wildfire-trends).

They retrieved their information from the [California Department of Forestry and Fire Protection (Cal Fire) Fire Perimeters Geodatabase](https://frap.fire.ca.gov/frap-projects/fire-perimeters/) and this will be a good resource since it explains all the variable names.


```r
ca_fires <- read_csv("https://raw.githubusercontent.com/BuzzFeedNews/2018-07-wildfire-trends/master/data/calfire_frap.csv")
ca_damage <- read_csv("https://raw.githubusercontent.com/BuzzFeedNews/2018-07-wildfire-trends/master/data/calfire_damage.csv")
```

Number of days each fire burned

```r
ca_fires %>% 
  mutate(total_days = cont_date - alarm_date) %>% 
  
ggplot(aes(x = total_days)) +
  geom_histogram() +
  xlim(0, 180) +
  ylim(0, 1000) 
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 7527 rows containing non-finite values (stat_bin).
```

```
## Warning: Removed 2 rows containing missing values (geom_bar).
```

![](ideas_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Causes of CA Fires - come back and put in the name for each number (see below - Edwin)


```r
cause_names <- tibble(
  cause = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
  cause_name = c("Lightning", "Equipment Use", "Smoking", "Campfire", "Debris", "Railroad", "Arson", "Playing with Fire", "Miscellaneous", "Vehicle", "Power Line", "Firefighter Training", "Non-Firefighter Training", "Unknown/Unidentified", "Structure", "Aircraft", "Volcanic", "Escaped Prescribed Burn", "Illegal Alien Campfire")
)
```


```r
# This uses info above to create new column in original data set to have name for each cause
ca_fires <- ca_fires %>%
  left_join(cause_names, by = "cause")
```



```r
ggplot(ca_fires, aes(x = cause)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Causes of CA Fires", x = "Causes of Fire", y = "Number of Fires")
```

```
## Warning: Removed 42 rows containing non-finite values (stat_count).
```

![](ideas_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
ca_fires %>%
  group_by(cause_name) %>%
  count() %>%
  ggplot(aes(x = cause_name, y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Causes of CA Fires", x = "Causes of Fire", y = "Number of Fires")
```

![](ideas_files/figure-html/unnamed-chunk-8-1.png)<!-- -->



```r
# Summarizing total acres a fire spanned and number of structures destroyed in year
ca_fires %>%
  group_by(year_) %>%
  summarize(total_acres = sum(gis_acres, na.rm = TRUE)) %>%
  left_join(ca_damage, by = c("year_" = "year")) %>%
  arrange(desc(year_))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["year_"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["total_acres"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["structures"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"2017","2":"1424768.15","3":"10823"},{"1":"2016","2":"545984.62","3":"1219"},{"1":"2015","2":"789205.99","3":"3097"},{"1":"2014","2":"570894.64","3":"434"},{"1":"2013","2":"569820.44","3":"495"},{"1":"2012","2":"847714.59","3":"205"},{"1":"2011","2":"202426.80","3":"132"},{"1":"2010","2":"101474.40","3":"64"},{"1":"2009","2":"435839.54","3":"123"},{"1":"2008","2":"1382462.24","3":"1122"},{"1":"2007","2":"1040224.30","3":"3238"},{"1":"2006","2":"744764.65","3":"431"},{"1":"2005","2":"255532.76","3":"102"},{"1":"2004","2":"274638.56","3":"1016"},{"1":"2003","2":"970479.30","3":"5394"},{"1":"2002","2":"963898.51","3":"327"},{"1":"2001","2":"246939.66","3":"389"},{"1":"2000","2":"249730.53","3":"130"},{"1":"1999","2":"801137.00","3":"1385"},{"1":"1998","2":"165354.68","3":"165"},{"1":"1997","2":"217847.45","3":"216"},{"1":"1996","2":"672216.81","3":"153"},{"1":"1995","2":"198354.82","3":"121"},{"1":"1994","2":"377553.18","3":"170"},{"1":"1993","2":"324664.27","3":"169"},{"1":"1992","2":"224215.05","3":"983"},{"1":"1991","2":"40345.40","3":"54"},{"1":"1990","2":"371289.50","3":"135"},{"1":"1989","2":"135334.00","3":"294"},{"1":"1988","2":"321194.42","3":"NA"},{"1":"1987","2":"862910.91","3":"NA"},{"1":"1986","2":"109078.95","3":"NA"},{"1":"1985","2":"568393.59","3":"NA"},{"1":"1984","2":"207241.61","3":"NA"},{"1":"1983","2":"85942.57","3":"NA"},{"1":"1982","2":"142577.74","3":"NA"},{"1":"1981","2":"313830.35","3":"NA"},{"1":"1980","2":"380262.59","3":"NA"},{"1":"1979","2":"379128.77","3":"NA"},{"1":"1978","2":"126263.88","3":"NA"},{"1":"1977","2":"465615.28","3":"NA"},{"1":"1976","2":"199035.20","3":"NA"},{"1":"1975","2":"223025.00","3":"NA"},{"1":"1974","2":"137296.70","3":"NA"},{"1":"1973","2":"257137.01","3":"NA"},{"1":"1972","2":"92264.00","3":"NA"},{"1":"1971","2":"60060.32","3":"NA"},{"1":"1970","2":"676019.52","3":"NA"},{"1":"1969","2":"97125.55","3":"NA"},{"1":"1968","2":"223609.87","3":"NA"},{"1":"1967","2":"237255.25","3":"NA"},{"1":"1966","2":"266466.64","3":"NA"},{"1":"1965","2":"82850.83","3":"NA"},{"1":"1964","2":"296782.54","3":"NA"},{"1":"1963","2":"32275.42","3":"NA"},{"1":"1962","2":"132534.61","3":"NA"},{"1":"1961","2":"363230.44","3":"NA"},{"1":"1960","2":"346191.03","3":"NA"},{"1":"1959","2":"299518.80","3":"NA"},{"1":"1958","2":"252308.88","3":"NA"},{"1":"1957","2":"203264.11","3":"NA"},{"1":"1956","2":"178798.54","3":"NA"},{"1":"1955","2":"386057.68","3":"NA"},{"1":"1954","2":"219797.45","3":"NA"},{"1":"1953","2":"335875.49","3":"NA"},{"1":"1952","2":"133855.51","3":"NA"},{"1":"1951","2":"307773.76","3":"NA"},{"1":"1950","2":"538277.14","3":"NA"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
ca_fires %>%
  group_by(year_) %>%
  summarize(total_acres = sum(gis_acres, na.rm = TRUE)) %>%
  left_join(ca_damage, by = c("year_" = "year")) %>%
  arrange(desc(year_)) %>%
  ggplot(aes(x = year_, y = total_acres)) +
  geom_line()
```

![](ideas_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
# Using buzzfeeds code to vategorize fires as 3 different causes
ca_fires <- ca_fires %>%
  mutate(cause2 = case_when(cause == 1 | cause == 17 ~ "Natural",
                            cause == 14 | is.na(cause) ~ "Unknown",
                            cause != 1 | cause != 14 | cause != 17 ~ "Human"))
```


```r
ca_fires %>%
  group_by(cause2) %>%
  count() %>%
  ggplot(aes(x = cause2, y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Causes of CA Fires", x = "Causes of Fire", y = "Number of Fires")
```

![](ideas_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
