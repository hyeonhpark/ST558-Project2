ST 558 Project 2
================
Hannah Park
10/15/2020

# Introduction

Bike sharing systems is a service in which bicycles are made available
for shared use to individuals on a short term basis. Bike share systems
allow individuals to rent a bike from a particular position and return
it at another through an automated process. This automated process,
which explicitly records departure and arrival positions and other
related observations such as date and duration of the rental, generates
a vast amount of data that can be used for research in areas such as
traffic, environmental and health issues, mobility in the city, and etc.
In this document, data on count of total rental bikes aggregated on
daily basis is used to create models for predicting the count using
predictors related to the day of the rental and the weather.

The original data set is related to the two-year historical log
corresponding to years 2011 and 2012 from Capital Bikeshare system,
Washington D.C., USA which is publicly available in
\[<http://capitalbikeshare.com/system-data>\]. The original data set
consists of the following variables:

  - instant: record index  
  - dteday : date  
  - season : season (1:springer, 2:summer, 3:fall, 4:winter)  
  - yr : year (0: 2011, 1:2012)  
  - mnth : month ( 1 to 12)  
  - holiday : weather day is holiday or not (extracted from [Department
    of HR Holiday Schedule](http://dchr.dc.gov/page/holiday-schedule))  
  - weekday : day of the week (0 to 6: Sunday to Monday)  
  - workingday : if day is neither weekend nor holiday is 1, otherwise
    is 0.  
  - weathersit :
      - 1: Clear, Few clouds, Partly cloudy, Partly cloudy  
      - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds,
        Mist  
      - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds,
        Light Rain + Scattered clouds  
      - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog  
  - temp : Normalized temperature in Celsius. The values are divided to
    41 (max)  
  - atemp: Normalized feeling temperature in Celsius. The values are
    divided to 50 (max)  
  - hum: Normalized humidity. The values are divided to 100 (max)  
  - windspeed: Normalized wind speed. The values are divided to 67
    (max)  
  - casual: count of casual users  
  - registered: count of registered users  
  - cnt: count of total rental bikes including both casual and
    registered

A tree-based model chosen using leave one out cross validation(LOOCV)
and a boosted tree model chosen using cross-validation are used to model
the count of total rental bikes (cnt). Some variables from the original
data set were removed/transformed from the models to avoid
multicollinearity. Variables season and month (mnth), as well as
variables temp and atemp, were highly correlated with each other
according to the [correlation plot](correlation-plot-1.png). Variable
month (mnth) was chosen over variable season to minimize the loss of
information and also because month was more highly correlated with the
response variable (cnt) than season. Variables temp and atemp were
combined and their average was used in the models. Thus, the predictor
variables used in the models are year (yr), month (mnth), holiday,
weather (weathersit), average temperature (average of temp and atemp),
humidity (hum), and wind speed (windspeed).

# Data

``` r
# Read in data
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip"
download.file(url, "Bike-Sharing-Dataset.zip")

unzip("Bike-Sharing-Dataset.zip", exdir = "./Data")
df.bike <- read_csv("/Data/day.csv") %>%
  select(-instant, -casual, -registered) %>%
  mutate(dayofweek = dplyr::recode(weekday,
                            `0` = "Sunday",
                            `1` = "Monday",
                            `2` = "Tuesday",
                            `3` = "Wednesday",
                            `4` = "Thursday",
                            `5` = "Friday",
                            `6` = "Saturday")) %>%
  #Combine variables temp and atemp 
  mutate(avgTemp = (temp+atemp)/2)

df.bike.day <- df.bike %>%
  filter(weekday == 1)

#Randomply sample from the data 
#Form training and test sets
train <- sample(1:nrow(df.bike.day), size = nrow(df.bike.day)*0.7)
test <- dplyr::setdiff(1:nrow(df.bike.day), train)
df.train <- df.bike.day[train, ]
df.test <- df.bike.day[test, ]
```

# Summarizations

## Full Data

Original data is used for full data exploratory analysis.

#### Figure 1. Correlation plot

``` r
corrplot(cor(df.bike[,2:13]))
```

![Correlation
Plot](MondayAnalysis_files/figure-gfm/correlation-plot-1.png)

#### Table 1. Qualitative variables: Contingency tables

``` r
df.tbl <- apply_labels(df.bike,
                       dteday = "Date",
                       holiday = "Holiday",
                       holiday = c("No" = 0,
                                   "Yes" = 1),
                       weathersit = "Weather",
                       weathersit = c("Good" = 1,
                                      "Moderate" = 2,
                                      "Bad" = 3,
                                      "Extreme" = 4),
                       dayofweek = "Day of Week",
                       yr = "Year",
                       mnth = "Month",
                       avgTemp = "Average Temperature",
                       hum = "Humidity",
                       windspeed = "Wind Speed",
                       cnt = "Count of Total Rental Bikes")
attach(df.tbl)
cro_cases(list(holiday, weathersit), dayofweek,
          total_row_position = "none")
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">

<thead>

<tr>

<th style="border-top: 2px solid grey;">

</th>

<th colspan="7" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

 Day of Week 

</th>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; font-weight: 900; text-align: center;">

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 Friday 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 Monday 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 Saturday 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 Sunday 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 Thursday 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 Tuesday 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 Wednesday 

</th>

</tr>

</thead>

<tbody>

<tr>

<td colspan="8" style="font-weight: 900;">

 Holiday 

</td>

</tr>

<tr>

<td style="text-align: left;">

   No 

</td>

<td style="text-align: right;">

102

</td>

<td style="text-align: right;">

90

</td>

<td style="text-align: right;">

105

</td>

<td style="text-align: right;">

105

</td>

<td style="text-align: right;">

102

</td>

<td style="text-align: right;">

103

</td>

<td style="text-align: right;">

103

</td>

</tr>

<tr>

<td style="text-align: left;">

   Yes 

</td>

<td style="text-align: right;">

2

</td>

<td style="text-align: right;">

15

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

2

</td>

<td style="text-align: right;">

1

</td>

<td style="text-align: right;">

1

</td>

</tr>

<tr>

<td colspan="8" style="font-weight: 900;">

 Weather 

</td>

</tr>

<tr>

<td style="text-align: left;">

   Good 

</td>

<td style="text-align: right;">

63

</td>

<td style="text-align: right;">

66

</td>

<td style="text-align: right;">

67

</td>

<td style="text-align: right;">

74

</td>

<td style="text-align: right;">

67

</td>

<td style="text-align: right;">

62

</td>

<td style="text-align: right;">

64

</td>

</tr>

<tr>

<td style="text-align: left;">

   Moderate 

</td>

<td style="text-align: right;">

41

</td>

<td style="text-align: right;">

37

</td>

<td style="text-align: right;">

34

</td>

<td style="text-align: right;">

30

</td>

<td style="text-align: right;">

34

</td>

<td style="text-align: right;">

38

</td>

<td style="text-align: right;">

33

</td>

</tr>

<tr>

<td style="text-align: left;">

   Bad 

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

2

</td>

<td style="text-align: right;">

4

</td>

<td style="text-align: right;">

1

</td>

<td style="text-align: right;">

3

</td>

<td style="text-align: right;">

4

</td>

<td style="text-align: right;">

7

</td>

</tr>

<tr>

<td style="border-bottom: 2px solid grey; text-align: left;">

   Extreme 

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

</tr>

</tbody>

</table>

``` r
detach(df.tbl)
```

#### Figure 2. Histograms of quantitative variables

``` r
df.bike %>%
  gather(avgTemp, hum, windspeed, key = "var", value = "value") %>%
  mutate(var = factor(var, levels = c("hum", "avgTemp", "windspeed"))) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~var, scales = "free", nrow = 2) 
```

![](MondayAnalysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

#### Figure 3. Scatterplot of response variable(cnt) over days

``` r
ggplot(df.bike, aes(x = dteday, y = cnt)) +
  geom_point(aes(colour = factor(holiday)))
```

![](MondayAnalysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Specific Day of the Week Data

Training data is used.

#### Table 2. Qualitative variables: Contingency tables

``` r
df.tbl.train <- apply_labels(df.train,
                       holiday = "Holiday",
                       holiday = c("No" = 0,
                                   "Yes" = 1),
                       weathersit = "Weather",
                       weathersit = c("Good" = 1,
                                      "Moderate" = 2,
                                      "Bad" = 3,
                                      "Extreme" = 4),
                       dayofweek = "Day of Week")
attach(df.tbl.train)
cro_cases(weathersit, mnth,
          total_row_position = "none")
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">

<thead>

<tr>

<th style="border-top: 2px solid grey;">

</th>

<th colspan="12" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

 mnth 

</th>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; font-weight: 900; text-align: center;">

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 1 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 2 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 3 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 4 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 5 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 6 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 7 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 8 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 9 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 10 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 11 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 12 

</th>

</tr>

</thead>

<tbody>

<tr>

<td colspan="13" style="font-weight: 900;">

 Weather 

</td>

</tr>

<tr>

<td style="text-align: left;">

   Good 

</td>

<td style="text-align: right;">

2

</td>

<td style="text-align: right;">

5

</td>

<td style="text-align: right;">

6

</td>

<td style="text-align: right;">

4

</td>

<td style="text-align: right;">

3

</td>

<td style="text-align: right;">

3

</td>

<td style="text-align: right;">

6

</td>

<td style="text-align: right;">

6

</td>

<td style="text-align: right;">

3

</td>

<td style="text-align: right;">

1

</td>

<td style="text-align: right;">

3

</td>

<td style="text-align: right;">

2

</td>

</tr>

<tr>

<td style="text-align: left;">

   Moderate 

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

2

</td>

<td style="text-align: right;">

1

</td>

<td style="text-align: right;">

2

</td>

<td style="text-align: right;">

4

</td>

<td style="text-align: right;">

4

</td>

<td style="text-align: right;">

2

</td>

<td style="text-align: right;">

2

</td>

<td style="text-align: right;">

4

</td>

<td style="text-align: right;">

4

</td>

<td style="text-align: right;">

1

</td>

<td style="text-align: right;">

3

</td>

</tr>

<tr>

<td style="text-align: left;">

   Bad 

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

</td>

<td style="text-align: right;">

</td>

</tr>

<tr>

<td style="border-bottom: 2px solid grey; text-align: left;">

   Extreme 

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

</td>

</tr>

</tbody>

</table>

``` r
detach(df.tbl.train)
```

#### Table 3. Quantitative variables: Summary statistics

``` r
df.tbl.train %>%
  tab_cells(avgTemp, hum, windspeed) %>%
  tab_cols(mnth) %>%
  tab_stat_fun(Minimum = w_min, Median = w_median, 
               Mean = w_mean, Max = w_max) %>%
  tab_pivot()
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">

<thead>

<tr>

<th style="border-top: 2px solid grey;">

</th>

<th colspan="12" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

 mnth 

</th>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; font-weight: 900; text-align: center;">

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 1 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 2 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 3 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 4 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 5 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 6 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 7 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 8 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 9 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 10 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 11 

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 12 

</th>

</tr>

</thead>

<tbody>

<tr>

<td colspan="13" style="font-weight: 900;">

 avgTemp 

</td>

</tr>

<tr>

<td style="text-align: left;">

   Minimum 

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.4

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.4

</td>

<td style="text-align: right;">

0.3

</td>

<td style="text-align: right;">

0.2

</td>

</tr>

<tr>

<td style="text-align: left;">

   Median 

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.3

</td>

<td style="text-align: right;">

0.3

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.4

</td>

<td style="text-align: right;">

0.4

</td>

</tr>

<tr>

<td style="text-align: left;">

   Mean 

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.3

</td>

<td style="text-align: right;">

0.4

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.4

</td>

<td style="text-align: right;">

0.4

</td>

</tr>

<tr>

<td style="text-align: left;">

   Max 

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.4

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.5

</td>

</tr>

<tr>

<td colspan="13" style="font-weight: 900;">

 hum 

</td>

</tr>

<tr>

<td style="text-align: left;">

   Minimum 

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.4

</td>

<td style="text-align: right;">

0.3

</td>

<td style="text-align: right;">

0.3

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.4

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.6

</td>

</tr>

<tr>

<td style="text-align: left;">

   Median 

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.8

</td>

</tr>

<tr>

<td style="text-align: left;">

   Mean 

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.8

</td>

</tr>

<tr>

<td style="text-align: left;">

   Max 

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.9

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.8

</td>

<td style="text-align: right;">

0.8

</td>

<td style="text-align: right;">

0.8

</td>

<td style="text-align: right;">

0.8

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.8

</td>

<td style="text-align: right;">

0.8

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.9

</td>

</tr>

<tr>

<td colspan="13" style="font-weight: 900;">

 windspeed 

</td>

</tr>

<tr>

<td style="text-align: left;">

   Minimum 

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.0

</td>

<td style="text-align: right;">

0.1

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.1

</td>

<td style="text-align: right;">

0.1

</td>

<td style="text-align: right;">

0.1

</td>

<td style="text-align: right;">

0.1

</td>

<td style="text-align: right;">

0.1

</td>

<td style="text-align: right;">

0.1

</td>

<td style="text-align: right;">

0.0

</td>

<td style="text-align: right;">

0.1

</td>

</tr>

<tr>

<td style="text-align: left;">

   Median 

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.3

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.1

</td>

</tr>

<tr>

<td style="text-align: left;">

   Mean 

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.3

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.2

</td>

<td style="text-align: right;">

0.1

</td>

</tr>

<tr>

<td style="border-bottom: 2px solid grey; text-align: left;">

   Max 

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

0.2

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

0.4

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

0.3

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

0.4

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

0.2

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

0.3

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

0.3

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

0.3

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

0.3

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

0.3

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

0.3

</td>

<td style="border-bottom: 2px solid grey; text-align: right;">

0.2

</td>

</tr>

</tbody>

</table>

#### Figure 4. Qualitative variables: Boxplots

``` r
boxplot1 <- df.train %>%
  ggplot(aes(x = factor(mnth), y = cnt)) +
  geom_boxplot()

boxplot2 <- df.train %>%
  ggplot(aes(x = factor(weathersit), y = cnt)) +
  geom_boxplot() 

boxplot3 <- df.train %>%
  ggplot(aes(x = factor(holiday), y = cnt)) +
  geom_boxplot() 

boxplot4 <- df.train %>%
  ggplot(aes(x = factor(yr), y = cnt)) +
  geom_boxplot()

grid.arrange(boxplot4, boxplot1, boxplot3, boxplot2)
```

![](MondayAnalysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

#### Figure 5. Quantitative variables: Scatterplots

``` r
df.train %>%
  gather(avgTemp, hum, windspeed, key = "var", value = "value") %>%
  mutate(var = factor(var, levels = c("hum", "avgTemp", "windspeed"))) %>%
  ggplot(aes(x = value, y = cnt, color = factor(yr))) +
  geom_point() +
  geom_smooth(aes(group = factor(yr))) +
  facet_wrap(~var, scales = "free", nrow = 2) 
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](MondayAnalysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

# Modeling

## Tree-based Model

``` r
treeFit <- train(cnt ~ yr + mnth + holiday + weathersit + avgTemp + hum + windspeed, data = df.train,
                  method = "rpart",
                  trControl = trainControl(method = "LOOCV")
                  )
treeFit$results
```

    ##           cp     RMSE   Rsquared       MAE
    ## 1 0.08315861 1249.265 0.43871382  871.9373
    ## 2 0.22085594 1480.598 0.23167038 1264.2147
    ## 3 0.41611962 1836.741 0.02313598 1644.0745

``` r
treePred <- predict(treeFit, newdata = df.test)
treeRMSE <- sqrt(mean((treePred-df.test$cnt)^2))
```

## Boosted Tree Model

``` r
boostFit <- train(cnt ~ yr + mnth + holiday + weathersit + avgTemp + hum + windspeed, data = df.train,
                  method = "gbm",
                  trControl = trainControl(method = "cv", number = 10),
                  preProcess = c("center", "scale"),
                  verbose = FALSE)
boostFit$results
```

    ##   shrinkage interaction.depth n.minobsinnode n.trees     RMSE  Rsquared
    ## 1       0.1                 1             10      50 761.2691 0.8151414
    ## 4       0.1                 2             10      50 761.1121 0.7996192
    ## 7       0.1                 3             10      50 759.1499 0.8009363
    ## 2       0.1                 1             10     100 738.6397 0.8171317
    ## 5       0.1                 2             10     100 754.8024 0.8086861
    ## 8       0.1                 3             10     100 772.8222 0.7991420
    ## 3       0.1                 1             10     150 744.0603 0.8168485
    ## 6       0.1                 2             10     150 750.0005 0.8108396
    ## 9       0.1                 3             10     150 763.2784 0.8018604
    ##        MAE   RMSESD RsquaredSD    MAESD
    ## 1 571.3036 297.5567  0.1107194 158.7968
    ## 4 563.0749 338.4192  0.1338603 167.3590
    ## 7 556.2662 347.0529  0.1392153 187.9989
    ## 2 542.3820 298.6180  0.1176837 146.3355
    ## 5 547.9519 306.3109  0.1173246 120.2693
    ## 8 571.4847 311.9770  0.1208261 148.7789
    ## 3 546.0298 288.4683  0.1130381 144.0938
    ## 6 547.4595 302.4847  0.1119084 134.2266
    ## 9 564.9278 323.0779  0.1339322 168.0201

``` r
boostPred <- predict(boostFit, newdata = df.test)
boostRMSE <- sqrt(mean((boostPred-df.test$cnt)^2))
```

# Model Comparison

``` r
tbl.rmse <- rbind.data.frame("tree" = treeRMSE, "boost" = boostRMSE)
colnames(tbl.rmse) <- "RMSE"
rownames(tbl.rmse) <- c("Reg. Tree", "Boost Tree")
kable(tbl.rmse, caption = "Comparison of Models' RMSE")
```

|            |     RMSE |
| ---------- | -------: |
| Reg. Tree  | 1521.097 |
| Boost Tree | 1251.520 |

Comparison of Models’ RMSE
