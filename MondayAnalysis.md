ST 558 Project 2
================
Hannah Park
10/15/2020

# Introduction

You should have an introduction section that briefly describes the data
and the variables you have to work with (no need to discuss all of them,
just the ones you want to use). **If you are analyzing the bike share
data, do not use the casual and registered variables to do any
modeling\!**

You should also mention the purpose of your analysis and the methods
you’ll use (no need to detail them here) for analysis.

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
                       holiday = "Holiday",
                       holiday = c("No" = 0,
                                   "Yes" = 1),
                       weathersit = "Weather",
                       weathersit = c("Good" = 1,
                                      "Moderate" = 2,
                                      "Bad" = 3,
                                      "Extreme" = 4),
                       dayofweek = "Day of Week")
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
cro_cases(list(holiday, weathersit), dayofweek,
          total_row_position = "none")
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">

<thead>

<tr>

<th style="border-top: 2px solid grey;">

</th>

<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

 Day of Week 

</th>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; font-weight: 900; text-align: center;">

</th>

<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">

 Monday 

</th>

</tr>

</thead>

<tbody>

<tr>

<td colspan="2" style="font-weight: 900;">

 Holiday 

</td>

</tr>

<tr>

<td style="text-align: left;">

   No 

</td>

<td style="text-align: right;">

62

</td>

</tr>

<tr>

<td style="text-align: left;">

   Yes 

</td>

<td style="text-align: right;">

11

</td>

</tr>

<tr>

<td colspan="2" style="font-weight: 900;">

 Weather 

</td>

</tr>

<tr>

<td style="text-align: left;">

   Good 

</td>

<td style="text-align: right;">

42

</td>

</tr>

<tr>

<td style="text-align: left;">

   Moderate 

</td>

<td style="text-align: right;">

29

</td>

</tr>

<tr>

<td style="text-align: left;">

   Bad 

</td>

<td style="text-align: right;">

2

</td>

</tr>

<tr>

<td style="border-bottom: 2px solid grey; text-align: left;">

   Extreme 

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

0.1

</td>

<td style="text-align: right;">

0.3

</td>

<td style="text-align: right;">

0.2

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

0.7

</td>

<td style="text-align: right;">

0.6

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.3

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

0.4

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

0.5

</td>

<td style="text-align: right;">

0.4

</td>

<td style="text-align: right;">

0.3

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

0.3

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

0.6

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

0.4

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.5

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

0.5

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

0.6

</td>

<td style="text-align: right;">

0.5

</td>

<td style="text-align: right;">

0.7

</td>

<td style="text-align: right;">

0.8

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

0.7

</td>

</tr>

<tr>

<td style="text-align: left;">

   Mean 

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

0.7

</td>

<td style="text-align: right;">

0.7

</td>

</tr>

<tr>

<td style="text-align: left;">

   Max 

</td>

<td style="text-align: right;">

0.9

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

0.9

</td>

<td style="text-align: right;">

0.9

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

0.1

</td>

<td style="text-align: right;">

0.2

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

0.0

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

0.1

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

0.3

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

0.4

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

``` r
df.train %>%
  gather(avgTemp, hum, windspeed, key = "var", value = "value") %>%
  mutate(var = factor(var, levels = c("hum", "avgTemp", "windspeed"))) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~var, scales = "free", nrow = 2) 
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](MondayAnalysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
df.train %>%
  gather(avgTemp, hum, windspeed, key = "var", value = "value") %>%
  mutate(var = factor(var, levels = c("hum", "avgTemp", "windspeed"))) %>%
  ggplot(aes(x = value, y = cnt, color = factor(yr), shape = factor(season))) +
  geom_point() +
  geom_smooth(aes(group = factor(yr))) +
  facet_wrap(~var, scales = "free", nrow = 2) 
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](MondayAnalysis_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->
