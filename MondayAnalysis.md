ST 558 Project 2
================
Hannah Park
10/15/2020

# Data

``` r
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip"
download.file(url, "data.zip")
unzip("data.zip")
df.bike <- read_csv("day.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   instant = col_double(),
    ##   dteday = col_date(format = ""),
    ##   season = col_double(),
    ##   yr = col_double(),
    ##   mnth = col_double(),
    ##   holiday = col_double(),
    ##   weekday = col_double(),
    ##   workingday = col_double(),
    ##   weathersit = col_double(),
    ##   temp = col_double(),
    ##   atemp = col_double(),
    ##   hum = col_double(),
    ##   windspeed = col_double(),
    ##   casual = col_double(),
    ##   registered = col_double(),
    ##   cnt = col_double()
    ## )

``` r
head(df.bike)
```

    ## # A tibble: 6 x 16
    ##   instant dteday     season    yr  mnth holiday weekday workingday weathersit
    ##     <dbl> <date>      <dbl> <dbl> <dbl>   <dbl>   <dbl>      <dbl>      <dbl>
    ## 1       1 2011-01-01      1     0     1       0       6          0          2
    ## 2       2 2011-01-02      1     0     1       0       0          0          2
    ## 3       3 2011-01-03      1     0     1       0       1          1          1
    ## 4       4 2011-01-04      1     0     1       0       2          1          1
    ## 5       5 2011-01-05      1     0     1       0       3          1          1
    ## 6       6 2011-01-06      1     0     1       0       4          1          1
    ## # ... with 7 more variables: temp <dbl>, atemp <dbl>, hum <dbl>,
    ## #   windspeed <dbl>, casual <dbl>, registered <dbl>, cnt <dbl>
