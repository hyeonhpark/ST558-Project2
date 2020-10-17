# ST558 Project 2

## Purpose of the Repo  
This repository was created to keep track of my work related to Project 2 for ST558 at NCSU (Fall 2020). This project involved creating predictive models and automating Markdown reports. Data used for the analysis is the two-year historical log corresponding to years 2011 and 2012 from Capital Bikeshare system, Washington D.C., USA which is publicly available in [http://capitalbikeshare.com/system-data].   

## Links to Analyses  
[Monday is available here](MondayAnalysis.md)  
[Tuesday is available here](TuesdayAnalysis.md)  
[Wednesday is available here](WednesdayAnalysis.md)  
[Thursday is available here](ThursdayAnalysis.md)  
[Friday is available here](FridayAnalysis.md)  
[Saturday is available here](SaturdayAnalysis.md)  
[Sunday is available here](SundayAnalysis.md)  

## Required Packages   
`knitr`
`tidyverse` - Data manipulation.  
`ggplot2` - Creating exploratory plots.  
`corrplot` - Visualizing correlation between variables.  
`expss` - Creating contingency tables.  
`gridExtra` - Displaying multiple plots in one panel.  
`caret` - Fitting tree models.  
`rattle` - Visualizing regression tree.  
`rmarkdown` - Render automation.  

## Code for Automation  
```{r}
dayofweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
output_file <- paste0(dayofweek, "Analysis", ".md")
params <- lapply(dayofweek, FUN = function(x){list(dayofweek = x)})
reports <- tibble(output_file, params)

apply(reports, MARGIN = 1, 
      FUN = function(x){render(input = "AnalysisCode.Rmd",
                               output_file = x[[1]],
                               params = x[[2]])})
```

