---
title: "README"
author: "Hannah Park"
date: "10/16/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(rmarkdown)
```

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

