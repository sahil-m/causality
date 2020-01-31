library(utils)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("readr", "readxl", "zoo", "manipulate", "plotly", "plyr", "reshape2", "tidyverse", "ggthemes", "RColorBrewer", "lubridate", "janitor", "caret", "assertr", "grid", "ggforce", "rpart", "rpart.plot", "knitr")

ipak(packages)

options("scipen"=100, "digits"=4) #### Use this to not display no. in exponent format in R

##### function to count NAs in each column of a data.frame
count_na <- function(df) {
  return(sapply(df, function(x) sum(is.na(x))))
}