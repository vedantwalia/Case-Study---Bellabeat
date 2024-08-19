# Bellabeat Case Study

## Background Information

**Bellabeat** is a high-tech company that manufactures health-focused
smart products. Collecting data on activity, sleep, stress, and
reproductive health has allowed Bellabeat to empower women with
knowledge about their own health and habits.

### Business Task

Sršen, the company’s cofounder, would like an analysis of Bellabeat’s
available consumer data to identify opportunities for growth. She has
asked the marketing analytics team to analyze smart device usage data in
order to gain insight into how people are already using their smart
devices. Then, using this information, she would like recommendations
for how these trends can inform Bellabeat marketing strategy. Therefore,
in this case study, I will answer the following questions:

1.  What are some trends in smart device usage?
2.  How can these trends help influence Bellabeat marketing strategy?

### About the Data

The data for this case study comes from [Fitbit Fitness Tracker
Data](https://www.kaggle.com/datasets/arashnic/fitbit), a public domain
dataset available on Kaggle. It contains personal fitness tracker
information from 30 FitBit users. These 30 Fitbit users consented to the
submission of all personal tracker data contained in this dataset.

I will import the data set from Kaggle into RStudio where I can clean,
filter, and analyze the data.

### Limitations

-   Sample size: 30 people is not a large enough sample to be
    representative of all FitBit users
-   Outdated: The dataset contains data from a one month period in 2016
    only. For a deeper and more accurate analysis of trends, we would
    need data from the current year, preferably collected for an entire
    year to look at if trends vary during different times of year.
-   Limited: The dataset does not contain any demographic information
    about the users, including gender, age, or location, which would be
    beneficial for marketing purposes to target specific customers

## Data Preparation

I can see that there are two folders of data, lets merge the files that
have the same name and

    # Define the paths to the two folders
    folder1 <- "/cloud/project/archive/Fitabase Data 3.12.16-4.11.16"
    folder2 <- "/cloud/project/archive/Fitabase Data 4.12.16-5.12.16"

    # List all files in both folders
    files1 <- list.files(folder1, full.names = TRUE)
    files2 <- list.files(folder2, full.names = TRUE)

    # Get the file names without the path
    names1 <- basename(files1)
    names2 <- basename(files2)

    # Find the common file names
    common_files <- intersect(names1, names2)

No I moved all the unique files into the work folder so that further
analysis can be done

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    # Loop over the common file names
    for (file in common_files) {
      # Construct the full paths for both files
      file1_path <- file.path(folder1, file)
      file2_path <- file.path(folder2, file)
      
      # Read the files
      df1 <- read.csv(file1_path)
      df2 <- read.csv(file2_path)
      
      # Merge the data frames row-wise
      merged_df <- bind_rows(df1, df2)
      
      # Save the merged file
      write.csv(merged_df, file.path("/cloud/project/archive", file), row.names = FALSE)
    }

    # Define the paths to the two folders
    folder1 <- "/cloud/project/archive/Fitabase Data 3.12.16-4.11.16"
    folder2 <- "/cloud/project/archive/Fitabase Data 4.12.16-5.12.16"
    new_folder <- "/cloud/project/archive"

    # List all files in both folders
    files1 <- list.files(folder1, full.names = TRUE)
    files2 <- list.files(folder2, full.names = TRUE)

    # Get the file names without the path
    names1 <- basename(files1)
    names2 <- basename(files2)

    # Find the unique file names in folder2
    unique_files <- setdiff(names2, names1)

    # Loop over the unique file names
    for (file in unique_files) {
      # Construct the full path for the file in folder2
      file2_path <- file.path(folder2, file)
      
      # Move the file to the new folder
      file.copy(file2_path, file.path(new_folder, file))
      
      # Optionally, remove the file from folder2
      file.remove(file2_path)
    }

Now lets start the analysis

## Data Preparation

1.  load packages

<!-- -->

    library(tidyverse)

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.5.1     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

    library(ggplot2)
    library(lubridate)
    library(lm.beta)

. Load CSV files containing our data

    # set working directory
    setwd("~/Downloads/8548186/archive")
    # load files
    daily_activity <- read.csv("dailyActivity_merged.csv")
    hourly_steps <- read.csv("hourlySteps_merged.csv")
    daily_sleep <- read.csv("sleepDay_merged.csv")
    weight <- read.csv("weightLogInfo_merged.csv")

1.  Identify number of participants in each data set by counting
    distinct IDs

<!-- -->

    n_distinct(daily_activity$Id)

    ## [1] 35

    n_distinct(hourly_steps$Id)

    ## [1] 35

    n_distinct(daily_sleep$Id)

    ## [1] 24

    n_distinct(weight$Id)

    ## [1] 13

1.  View and clean up the data sets

<!-- -->

    # First, the daily_sleep data
    head(daily_sleep)

    ##           Id              SleepDay TotalSleepRecords TotalMinutesAsleep
    ## 1 1503960366 4/12/2016 12:00:00 AM                 1                327
    ## 2 1503960366 4/13/2016 12:00:00 AM                 2                384
    ## 3 1503960366 4/15/2016 12:00:00 AM                 1                412
    ## 4 1503960366 4/16/2016 12:00:00 AM                 2                340
    ## 5 1503960366 4/17/2016 12:00:00 AM                 1                700
    ## 6 1503960366 4/19/2016 12:00:00 AM                 1                304
    ##   TotalTimeInBed
    ## 1            346
    ## 2            407
    ## 3            442
    ## 4            367
    ## 5            712
    ## 6            320

    # The 12:00:00 AM time stamp on each observation is redundant so we should remove it to make the data easier to work with
    daily_sleep$SleepDay <- (gsub('12:00:00 AM', '', daily_sleep$SleepDay))
    # Renaming column
    colnames(daily_sleep)[2] = "Date"
    # View updated data
    head(daily_sleep)

    ##           Id       Date TotalSleepRecords TotalMinutesAsleep TotalTimeInBed
    ## 1 1503960366 4/12/2016                  1                327            346
    ## 2 1503960366 4/13/2016                  2                384            407
    ## 3 1503960366 4/15/2016                  1                412            442
    ## 4 1503960366 4/16/2016                  2                340            367
    ## 5 1503960366 4/17/2016                  1                700            712
    ## 6 1503960366 4/19/2016                  1                304            320
