> **Tables of Contents**

-   Introduction

-   Ask

    -   Guiding Questions
    -   Key tasks
    -   Deliverable

-   Prepare

    -   Guiding questions
    -   Key Tasks
    -   Deliverable

-   Process

    -   Code
        -   Dependence
        -   Concatenating
        -   Data Cleaning
        -   Manipulating the data
        -   Saving the result as a csv
    -   Guiding questions
    -   Key tasks
    -   Deliverable

-   Analyze

    -   Code
        -   Data Distribution
        -   Other variables
    -   Guiding questions
    -   Key Tasks
    -   Deliverable

-   Share

    -   Guiding questions
    -   Key Tasks
    -   Deliverable

-   Act

    -   Guiding questions
    -   Key Tasks
    -   Deliverable

-   Conclusion

## Introduction

This is my version of the Google Data Analytics Capstone Case Study 1.
The full documentation to the case study can be found in the [Google
Data Analytics Capstone: Complete a Case
Study](https://www.coursera.org/learn/google-data-analytics-capstone)
course.

For this project, the following steps will be followed to ensure its
completion: –

-   It will follow the steps of the data analysis process: Ask, Prepare,
    Process, Analyze, Share and Act
-   Each step will follow its own roadmap with: –
    -   code, if needed on the step.
    -   Guiding question, with answers.
    -   Key tasks, as a checklist.
    -   Deliverable, as a checklist.

## Ask

For the ask step, first, let’s get some context from the cyclists
document:

### Guiding questions

-   **What is the problem you are trying to solve?**

The main objective is to determine a way to build a profile for annual
members and the best marketing strategies to turn casual bike riders
into annual members.

-   **How can your insight drive business decisions?**

The insight will help the marketing team to increase annual member

### Key tasks

-   ☒ Identify the business task
-   ☒ Consider Key stakeholders

### Deliverable

-   ☒ A Clear Statement of the business task Find the keys differences
    between casual and members riders and how digital media could
    influence them

## Prepare

The project will use the data provided by this [Kaggle
dataset](https://www.kaggle.com/timgid/cyclistic-dataset-google-certificate-capstone)

### Guiding questions

-   **Where is your data located?**

The data is located in kaggle dataset.

-   **How is the data organized?**

The data is separated by month, each on its own *CSV*.

-   **How are you addressing licensing, privacy, security and
    accessibility?**

The company has their own license over the dataset. Besides that, the
dataset doesn’t have any personal information about the riders.

-   **How did you verify the data’s integrity?**

All the files have consistent columns and each column has the correct
type of data.

-   **How does it help you answer your question?**

It may have some key insight about riders and their riding style.

-   **are there any problems with the data?**

It would be good to have some updated information about the bike
stations. Also more information about the riders could be useful.

### Key tasks

-   ☒ Download data and store it appropriately.
-   ☒ Identify how its organized.
-   ☒ Determine the credibility of the data.

### Deliverable

-   ☒ A description of all data source used The main data source is 12
    months (Between April 2020 and march 2021) of riding data provided
    by the Cicylistic Company.

## Process

This step will prepare the data for analysis. All the csv files will be
merged into one file to improve workflow.

### Code

    library(tidyverse)

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.2     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

**Importing Data**

###### import data

    #loading data-sets 
    q1_2020 <- read_csv("D:\\Dataset_ash\\case study\\bike sharing divvy\\Divvy_Trips_2020_Q1.csv")

    ## Rows: 426887 Columns: 13

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (5): ride_id, rideable_type, start_station_name, end_station_name, memb...
    ## dbl  (6): start_station_id, end_station_id, start_lat, start_lng, end_lat, e...
    ## dttm (2): started_at, ended_at

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    q2_2019 <- read_csv("D:\\Dataset_ash\\case study\\bike sharing divvy\\Divvy_Trips_2019_Q2.csv")

    ## Rows: 1108163 Columns: 12

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (4): 03 - Rental Start Station Name, 02 - Rental End Station Name, User...
    ## dbl  (5): 01 - Rental Details Rental ID, 01 - Rental Details Bike ID, 03 - R...
    ## dttm (2): 01 - Rental Details Local Start Time, 01 - Rental Details Local En...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    q3_2019 <- read_csv("D:\\Dataset_ash\\case study\\bike sharing divvy\\Divvy_Trips_2019_Q3.csv")

    ## Rows: 1640718 Columns: 12

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (4): from_station_name, to_station_name, usertype, gender
    ## dbl  (5): trip_id, bikeid, from_station_id, to_station_id, birthyear
    ## dttm (2): start_time, end_time

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    q4_2019 <- read_csv("D:\\Dataset_ash\\case study\\bike sharing divvy\\Divvy_Trips_2019_Q4.csv")

    ## Rows: 704054 Columns: 12

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (4): from_station_name, to_station_name, usertype, gender
    ## dbl  (5): trip_id, bikeid, from_station_id, to_station_id, birthyear
    ## dttm (2): start_time, end_time

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

###### Wrangle data

    #Rename columns to make them consistent with q1_2020.
    q2_2019 <- q2_2019 %>% rename(ride_id = "01 - Rental Details Rental ID",
                                  rideable_type = "01 - Rental Details Bike ID",
                                  started_at = "01 - Rental Details Local Start Time",
                                  ended_at = "01 - Rental Details Local End Time"
                                  ,start_station_name = "03 - Rental Start Station Name" 
                                  ,start_station_id = "03 - Rental Start Station ID"
                                  ,end_station_name = "02 - Rental End Station Name" 
                                  ,end_station_id = "02 - Rental End Station ID"
                                  ,member_casual = "User Type")

    q3_2019 <- q3_2019 %>% rename(ride_id = "trip_id",
                                  rideable_type = "bikeid",
                                  started_at = "start_time",
                                  ended_at = "end_time"
                                  ,start_station_name = "from_station_name" 
                                  ,start_station_id = "from_station_id"
                                  ,end_station_name = "to_station_name" 
                                  ,end_station_id = "to_station_id"
                                  ,member_casual = "usertype")

    q4_2019 <- q4_2019 %>% rename(ride_id = "trip_id",
                                  rideable_type = "bikeid",
                                  started_at = "start_time",
                                  ended_at = "end_time"
                                  ,start_station_name = "from_station_name" 
                                  ,start_station_id = "from_station_id"
                                  ,end_station_name = "to_station_name" 
                                  ,end_station_id = "to_station_id"
                                  ,member_casual = "usertype")

    #converting ride_id and rideable_type to character so they can stack easily:
    q2_2019 <- mutate(q2_2019,ride_id = as.character(ride_id),
                      rideable_type = as.character(rideable_type))
    q3_2019 <- mutate(q3_2019,ride_id = as.character(ride_id),
                      rideable_type = as.character(rideable_type))
    q4_2019 <- mutate(q4_2019,ride_id = as.character(ride_id),
                      rideable_type = as.character(rideable_type))

#### Concatenating.

All the CSV files will be concatenated into one dataframe.

    #stacking individual data frames into one.
    all_trips_df <- bind_rows(q1_2020,q2_2019,q3_2019,q4_2019)
    head(all_trips_df)

    ## # A tibble: 6 x 19
    ##   ride_id rideable_type started_at          ended_at            start_station_n~
    ##   <chr>   <chr>         <dttm>              <dttm>              <chr>           
    ## 1 EACB19~ docked_bike   2020-01-21 20:06:59 2020-01-21 20:14:30 Western Ave & L~
    ## 2 8FED87~ docked_bike   2020-01-30 14:22:39 2020-01-30 14:26:22 Clark St & Mont~
    ## 3 789F3C~ docked_bike   2020-01-09 19:29:26 2020-01-09 19:32:17 Broadway & Belm~
    ## 4 C9A388~ docked_bike   2020-01-06 16:17:07 2020-01-06 16:25:56 Clark St & Rand~
    ## 5 943BC3~ docked_bike   2020-01-30 08:37:16 2020-01-30 08:42:48 Clinton St & La~
    ## 6 6D9C8A~ docked_bike   2020-01-10 12:33:05 2020-01-10 12:37:54 Wells St & Hubb~
    ## # ... with 14 more variables: start_station_id <dbl>, end_station_name <chr>,
    ## #   end_station_id <dbl>, start_lat <dbl>, start_lng <dbl>, end_lat <dbl>,
    ## #   end_lng <dbl>, member_casual <chr>,
    ## #   01 - Rental Details Duration In Seconds Uncapped <dbl>,
    ## #   Member Gender <chr>, 05 - Member Details Member Birthday Year <dbl>,
    ## #   tripduration <dbl>, gender <chr>, birthyear <dbl>

    #Remove extra columns.
    all_trips_df <- all_trips_df %>% select(-c(start_lat,start_lng,end_lat,
                                               end_lng,birthyear,gender,"01 - Rental Details Duration In Seconds Uncapped",
                                               "05 - Member Details Member Birthday Year","Member Gender","tripduration"))

#### Data Cleaning.

##### Removing Duplicates

    all_trips_df_no_dups = all_trips_df[!duplicated(all_trips_df$ride_id),]
    print(paste("Removed", nrow(all_trips_df)-nrow(all_trips_df_no_dups),"duplicate rows"))

    ## [1] "Removed 0 duplicate rows"

    table(all_trips_df$member_casual) #seeing how many observation fall under each userType

    ## 
    ##     casual   Customer     member Subscriber 
    ##      48480     857474     378407    2595461

    #Reassign to the desired values.
    all_trips_df <- all_trips_df %>% mutate(member_casual = recode(member_casual, "Subscriber" = "member","Customer" = "casual"))

    table(all_trips_df$member_casual) #check to make sure the proper number of observation.

    ## 
    ##  casual  member 
    ##  905954 2973868

##### Parsing datetime columns

    # Add columns that list the date, month, day, and year of each ride
    all_trips_df$date <- as.Date(all_trips_df$started_at)
    all_trips_df$month  <- format(as.Date(all_trips_df$date), "%m")
    all_trips_df$day <- format(as.Date(all_trips_df$date),"%d")
    all_trips_df$year <- format(as.Date(all_trips_df$date),"%Y")
    all_trips_df$day_of_week <- format(as.Date(all_trips_df$date),"%A")
    all_trips_df <- all_trips_df %>%
        mutate(year_month = paste(strftime(all_trips_df$started_at, "%Y"),
                                  "-",
                                  strftime(all_trips_df$started_at, "%m"),
                                  paste("(",strftime(all_trips_df$started_at, "%b"), ")", sep="")))
    all_trips_df <- all_trips_df %>%
        mutate(start_hour = strftime(all_trips_df$ended_at, "%H"))

#### Manipulating the data

##### ride length

the total time of a bike ride, in minutes

    all_trips_df$ride_length <- (difftime(all_trips_df$ended_at,all_trips_df$started_at)/60)

    #convert ride_length from factor to numeric for ease in calculation
    all_trips_df$ride_length <- as.numeric(as.character(all_trips_df$ride_length))
    summary(all_trips_df$ride_length)

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##    -56.37      6.85     11.85     24.63     21.47 156450.40

    #remove bad data
    all_trips_df_v2 <- all_trips_df[!(all_trips_df$start_station_name == "HQ QR"|all_trips_df$ride_length<0),]

### Guiding questions

-   **What tools are you choosing and why?**

I’m using R for this project, for two main reasons: Because of the large
dataset and to gather experience with the language.

-   **Have you ensured your data’s integrity?**

Yes, the data is consistent throughout the columns.

-   **What steps have you taken to ensure that your data is clean?**

First the duplicates values where removed, then the columns where
formatted to their correct format.

-   **How can you verify that your data is clean and ready to analyze?**

It can be verified by this notebook.

-   **Have you documented your cleaning process so you can review and
    share those result?**

Yes, It’s all documented in this R notebook.

### Key tasks

-   ☒ Check the data for errors.
-   ☒ Choose your tools.
-   ☒ Transform the data so you can work with it effectively.
-   ☒ Document the cleaning process.

### Deliverable

-   ☒ Documentation of any cleaning or manipulation of data.

## Analyze

The data exploration will consist of building a profile for annual
members and how they differ from casual riders.

Putting in a new variable with a simpler name will help reduce some
typing in the future.

#### Code

To quick start, let’s generate a summary of the dataset.

    summary(all_trips_df_v2)

    ##    ride_id          rideable_type        started_at                 
    ##  Length:3876042     Length:3876042     Min.   :2019-04-01 00:02:22  
    ##  Class :character   Class :character   1st Qu.:2019-06-22 23:44:33  
    ##  Mode  :character   Mode  :character   Median :2019-08-14 16:56:35  
    ##                                        Mean   :2019-08-25 20:15:33  
    ##                                        3rd Qu.:2019-10-11 23:23:20  
    ##                                        Max.   :2020-03-31 23:51:34  
    ##     ended_at                   start_station_name start_station_id
    ##  Min.   :2019-04-01 00:09:48   Length:3876042     Min.   :  1.0   
    ##  1st Qu.:2019-06-23 00:16:46   Class :character   1st Qu.: 77.0   
    ##  Median :2019-08-14 17:15:04   Mode  :character   Median :174.0   
    ##  Mean   :2019-08-25 20:40:12                      Mean   :202.4   
    ##  3rd Qu.:2019-10-12 00:26:13                      3rd Qu.:290.0   
    ##  Max.   :2020-05-19 20:10:34                      Max.   :673.0   
    ##  end_station_name   end_station_id  member_casual           date           
    ##  Length:3876042     Min.   :  1.0   Length:3876042     Min.   :2019-04-01  
    ##  Class :character   1st Qu.: 77.0   Class :character   1st Qu.:2019-06-22  
    ##  Mode  :character   Median :174.0   Mode  :character   Median :2019-08-14  
    ##                     Mean   :203.3                      Mean   :2019-08-25  
    ##                     3rd Qu.:291.0                      3rd Qu.:2019-10-11  
    ##                     Max.   :675.0                      Max.   :2020-03-31  
    ##     month               day                year           day_of_week       
    ##  Length:3876042     Length:3876042     Length:3876042     Length:3876042    
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##   year_month         start_hour         ride_length       
    ##  Length:3876042     Length:3876042     Min.   :     0.02  
    ##  Class :character   Class :character   1st Qu.:     6.87  
    ##  Mode  :character   Mode  :character   Median :    11.87  
    ##                                        Mean   :    24.65  
    ##                                        3rd Qu.:    21.48  
    ##                                        Max.   :156450.40

##### Casuals vs members

How much of the data is about members and how much is about casuals?

    all_trips_df_v2 %>% group_by(member_casual) %>% summarise(count = length(ride_id),'%' = (length(ride_id)/ nrow(all_trips_df_v2))*100)

    ## # A tibble: 2 x 3
    ##   member_casual   count   `%`
    ##   <chr>           <int> <dbl>
    ## 1 casual         902182  23.3
    ## 2 member        2973860  76.7

    # Compare members and casual users
    aggregate(all_trips_df_v2$ride_length ~ all_trips_df_v2$member_casual, FUN = mean)

    ##   all_trips_df_v2$member_casual all_trips_df_v2$ride_length
    ## 1                        casual                    59.21250
    ## 2                        member                    14.16777

    aggregate(all_trips_df_v2$ride_length ~ all_trips_df_v2$member_casual, FUN = median)

    ##   all_trips_df_v2$member_casual all_trips_df_v2$ride_length
    ## 1                        casual                   25.766667
    ## 2                        member                    9.816667

    aggregate(all_trips_df_v2$ride_length ~ all_trips_df_v2$member_casual, FUN = max)

    ##   all_trips_df_v2$member_casual all_trips_df_v2$ride_length
    ## 1                        casual                    156450.4
    ## 2                        member                    150943.9

    aggregate(all_trips_df_v2$ride_length ~ all_trips_df_v2$member_casual, FUN = min)

    ##   all_trips_df_v2$member_casual all_trips_df_v2$ride_length
    ## 1                        casual                  0.03333333
    ## 2                        member                  0.01666667

    fig <- function(width,height){options(repr.plot.width = width,repr.plot.height = height)}

    fig(16,8)
    ggplot(data = all_trips_df_v2) + geom_bar(mapping  = aes(member_casual, fill = member_casual)) + labs(x = "Casual x Members", title = "Chart 01 - Casuals x Members distribution")

![](Case_study_1_files/figure-markdown_strict/unnamed-chunk-15-1.png)

As we can see on the member x casual table, members have a bigger
proportion of the dataset, composing ~76%, ~53% bigger than the count of
casual riders.

##### Month

How much of the data is distributed by month?

    all_trips_df_v2 %>%  group_by(year_month) %>% summarise(count = length(ride_id),'%' = (length(ride_id) / nrow(all_trips_df_v2)) * 100,
    'members_percent' = (sum(member_casual == "member") / length(ride_id)) * 100,"Casual_percent" = (sum(member_casual == "casual") / length(ride_id)) * 100, 'Member x Casual perc Differ' = members_percent - Casual_percent)

    ## # A tibble: 13 x 6
    ##    year_month    count     `%` members_percent Casual_percent `Member x Casual ~
    ##    <chr>         <int>   <dbl>           <dbl>          <dbl>              <dbl>
    ##  1 2019 - 04 (~ 264493 6.82e+0            82.0          18.0                63.9
    ##  2 2019 - 05 (~ 364223 9.40e+0            78.0          22.0                56.0
    ##  3 2019 - 06 (~ 476993 1.23e+1            72.6          27.4                45.2
    ##  4 2019 - 07 (~ 555023 1.43e+1            68.4          31.6                36.8
    ##  5 2019 - 08 (~ 592576 1.53e+1            68.4          31.6                36.9
    ##  6 2019 - 09 (~ 492350 1.27e+1            73.6          26.4                47.3
    ##  7 2019 - 10 (~ 374370 9.66e+0            80.9          19.1                61.7
    ##  8 2019 - 11 (~ 177633 4.58e+0            89.5          10.5                78.9
    ##  9 2019 - 12 (~ 154970 4.00e+0            89.4          10.6                78.8
    ## 10 2020 - 01 (~ 143406 3.70e+0            94.6           5.42               89.2
    ## 11 2020 - 02 (~ 139327 3.59e+0            91.2           8.84               82.3
    ## 12 2020 - 03 (~ 140383 3.62e+0            82.5          17.5                64.9
    ## 13 2020 - 04 (~    295 7.61e-3            85.8          14.2                71.5

    all_trips_df_v2 %>% ggplot() + geom_bar(mapping = aes(x=year_month,fill= member_casual)) + labs(x="Month",title = "Chart 02 - Distribution by month") + coord_flip()

![](Case_study_1_files/figure-markdown_strict/unnamed-chunk-17-1.png)

Some key takeaways can be taken by this chart: – \* There’s more riders
in summer season as compared to winter season. \* The month with the
biggest count of data points was August. \* In all month we hae more
members riders than casual riders.

##### Weekday

How much of the data is distributed by weekday?

    all_trips_df_v2 %>% group_by(day_of_week) %>% summarise(count = length(ride_id),'%' = (length(ride_id)/nrow(all_trips_df_v2))*100,'members_percent' = (sum(member_casual == "member") / length(ride_id))*100,
        'casual_percent' = (sum(member_casual == "casual")/length(ride_id))*100,'Member X Casual Perc Differ' = members_percent-casual_percent)

    ## # A tibble: 7 x 6
    ##   day_of_week  count   `%` members_percent casual_percent `Member X Casual Perc~
    ##   <chr>        <int> <dbl>           <dbl>          <dbl>                  <dbl>
    ## 1 Friday      575194  14.8            78.7           21.3                   57.4
    ## 2 Monday      575492  14.8            82.1           17.9                   64.1
    ## 3 Saturday    497501  12.8            57.9           42.1                   15.8
    ## 4 Sunday      449258  11.6            59.6           40.4                   19.3
    ## 5 Thursday    586856  15.1            82.5           17.5                   65.0
    ## 6 Tuesday     598955  15.5            84.9           15.1                   69.8
    ## 7 Wednesday   592786  15.3            84.4           15.6                   68.8

    ggplot(all_trips_df_v2)+geom_bar(mapping = aes(x=day_of_week,fill = member_casual)) + labs(x="Weekday",title = "Chart 03 - Distribution by weekdasy") + coord_flip()

![](Case_study_1_files/figure-markdown_strict/unnamed-chunk-19-1.png)

Some key takeaways can be taken by this chart: – \* The biggest volume
of data is on weekdays. \* weekends has the smallest data points. \*
weekends has the biggest volume of casual, starting on Friday.

##### ride time

    ventiles = quantile(all_trips_df_v2$ride_length, seq(0,1,by = 0.05))
    ventiles

    ##           0%           5%          10%          15%          20%          25% 
    ## 1.666667e-02 3.366667e+00 4.350000e+00 5.200000e+00 6.016667e+00 6.866667e+00 
    ##          30%          35%          40%          45%          50%          55% 
    ## 7.733333e+00 8.633333e+00 9.616667e+00 1.068333e+01 1.186667e+01 1.320000e+01 
    ##          60%          65%          70%          75%          80%          85% 
    ## 1.476667e+01 1.660000e+01 1.880000e+01 2.148333e+01 2.478333e+01 2.893333e+01 
    ##          90%          95%         100% 
    ## 3.563333e+01 5.376667e+01 1.564504e+05

    all_trips_outliers <- all_trips_df_v2 %>% filter(ride_length > as.numeric(ventiles['5%'])) %>% filter(ride_length < as.numeric(ventiles["95%"]))

    print(paste("Removed",nrow(all_trips_df_v2) - nrow(all_trips_outliers),"row as outliers" ))

    ## [1] "Removed 389004 row as outliers"

    all_trips_outliers %>% group_by(member_casual) %>% 
      summarise(mean = mean(ride_length), 'first_quarter'
                = as.numeric(quantile(ride_length,.25)),
                'median' = median(ride_length), 'third_quarter'
                = as.numeric(quantile(ride_length,0.75)),
                'IR' = third_quarter - first_quarter)

    ## # A tibble: 2 x 6
    ##   member_casual  mean first_quarter median third_quarter    IR
    ##   <chr>         <dbl>         <dbl>  <dbl>         <dbl> <dbl>
    ## 1 casual         23.3         13.5    21.6          30.8 17.3 
    ## 2 member         12.9          6.67   10.4          16.6  9.98

    ggplot(all_trips_outliers, aes(x = member_casual, y = ride_length, fill = member_casual)) + labs(x= "member x Casual", y = "Riding time", title = "chart 04 - Distribution of riding time for Casual x member") + geom_boxplot()

![](Case_study_1_files/figure-markdown_strict/unnamed-chunk-23-1.png)

It’s Important to note that: \* Casual have more riding time than
members. \* Mean and IQR is also bigger for casual.

    ggplot(all_trips_outliers) + geom_boxplot(mapping = aes(x=day_of_week, y=ride_length,fill = member_casual)) +
      facet_wrap(~ member_casual) + labs(x = "weekday",y = "Riding time",title = "chart 05 - Distribution of Riding for day of the week") + coord_flip()

![](Case_study_1_files/figure-markdown_strict/unnamed-chunk-24-1.png)

### Guiding queestions

-   **How should you organize your data to perform analysis on it?**

All dataset has been organized into a single csv.

-   **Has your data been properly formatted?**

Yes, all the columns have their correct data type.

-   **what surprises did you discover in the data?**

One of the main surprises is how members differ from casuals when
analysed from weekdays. Also the members have less riding time than
casual.

-   **what trends or relationships did you find in the data?**

-   There are more members than casuals in the dataset.

-   There are more of a difference between the flow of members/casual
    fro midweek to weekends.

-   Members use bikes on schedules that differs from casual.

-   Members have less riding time.

-   **How will these insight help answer your business questions?**

This insight helps to build a profile for members.

### Key Tasks

-   ☒ Aggregate your data so it’s useful and accessible.
-   ☒ Organize and format your data.
-   ☒ Perform calculations.
-   \[X Identify trends and relationships.

### Deliverable

-   ☒ A summary of your analysis.

## Share

The share phase is usually done by building a presentation.

Let’s go through the main finds and try to arrive at a conclusion.

What we know about the dataset:

-   Members have the biggest proportion of the dataset, ~53% bigger than
    casual.
-   The month with the biggest count of data points was August.
-   In all months we have more member’s rides than casual rides.
-   Temperature heavily influences the volume of rides in the month. Now
    how members differs from casuals:
-   Members have the biggest volume of data.
-   Casuals have more riding time than members.

### Guiding questions

-   **Were you able to answer the question of how annual members and
    casual riders use Cyclist bikes differently?**

Yes. The data points to several differences between casuals and members.

-   **How do your findings relate to your original question?**

The findings build a profile for members, relating to “Find the keys
differences between casuals and annual riders”, also knowing whey they
use the bikes helps to find “How digital media could influence them”.

-   **Who is your audience? What is the best way to communicate with
    them?**

The main target audience is my cyclistic marketing analytics team and
Lily Moreno. The best way to communicate is through a slide presentation
of the findings.

-   **Can data visualization help you share your findings?**

Yes, the main core of the finds is through data visualization.

-   **Is your presentation accessible to your audience?**

Yes, the plots were made using vibrant colors, and corresponding labels.

### Key tasks

-   ☒ Determine the best way to share your findings.
-   ☒ Create effective data visualizations.
-   ☒ Present your findings.
-   ☒ Ensure your work is accessible.

### Deliverable

-   ☒ Supporting visualizations and key findings.

## Act

The act phase would be done by the marketing team of the company. The
main takeaway will be the top three recommendations for the marketing.

### Guiding questions

-   **What is your final conclusion based on your analysis?**

Members and casual have different habits when using the bikes.

-   **How could your team and business apply your insights?**

The insight could be implemented when preparing a marketing campaign for
turning casual into members.

-   **What next steps would you or your stakeholders take based on your
    findings?**

Further analysis could be done to improve the findings, besides that,
the marketing team can take the main information to build a marketing
campaign.

-   **Is there additional data you could use to expand on your
    findings?**

-   Mobility data.

-   Improved climate data.

-   More information members.

### Deliverable

-   My top three recommendations based on analysis:

-   1.  Build a marketing campaign focusing on show how bikes help
        people to get to work, while maintaining the planet green and
        avoid traffic. The ads could be show on professional social
        networks.

-   1.  Increase benefits for riding during cold months. Coupons and
        discounts could be handed out.

-   1.  As the bikes are also used for recreations on the weekends, ads
        campaigns could also be made showing people using the bikes for
        exercise during the weeks. The ads could focus on how practical
        and **consistent** the bikes can be.

## Conclusion

The Google Analytics Professional Certificate teaches me a lot and the R
language is really useful for analyzing data.
