Example 1 - Loading aimsir17
================

The first step (after installing the package) is to load the library,
along with the library dplyr.

``` r
library(aimsir17)
library(dplyr)
```

There are two datasets that can be used. The first is **observations**

``` r
observations
```

    ## # A tibble: 219,000 x 12
    ##    station  year month   day  hour date                 rain  temp  rhum
    ##    <chr>   <dbl> <dbl> <int> <int> <dttm>              <dbl> <dbl> <dbl>
    ##  1 ATHENRY  2017     1     1     0 2017-01-01 00:00:00   0     5.2    89
    ##  2 ATHENRY  2017     1     1     1 2017-01-01 01:00:00   0     4.7    89
    ##  3 ATHENRY  2017     1     1     2 2017-01-01 02:00:00   0     4.2    90
    ##  4 ATHENRY  2017     1     1     3 2017-01-01 03:00:00   0.1   3.5    87
    ##  5 ATHENRY  2017     1     1     4 2017-01-01 04:00:00   0.1   3.2    89
    ##  6 ATHENRY  2017     1     1     5 2017-01-01 05:00:00   0     2.1    91
    ##  7 ATHENRY  2017     1     1     6 2017-01-01 06:00:00   0     2      89
    ##  8 ATHENRY  2017     1     1     7 2017-01-01 07:00:00   0     1.7    89
    ##  9 ATHENRY  2017     1     1     8 2017-01-01 08:00:00   0     1      91
    ## 10 ATHENRY  2017     1     1     9 2017-01-01 09:00:00   0     1.1    91
    ## # … with 218,990 more rows, and 3 more variables: msl <dbl>, wdsp <dbl>,
    ## #   wddir <dbl>

To get see all the variables, use the glimpse() function.

``` r
glimpse(observations)
```

    ## Observations: 219,000
    ## Variables: 12
    ## $ station <chr> "ATHENRY", "ATHENRY", "ATHENRY", "ATHENRY", "ATHENRY", "…
    ## $ year    <dbl> 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 20…
    ## $ month   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ day     <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ hour    <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16…
    ## $ date    <dttm> 2017-01-01 00:00:00, 2017-01-01 01:00:00, 2017-01-01 02…
    ## $ rain    <dbl> 0.0, 0.0, 0.0, 0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0…
    ## $ temp    <dbl> 5.2, 4.7, 4.2, 3.5, 3.2, 2.1, 2.0, 1.7, 1.0, 1.1, 3.0, 4…
    ## $ rhum    <dbl> 89, 89, 90, 87, 89, 91, 89, 89, 91, 91, 84, 78, 75, 72, …
    ## $ msl     <dbl> 1021.9, 1022.0, 1022.1, 1022.5, 1022.7, 1023.3, 1023.5, …
    ## $ wdsp    <dbl> 8, 9, 8, 9, 8, 8, 7, 7, 7, 8, 9, 12, 11, 12, 11, 11, 11,…
    ## $ wddir   <dbl> 320, 320, 320, 330, 330, 330, 330, 340, 330, 330, 320, 3…

The following variables are recored for each observation:

  - **station** (weather station name)
  - **year** (2017)
  - **month** (month of year 1-12)
  - **day** (day of month 1-31)
  - **date** (date object in R)
  - **rain** (hourly rainfall in mm)
  - **temp** (hourly temperature in C)
  - **rhum** (Relative Humidity - percent)
  - **msl** (Mean Sea Level Pressure - hPa)
  - **wdsp** (Mean Wind Speed - knots)
  - **wddir** (Predominant Wind Direction - degree)

The second data set is **stations**, which can be linked to
**observations** via the the variable **station**

``` r
stations %>% print(n=25)
```

    ## # A tibble: 25 x 5
    ##    station              county    height latitude longitude
    ##    <chr>                <chr>      <dbl>    <dbl>     <dbl>
    ##  1 ATHENRY              Galway        40     53.3     -8.79
    ##  2 BALLYHAISE           Cavan         78     54.1     -7.31
    ##  3 BELMULLET            Mayo           9     54.2    -10.0 
    ##  4 CASEMENT             Dublin        91     53.3     -6.44
    ##  5 CLAREMORRIS          Mayo          68     53.7     -8.99
    ##  6 CORK AIRPORT         Cork         155     51.8     -8.49
    ##  7 DUBLIN AIRPORT       Dublin        71     53.4     -6.24
    ##  8 DUNSANY              Meath         83     53.5     -6.66
    ##  9 FINNER               Donegal       33     54.5     -8.24
    ## 10 GURTEEN              Tipperary     75     53.1     -8.01
    ## 11 JOHNSTOWNII          Wexford       62     52.3     -6.50
    ## 12 KNOCK AIRPORT        Mayo         201     53.9     -8.82
    ## 13 MACE HEAD            Galway        21     53.3     -9.90
    ## 14 MALIN HEAD           Donegal       20     55.4     -7.34
    ## 15 MARKREE              Sligo         34     54.2     -8.46
    ## 16 MOORE PARK           Cork          46     52.2     -8.26
    ## 17 MT DILLON            Roscommon     39     53.7     -7.98
    ## 18 MULLINGAR            Westmeath    101     53.5     -7.36
    ## 19 NEWPORT              Mayo          22     53.9     -9.57
    ## 20 OAK PARK             Carlow        62     52.9     -6.92
    ## 21 PHOENIX PARK         Dublin        48     53.4     -6.35
    ## 22 ROCHES POINT         Cork          40     51.8     -8.24
    ## 23 SHANNON AIRPORT      Clare         15     52.7     -8.92
    ## 24 SherkinIsland        Cork          21     51.5     -9.43
    ## 25 VALENTIA OBSERVATORY Kerry         24     51.9    -10.2

The following variables are recored for each station:

  - **station** (weather station name)
  - **county** (location)
  - **height** (height above sea level (m))
  - **latitude** (location coordinates)
  - **longitude** (location coordinates)

A summary of all the observations can be viewed using dplyr.

``` r
observations %>% 
  group_by(station) %>% 
  summarise(TotalRecords=n()) %>%
  print(n=25)
```

    ## # A tibble: 25 x 2
    ##    station              TotalRecords
    ##    <chr>                       <int>
    ##  1 ATHENRY                      8760
    ##  2 BALLYHAISE                   8760
    ##  3 BELMULLET                    8760
    ##  4 CASEMENT                     8760
    ##  5 CLAREMORRIS                  8760
    ##  6 CORK AIRPORT                 8760
    ##  7 DUBLIN AIRPORT               8760
    ##  8 DUNSANY                      8760
    ##  9 FINNER                       8760
    ## 10 GURTEEN                      8760
    ## 11 JOHNSTOWNII                  8760
    ## 12 KNOCK AIRPORT                8760
    ## 13 MACE HEAD                    8760
    ## 14 MALIN HEAD                   8760
    ## 15 MARKREE                      8760
    ## 16 MOORE PARK                   8760
    ## 17 MT DILLON                    8760
    ## 18 MULLINGAR                    8760
    ## 19 NEWPORT                      8760
    ## 20 OAK PARK                     8760
    ## 21 PHOENIX PARK                 8760
    ## 22 ROCHES POINT                 8760
    ## 23 SHANNON AIRPORT              8760
    ## 24 SherkinIsland                8760
    ## 25 VALENTIA OBSERVATORY         8760

A summary of the overall data set can also be
    viewed

``` r
summary(observations)
```

    ##    station               year          month             day       
    ##  Length:219000      Min.   :2017   Min.   : 1.000   Min.   : 1.00  
    ##  Class :character   1st Qu.:2017   1st Qu.: 4.000   1st Qu.: 8.00  
    ##  Mode  :character   Median :2017   Median : 7.000   Median :16.00  
    ##                     Mean   :2017   Mean   : 6.526   Mean   :15.72  
    ##                     3rd Qu.:2017   3rd Qu.:10.000   3rd Qu.:23.00  
    ##                     Max.   :2017   Max.   :12.000   Max.   :31.00  
    ##                                                                    
    ##       hour            date                          rain        
    ##  Min.   : 0.00   Min.   :2017-01-01 00:00:00   Min.   : 0.0000  
    ##  1st Qu.: 5.75   1st Qu.:2017-04-02 05:45:00   1st Qu.: 0.0000  
    ##  Median :11.50   Median :2017-07-02 11:30:00   Median : 0.0000  
    ##  Mean   :11.50   Mean   :2017-07-02 11:30:00   Mean   : 0.1227  
    ##  3rd Qu.:17.25   3rd Qu.:2017-10-01 17:15:00   3rd Qu.: 0.0000  
    ##  Max.   :23.00   Max.   :2017-12-31 23:00:00   Max.   :16.6000  
    ##                                                NA's   :157      
    ##       temp           rhum             msl              wdsp       
    ##  Min.   :-6.2   Min.   : 20.00   Min.   : 962.2   Min.   : 0.000  
    ##  1st Qu.: 7.5   1st Qu.: 77.00   1st Qu.:1007.5   1st Qu.: 5.000  
    ##  Median :10.6   Median : 86.00   Median :1015.6   Median : 8.000  
    ##  Mean   :10.3   Mean   : 83.65   Mean   :1014.4   Mean   : 9.426  
    ##  3rd Qu.:13.4   3rd Qu.: 93.00   3rd Qu.:1022.3   3rd Qu.:12.000  
    ##  Max.   :28.3   Max.   :100.00   Max.   :1039.1   Max.   :59.000  
    ##  NA's   :166    NA's   :144      NA's   :182      NA's   :17547   
    ##      wddir      
    ##  Min.   :  0.0  
    ##  1st Qu.:170.0  
    ##  Median :220.0  
    ##  Mean   :213.3  
    ##  3rd Qu.:270.0  
    ##  Max.   :360.0  
    ##  NA's   :17548
