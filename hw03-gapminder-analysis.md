STAT 545A Homework 3
================
Lucy Bellemare
September 30, 2018

Homework 3
==========

Introduction
============

The goal of this assignment is to explore the use of dplyr and ggplot2 to explore, draw conclusions, and visualize datasets.

Load Packages
-------------

``` r
library(tidyverse)
library(knitr)
library(gridExtra)
library(gapminder)
```

Get the maximum and minimum of GDP per capita for all continents.
=================================================================

``` r
gapminder %>% 
  group_by(continent) %>% 
  summarize(minGDPperCap = min(gdpPercap),
            maxGDPperCap = max(gdpPercap)) %>% 
  mutate(rangeGDPperCap = maxGDPperCap - minGDPperCap) %>% 
  knitr::kable()
```

| continent |  minGDPperCap|  maxGDPperCap|  rangeGDPperCap|
|:----------|-------------:|-------------:|---------------:|
| Africa    |      241.1659|      21951.21|        21710.05|
| Americas  |     1201.6372|      42951.65|        41750.02|
| Asia      |      331.0000|     113523.13|       113192.13|
| Europe    |      973.5332|      49357.19|        48383.66|
| Oceania   |    10039.5956|      34435.37|        24395.77|

From this we can see the maximum and minimum GDP per capita for each continent, considering from 1952-2007. We can see that Africa has the lowest minimum GDP per capita and the lowest maximum GDP per capita. We also can see that Asia has the largest different between their minimum and maximum GDP per capitas.

Look at the spread of GDP per capita within the continents.
-----------------------------------------------------------

Compute a trimmed mean of life expectancy for different years. Or a weighted mean, weighting by population. Just try something other than the plain vanilla mean.

How is life expectancy changing over time on different continents?

Report the absolute and/or relative abundance of countries with low life expectancy over time by continent: Compute some measure of worldwide life expectancy - you decide - a mean or median or some other quantile or perhaps your current age. Then determine how many countries on each continent have a life expectancy less than this benchmark, for each year.
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

For this exercise, I'm going to exclude the continent of Oceania, as it has a substantially smaller sample size than the other continents, and the minimum life expectancy value observed in Oceania is:

``` r
gapminder %>% 
  filter(continent=="Oceania") %>% 
  summarize(minLifeExp = min(lifeExp)) %>% 
  kable()
```

|  minLifeExp|
|-----------:|
|       69.12|

And we will use 65 as the boundary between low and high life expectancy. If we were to include Oceania, the graphs would be uninformative as both countries in Oceania have had life expectancies greater than 65 for every year reported.

Find countries with interesting stories. Open-ended and, therefore, hard. Promising but unsuccessful attempts are encouraged. This will generate interesting questions to follow up on in class.

How is Oceania doing
--------------------

In the continent of Oceania, there are two countries. Let's see how they compare to each other over time.

First let's build a dataset of just these two countries:

``` r
Oceania <- gapminder %>% 
  filter(continent=="Oceania")
unique(Oceania$country)
```

    ## [1] Australia   New Zealand
    ## 142 Levels: Afghanistan Albania Algeria Angola Argentina ... Zimbabwe

We note that while the dataset Oceania has only data for the countries Australia and New Zealand, the varaible country still contains all 142 levels from the original dataset. This makes sense as country is a factor. Factors require all levels be encoded in the variable, regardless of whether or not they are still present in a new data frame.

### Initial Plots

``` r
OPopVsYear <- ggplot(Oceania, aes(x=year, y=pop)) +
  # scale_y_log10() +
  geom_point(aes(colour=country), show.legend=F)

OLifeExpVsYear <- ggplot(Oceania, aes(x=year, y=lifeExp)) +
  # scale_y_log10() +
  geom_point(aes(colour=country), show.legend=F)

OGDPPercapVsYear <- ggplot(Oceania, aes(x=year, y=gdpPercap)) +
  # scale_y_log10() +
  geom_point(aes(colour=country))

grid.arrange(OPopVsYear, OLifeExpVsYear, OGDPPercapVsYear,
             ncol = 7, nrow = 2, layout_matrix = rbind(c(1,1,2,2,3,3,3), c(1,1,2,2,3,3,3)),
             top = "Comparing the Countries of Oceania from 1952-2007")
```

![](hw03-gapminder-analysis_files/figure-markdown_github/OceaniaPlots1-1.png)

### GDP per Capita

Life Expectancy
---------------
