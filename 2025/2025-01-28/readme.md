# Water Insecurity

This week we're exploring water insecurity data featured in the article [Mapping water insecurity in R with tidycensus](https://waterdata.usgs.gov/blog/acs-maps/)!

> Water insecurity can be influenced by number of social vulnerability indicators—from demographic characteristics to living conditions and socioeconomic status —that vary spatially across the U.S. This blog shows how the tidycensus package for R can be used to access U.S. Census Bureau data, including the American Community Surveys, as featured in the “Unequal Access to Water ” data visualization from the USGS Vizlab. It offers reproducible code examples demonstrating use of tidycensus for easy exploration and visualization of social vulnerability indicators in the Western U.S.

- How does the lack of complete indoor plumbing compare between the 2023 and 2022 Census data? 
- What counties have the greatest percent of households lacking plumbing?
- Are there differences in indoor plumbing availability between Western U.S and Eastern U.S counties? 

Thank you to [Niha Pereira](https://github.com/nnpereira) for curating this week's dataset.

## The Data

```r
tuesdata <- tidytuesdayR::tt_load('2025-01-28')
## OR
tuesdata <- tidytuesdayR::tt_load(2025, week = 4)

water_insecurity_2022 <- tuesdata$water_insecurity_2022
water_insecurity_2023 <- tuesdata$water_insecurity_2023
```

### Data Dictionary

# `water_insecurity_2022.csv`

|variable                 |class            |description                           |
|:------------------------|:----------------|:-------------------------------------|
|geoid                    |character        |The U.S. Census Bureau ACS county id.  |
|name                     |character        |The U.S. Census Bureau ACS county name. |
|year                     |character        |The year of U.S. Census Bureau ACS sample. |
|geometry                 |sfc_MULTIPOLYGON |The county geographic boundaries. |
|total_pop                |double           |The total population. |
|plumbing                 |double           |The total owner occupied households lacking plumbing facilities. |
|percent_lacking_plumbing |double           |The percent of population lacking plumbing facilities. |

# `water_insecurity_2023.csv`

|variable                 |class            |description                           |
|:------------------------|:----------------|:-------------------------------------|
|geoid                    |character        |The U.S. Census Bureau ACS county id.  |
|name                     |character        |The U.S. Census Bureau ACS county name. |
|year                     |character        |The year of U.S. Census Bureau ACS sample. |
|geometry                 |sfc_MULTIPOLYGON |The county geographic boundaries. |
|total_pop                |double           |The total population. |
|plumbing                 |double           |The total owner occupied households lacking plumbing facilities. |
|percent_lacking_plumbing |double           |The percent of population lacking plumbing facilities. |

### My Shiny APP

+ I just want to build a shiny app using modules and practice on shiny tests

