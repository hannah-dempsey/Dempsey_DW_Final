data_cleaning
================
Hannah Dempsey

# Cleaning Rodent Survey Datasets

Set up:

``` r
#Week 3 (loading in packages)

#install.packages("tidyverse")
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

## Saguaro National Park

Reading in the dataset:

``` r
#Week 3 (read_csv) and Week 9 (file paths)

saguaro <- read_csv("../data_raw/saguaro_rodents.csv", skip = 5)
```

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 1216 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): Trap Check Date, Species, Recapture?, Sex, Age, Mass (g), Reprod. C...
    ## dbl (3): RHF, Body, Tail
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Cleaning the dataset:

``` r
#Week 3 (select and filter), Week 4 (mutate and pipes), Week 6 (rename and separate), and Week 9 (write_cvs)

saguaro_clean <- saguaro %>% 
  rename(Trap_Check_Date = `Trap Check Date`,
         Recapture = `Recapture?`,
         Mass_g = `Mass (g)`,
         Reprod_Cond = `Reprod. Cond.`,
         Web_Trap_Number = `Web Trap #`) %>% 
  separate(Species, c("Genus", "Species"), sep = " ") %>% 
  mutate(Trap_Check_Date = dmy(Trap_Check_Date),
         Year = year(Trap_Check_Date),
         Month = month(Trap_Check_Date),
         Day = day(Trap_Check_Date),
         Recapture = toupper(Recapture),
         Recapture = str_replace(Recapture, "R", "Y"),
         Recapture = replace_na(Recapture, "N"),
         Mass_g = as.numeric(Mass_g)) %>% 
  select(Trap_Check_Date, Year, Month, Day, everything()) %>% 
  filter(Genus != is.na(Genus)) %>% 
  filter(Species != "auduboni" & Species != "harrisii") %>% 
  rename_with(tolower)
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `Mass_g = as.numeric(Mass_g)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

``` r
saguaro_clean
```

    ## # A tibble: 1,212 × 16
    ##    trap_check_date  year month   day genus  species recapture sex   age   mass_g
    ##    <date>          <dbl> <dbl> <int> <chr>  <chr>   <chr>     <chr> <chr>  <dbl>
    ##  1 1991-03-15       1991     3    15 Chaet… penici… N         M     A       13  
    ##  2 1991-03-15       1991     3    15 Chaet… penici… N         M     A       16  
    ##  3 1991-03-15       1991     3    15 Chaet… penici… N         M     A       14.5
    ##  4 1991-03-15       1991     3    15 Chaet… penici… N         M     A       16  
    ##  5 1991-03-15       1991     3    15 Chaet… penici… N         M     A       14.5
    ##  6 1991-03-15       1991     3    15 Chaet… penici… N         M     A       15  
    ##  7 1991-03-15       1991     3    15 Chaet… penici… N         M     A       15  
    ##  8 1991-03-15       1991     3    15 Chaet… penici… N         M     A       16  
    ##  9 1991-03-15       1991     3    15 Chaet… penici… N         M     A       15  
    ## 10 1991-03-15       1991     3    15 Chaet… penici… N         M     A       17  
    ## # ℹ 1,202 more rows
    ## # ℹ 6 more variables: reprod_cond <chr>, web_trap_number <chr>, comments <chr>,
    ## #   rhf <dbl>, body <dbl>, tail <dbl>

``` r
write_csv(saguaro_clean, "../data_clean/saguaro_clean.csv")
```

## Organ Pipe National Monument

Reading in the datasets:

``` r
#Week 3 (read_csv) and Week 9 (file paths)

orpi_species_codes <- read_csv("../data_raw/tluRodentSpecies.csv")
```

    ## Rows: 18 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): SpeciesCode, GenusSpecies, Family, CreatedDate
    ## dbl (1): ID
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
orpi_data <- read_csv("../data_raw/RodentDetail.csv")
```

    ## Rows: 29880 Columns: 16
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (5): Capture1, Capture2, Capture3, Capture4, Notes
    ## dbl  (10): ID, RodentSurveyID, RodentSpeciesID, IDNum, SexID, AgeID, YearIDN...
    ## date  (1): CreatedDate
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
orpi_surveys <- read_csv("../data_raw/RodentSurvey.csv")
```

    ## Rows: 704 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): Personnel, Weather, Notes
    ## dbl  (9): ID, SiteID, Quadrat, NotID, NumTraps, NumSprungTraps, MoonPhaseNum...
    ## date (3): StartDate, EndDate, CreatedDate
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Cleaning the datasets:

``` r
#Week 3 (select), Week 4 (mutate, pipes, and joins), Week 6 (rename and separate), and Week 9 (write_csv)

orpi_species_codes <- orpi_species_codes %>% 
  select(-5) %>% 
  rename_with(tolower) %>% 
  rename(rodent_species_id = id,
         species_code = speciescode) %>% 
  separate(genusspecies, c("genus", "species"), sep = " ")

orpi_surveys <- orpi_surveys %>% 
  rename_with(tolower) %>% 
  select(-6, -9, -11, -12, -13, -14, -15) %>% 
  rename(rodent_survey_id = id,
         site_id = siteid,
         start_date = startdate,
         end_date = enddate,
         num_traps = numtraps)

orpi_clean <- orpi_data %>% 
  rename_with(tolower) %>% 
  select(1:3, 9:10) %>%
  rename(rodent_survey_id = rodentsurveyid,
         rodent_species_id = rodentspeciesid) %>% 
  left_join(orpi_species_codes, join_by(rodent_species_id)) %>% 
  left_join(orpi_surveys, join_by(rodent_survey_id))

orpi_clean
```

    ## # A tibble: 29,880 × 16
    ##        id rodent_survey_id rodent_species_id weight recapture species_code genus
    ##     <dbl>            <dbl>             <dbl>  <dbl>     <dbl> <chr>        <chr>
    ##  1 144775               22               112   NA           1 CHPE         Chae…
    ##  2 144776               22               119   NA           1 PEAM         Pero…
    ##  3 144777               22               115   30.6         0 DIME         Dipo…
    ##  4 144778               22               115   32.2         0 DIME         Dipo…
    ##  5 144779               22               115   40           0 DIME         Dipo…
    ##  6 144780               22               112   14.2         0 CHPE         Chae…
    ##  7 144781               22               112   NA           1 CHPE         Chae…
    ##  8 144782               22               115   43.8         0 DIME         Dipo…
    ##  9 144783               22               115   45.5         0 DIME         Dipo…
    ## 10 144784               22               115   41.8         0 DIME         Dipo…
    ## # ℹ 29,870 more rows
    ## # ℹ 9 more variables: species <chr>, family <chr>, site_id <dbl>,
    ## #   start_date <date>, end_date <date>, quadrat <dbl>, weather <chr>,
    ## #   notes <chr>, num_traps <dbl>

``` r
write_csv(orpi_clean, "../data_clean/orpi_clean.csv")
write_csv(orpi_surveys, "../data_clean/orpi_surveys.csv")
```
