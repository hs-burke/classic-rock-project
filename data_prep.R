#reading in csv
data <- read.csv("data/athlete_events.csv")

#preparing data for cleaning
row_data <- data %>%
  filter(Sport == "Rowing") %>%
  mutate(Medal = as.character(Medal))

#turning all NA values to 0
row_data[is.na(row_data)] <- 0


download.file(
  "https://raw.githubusercontent.com/datasets/population/master/data/population.csv",
  destfile = "data/pop.csv"
)

world_pop <- read_csv("data/pop.csv")

# Changing variable name and format so I can merge with the rowing dataset
clean_pop <- world_pop %>%
  rename(NOC = "Country Code", total_pop = Value) %>%
  mutate(NOC = countrycode(NOC, origin = "iso3c", destination = "ioc"))


# Joining rowing data with population data
row_data_pop <-
  inner_join(clean_pop, row_data, by = c("NOC", "Year"))


# Reading in gsp data
gdp <- read_csv("data/gdp.csv")


# Reformatting and renaming data so I can merge with rowing + population dataset. I had to write in the name of every column in that nasty format,
# which I then cleaned up using str_extract to get rid of the square brackets
clean_gdp <- gdp %>%
  select(-"Series Name",-"Series Code") %>%
  pivot_longer(
    cols = c(
      "1960 [YR1960]",
      "1961 [YR1961]",
      "1962 [YR1962]",
      "1963 [YR1963]",
      "1964 [YR1964]",
      "1965 [YR1965]",
      "1966 [YR1966]",
      "1967 [YR1967]",
      "1968 [YR1968]",
      "1969 [YR1969]",
      "1970 [YR1970]",
      "1971 [YR1971]",
      "1972 [YR1972]",
      "1973 [YR1973]",
      "1974 [YR1974]",
      "1975 [YR1975]",
      "1976 [YR1976]",
      "1977 [YR1977]",
      "1978 [YR1978]",
      "1979 [YR1979]",
      "1980 [YR1980]",
      "1981 [YR1981]",
      "1982 [YR1982]",
      "1983 [YR1983]",
      "1984 [YR1984]",
      "1985 [YR1985]",
      "1986 [YR1986]",
      "1987 [YR1987]",
      "1988 [YR1988]",
      "1989 [YR1989]",
      "1990 [YR1990]",
      "1991 [YR1991]",
      "1992 [YR1992]",
      "1993 [YR1993]",
      "1994 [YR1994]",
      "1995 [YR1995]",
      "1996 [YR1996]",
      "1997 [YR1997]",
      "1998 [YR1998]",
      "1999 [YR1999]",
      "2000 [YR2000]",
      "2001 [YR2001]",
      "2002 [YR2002]",
      "2003 [YR2003]",
      "2004 [YR2004]",
      "2005 [YR2005]",
      "2006 [YR2006]",
      "2007 [YR2007]",
      "2008 [YR2008]",
      "2009 [YR2009]",
      "2010 [YR2010]",
      "2011 [YR2011]",
      "2012 [YR2012]",
      "2013 [YR2013]",
      "2014 [YR2014]",
      "2015 [YR2015]",
      "2016 [YR2016]",
      "2017 [YR2017]",
      "2018 [YR2018]",
      "2019 [YR2019]"
    ),
    names_to = "Year",
    values_to = "gdp_percap"
  ) %>%
  mutate(Year = str_extract(Year, "^\\d{4}")) %>%
  mutate(Year = as.integer(Year)) %>%
  rename(NOC = "Country Code") %>%
  mutate(NOC = countrycode(NOC, origin = "iso3c", destination = "ioc"))


# Joining rowing dataset that includes population data with the gdp dataset
big_data <-
  inner_join(clean_gdp, row_data_pop, by = c("NOC", "Year"))


model_data_gdp <- big_data %>%
  mutate(gdp_percap = as.double(gdp_percap)) %>%
  group_by(Year, Event, NOC) %>%
  distinct(Event, .keep_all = TRUE) %>%
  add_count(Medal) %>%
  pivot_wider(
    names_from = "Medal",
    values_from = "n",
    values_fill = list(n = 0)
  ) %>%
  group_by(Year, total_pop, gdp_percap) %>%
  mutate(ycount = Gold + Bronze + Silver) %>%
  group_by(Year, total_pop, gdp_percap, NOC) %>%
  summarize(
    Gold = sum(Gold),
    Bronze = sum(Bronze),
    Silver = sum(Silver),
    Total = sum(ycount)
  ) %>%
  pivot_longer(
    cols = c(Gold, Bronze, Silver, Total),
    names_to = "Medal",
    values_to = "nmedals"
  ) %>%
  filter(Medal == "Total") %>%
  mutate(medals_per_mil = (nmedals / total_pop) * 1000000)



