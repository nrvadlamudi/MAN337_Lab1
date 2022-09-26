library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)

data <- read.csv("WorldCupData.csv")

# filter out games that have not been played yet
data <- data %>%
    filter(Date < "9/24/2022") %>%
    filter(HomeGoals != "NA") %>%
    filter(AwayGoals != "NA")

# 1
# get average goals scored by home team, how much their opponenet scored, and how many games they played # nolint # nolint
HomeAvg <- data %>%
    group_by(Home) %>%
    summarize(avgHomeGoals = mean(HomeGoals), avgOppGoals = mean(AwayGoals), Games = length(Country), HomeWinPct = sum(HomeGoals > AwayGoals) / Games) %>%
    # add a column for the difference between the average goals scored and the average goals scored by the opponent
    mutate(Diff = avgHomeGoals - avgOppGoals) %>%
    # sort by biggest Diff and avgHomeGoals
    arrange(desc(HomeWinPct), desc(Diff), desc(avgHomeGoals)) %>%
    filter(Games > 12)

# print all rows of HomeAvg
print(HomeAvg, n = Inf)

AwayAvg <- data %>%
    group_by(Away) %>%
    summarise(avgAwayGoals = mean(AwayGoals), avgOppGoals = mean(HomeGoals), Games = length(AwayGoals), AwayWinPct = (sum(AwayGoals > HomeGoals) / Games)) %>%
    mutate(Diff = avgAwayGoals - avgOppGoals) %>%
    arrange(desc(AwayWinPct), desc(Diff), desc(avgAwayGoals)) %>%
    filter(Games > 12)

print(AwayAvg, n = Inf)

# 2
# filter data to date of the year 2018 and summarize which team had the most goals
MostGoals1 <- data %>%
    # filter to get Date of 2018
    filter(Date > "1/1/2018" & Date < "12/31/2018") %>%
    # rename Home to Country
    mutate(Country = Home) %>%
    group_by(Country) %>%
    summarise(HomeGoals = sum(HomeGoals)) %>%
    arrange(desc(HomeGoals))


MostGoals2 <- data %>%
    filter(Date > "1/1/2018" & Date < "12/31/2018") %>%
    mutate(Country = Away) %>%
    group_by(Country) %>%
    summarise(AwayGoals = sum(AwayGoals)) %>%
    arrange(desc(AwayGoals))

print(MostGoals1, n = Inf)
print(MostGoals2, n = Inf)
# inner join the two data frames with country as the key
MostGoals <- merge(x = MostGoals1, y = MostGoals2, by = "Country", all = TRUE) %>%
    # add a column for the total goals scored
    summarise(Country = Country, TotalGoals = HomeGoals + AwayGoals) %>%
    # sort by total goals scored
    arrange(desc(TotalGoals))

print(MostGoals)

# Issue in #2 where the string of Home and Away country is not the same, needs further parsing to work fully

# 3

# filter Home and Away by average attendance and average goals scored
HomeAvg <- data %>%
    group_by(Home) %>%
    filter(Attendance != "NA") %>%
    # filter for Date in the past 10 years
    filter(Date > "1/1/2012") %>%
    summarise(avgHomeGoals = mean(HomeGoals), avgAtt = mean(Attendance), Games = length(Country)) %>%
    arrange(desc(avgAtt), desc(avgHomeGoals)) %>%
    filter(Games > 12)

print(HomeAvg, n = Inf)
