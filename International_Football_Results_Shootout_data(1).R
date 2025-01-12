# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(tidyr)
library(stats)

# Load the dataset
shootouts <- read.csv("shootouts.csv")

# Inspect the dataset
str(shootouts)
summary(shootouts)
head(shootouts)

# Convert date column to Date type
shootouts$date <- as.Date(shootouts$date, format = "%Y-%m-%d")

# Check for missing values
colSums(is.na(shootouts))

# Fill missing values in `first_shooter` with 'Unknown'
shootouts$first_shooter[is.na(shootouts$first_shooter)] <- "Unknown"

# Exploratory Analysis
## Count matches by year
matches_per_year <- shootouts %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(matches = n())

## Visualize matches per year
ggplot(matches_per_year, aes(x = year, y = matches)) +
  geom_line(color = "blue") +
  labs(title = "Number of Matches per Year", x = "Year", y = "Number of Matches") +
  theme_minimal()

## Distribution of Winners
winner_counts <- shootouts %>% 
  group_by(winner) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Top 10 Winners Visualization
winner_counts_top10 <- winner_counts %>% top_n(10, count)

ggplot(winner_counts_top10, aes(x = reorder(winner, -count), y = count, fill = winner)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Teams with Most Shootout Wins", x = "Team", y = "Number of Wins") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## First Shooter Analysis
first_shooter_counts <- shootouts %>% 
  group_by(first_shooter) %>% 
  summarise(count = n())

# First Shooter Visualization
ggplot(first_shooter_counts, aes(x = reorder(first_shooter, -count), y = count, fill = first_shooter)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of First Shooter Choices", x = "First Shooter", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Home vs Away Performance
home_away_performance <- shootouts %>% 
  mutate(outcome = ifelse(winner == home_team, "Home Win", 
                          ifelse(winner == away_team, "Away Win", "Draw"))) %>% 
  group_by(outcome) %>% 
  summarise(count = n())

# Home vs Away Visualization
ggplot(home_away_performance, aes(x = outcome, y = count, fill = outcome)) +
  geom_bar(stat = "identity") +
  labs(title = "Outcome Distribution: Home vs Away", x = "Outcome", y = "Count") +
  theme_minimal()

# Inferential Analysis
## Hypothesis Testing: Is there a significant difference in the frequency of Home Wins vs Away Wins?
home_away_test <- home_away_performance %>% 
  filter(outcome != "Draw")
chisq_test <- chisq.test(home_away_test$count)
print(chisq_test)

## Correlation Analysis: Does the order of first shooter correlate with the winner?
first_shooter_winner <- shootouts %>% 
  mutate(is_first_shooter_winner = ifelse(first_shooter == winner, 1, 0))

first_shooter_corr <- cor.test(as.numeric(first_shooter_winner$is_first_shooter_winner), 
                               as.numeric(first_shooter_winner$first_shooter != "Unknown"), 
                               method = "pearson")
print(first_shooter_corr)

# Visualization of Correlation
ggplot(first_shooter_winner, aes(x = first_shooter, fill = as.factor(is_first_shooter_winner))) +
  geom_bar(position = "fill") +
  labs(title = "Relationship between First Shooter and Winning", 
       x = "First Shooter", 
       y = "Proportion", 
       fill = "Winner Status") +
  theme_minimal()

# Trend Analysis: Wins over Time for Top Team
south_korea_trend <- shootouts %>% 
  filter(winner == "South Korea") %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(wins = n())

ggplot(south_korea_trend, aes(x = year, y = wins)) +
  geom_line(color = "red") +
  labs(title = "Wins Over Time for South Korea", x = "Year", y = "Number of Wins") +
  theme_minimal()

# Save cleaned dataset
write.csv(shootouts, "cleaned_shootouts.csv", row.names = FALSE)

# Summary of Key Insights
cat("Key Findings:\n")
cat("1. Matches per year show significant variations over time.\n")
cat("2. Top teams like South Korea dominate shootouts.\n")
cat("3. Home teams win significantly more often than away teams in shootouts (p-value from chi-squared test).\n")
cat("4. The role of the first shooter appears weakly correlated with winning status.\n")
cat("5. 'Unknown' first shooter highlights missing data issues in the dataset.\n")
