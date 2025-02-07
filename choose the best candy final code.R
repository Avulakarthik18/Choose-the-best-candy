
# Load necessary packages
library(tidyverse)
library(broom)
library(corrplot)
library(caret) 

# Load the candy_rankings dataset 
candy_data <- read.csv("candy-data.csv")

# Take a glimpse() at the dataset
glimpse(candy_data)
# Reshape data for visualization
candy_data_long <- gather(data = candy_data, key = feature, value = value, chocolate:pluribus)
candy_data_long

# Making a bar plot showing the distribution of each variable
ggplot(candy_data_long, aes(x = value, fill = feature)) +
  geom_bar() +
  xlab("Value") +
  ylab("Count") +
  facet_wrap(.~ feature) +
  theme(legend.position = "none")

# Price vs. Competitor Name plot
ggplot(candy_data, aes(x = reorder(competitorname, pricepercent), y = pricepercent)) +
  geom_segment(aes(xend = reorder(competitorname, pricepercent), yend = 0), size = 1, color = "orange") +
  geom_point(color = "purple", size = 1, alpha = 0.4) +
  xlab("Competitor Name") +
  ylab("Price per Cent") +
  coord_flip() #flips vertically

# Preferences vs. Competitor Name plot
ggplot(candy_data, aes(x = reorder(competitorname, winpercent), y = winpercent)) +
  geom_segment(aes(xend = reorder(competitorname, winpercent), yend = 0), color = "green") +
  geom_point(color = "blue", alpha = 0.4) +
  xlab("Competitor Name") +
  ylab("Preferences") +
  coord_flip()

# Correlation plot
corrplot(cor(candy_data[, 2:13]))

# Split data into train and test sets
set.seed(123) 
trainIndex <- createDataPartition(candy_data$winpercent, p = 0.7, list = FALSE)
train_data <- candy_data[trainIndex, ]
test_data <- candy_data[-trainIndex, ]

# Linear regression model for winpercent
win_mod <- lm(winpercent ~ ., data = train_data[, 2:13])
win_mod
summary(win_mod)

# Residual plot for win_mod
augment(win_mod) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(color = "green", size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "red")

# Logistic regression model for chocolate preference
choc_mod <- glm(chocolate ~ ., data = train_data[, 2:13], family = "binomial")
choc_mod
summary(choc_mod)

# Make predictions on the test set and create confusion matrix
preds <- augment(choc_mod, newdata = test_data, type.predict = "response") %>% 
  mutate(prediction = .fitted > 0.5)
conf_mat <- table(test_data$chocolate, preds$prediction)
conf_mat

# Calculate accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
accuracy

# Find qualities associated with higher rankings
coefficients <- tidy(win_mod)
coefficients

# Order coefficients by absolute value of estimate (to find most influential features)
ordered_coefficients <- coefficients[order(-abs(coefficients$estimate)), ]
ordered_coefficients

# Print top qualities associated with higher rankings
top_qualities <- ordered_coefficients$term[1:5]
print("Qualities associated with higher rankings:")
print(top_qualities)

# Find the most popular and least popular candies
most_popular <- candy_data[which.max(candy_data$winpercent), "competitorname"]
least_popular <- candy_data[which.min(candy_data$winpercent), "competitorname"]

print(paste("Most Popular Candy:", most_popular))
print(paste("Least Popular Candy:", least_popular))

# Calculate mean winpercent for each binary feature
binary_features <- select(candy_data, chocolate:pluribus)
mean_winpercent <- binary_features %>%
  mutate(winpercent = candy_data$winpercent) %>%
  gather(key = "feature", value = "presence", -winpercent) %>%
  group_by(feature, presence) %>%
  summarize(mean_winpercent = mean(winpercent), .groups = 'drop')
print(mean_winpercent)

# Create the data frame for popularities
popularities <- data.frame(
  candy = c(most_popular, least_popular),
  winpercent = c(candy_data$winpercent[which.max(candy_data$winpercent)],
                 candy_data$winpercent[which.min(candy_data$winpercent)])
)

# Plot most and least popular candies
ggplot(popularities, aes(x = candy, y = winpercent, fill = candy)) +
  geom_bar(stat = "identity") +
  labs(x = "Candy", y = "Win Percent") +
  theme_minimal() 

