# Loading necessary libraries
library(ggplot2)
library(dplyr)

# Read the cleaned dataset
data <- read.csv("C:/Users/Priyal/Dropbox/My PC (LAPTOP-ND4LQUT1)/Downloads/Bengaluru_Restaurants.csv")
View(data)

# Visualization 1: Bar chart of top 10 cuisines
cuisine_counts <- data %>% 
  count(cuisine, sort = TRUE) %>% 
  top_n(10)

ggplot(cuisine_counts, aes(x = reorder(cuisine, n), y = n)) +
  geom_bar(stat = 'identity', fill = 'skyblue') +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = 'Top 10 Cuisines by Count', x = 'Cuisine', y = 'Count') +
  coord_flip()

# Visualization 2: Pie chart of meal types
meal_counts <- data %>% 
  count(Meal.Type)

ggplot(meal_counts, aes(x = "", y = n, fill = Meal.Type)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar(theta = 'y') +
  geom_text(aes(label = paste0(Meal.Type, ' (', n, ')')), 
            position = position_stack(vjust = 0.5)) +
  labs(title = 'Distribution of Meal Types') +
  theme_void()

# Visualization 3: Line chart of average rating by state
rating_state <- data %>% 
  group_by(addressObj.state) %>% 
  summarise(avg_rating = mean(rating, na.rm = TRUE))

ggplot(rating_state, aes(x = reorder(addressObj.state, avg_rating), y = avg_rating, group = 1)) +
  geom_line(color = 'blue') +
  geom_point() +
  geom_text(aes(label = round(avg_rating, 2)), vjust = -0.5) +
  labs(title = 'Average Rating by State', x = 'State', y = 'Average Rating') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization 4: Histogram of Ratings
ggplot(data, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = 'orange', color = 'black') +
  geom_text(stat='bin', binwidth = 0.5, aes(label = ..count..), vjust = -0.5) +
  labs(title = 'Distribution of Ratings', x = 'Rating', y = 'Count')

# Visualization 5: Bar chart of top 10 dietary restrictions
dietary_counts <- data %>% 
  count(DietaryRestrictions, sort = TRUE) %>% 
  top_n(10)

ggplot(dietary_counts, aes(x = reorder(DietaryRestrictions, n), y = n)) +
  geom_bar(stat = 'identity', fill = 'purple') +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = 'Top 10 Dietary Restrictions', x = 'Dietary Restrictions', y = 'Count') +
  coord_flip()

# Visualization 6: Line chart of ranking position vs. raw ranking
ranking_trend <- data %>% 
  arrange(rankingPosition) %>% 
  mutate(id = row_number())

ggplot(ranking_trend, aes(x = id, y = rawRanking)) +
  geom_line(color = 'green') +
  geom_point() +
  labs(title = 'Raw Ranking Trend vs Ranking Position', x = 'Index', y = 'Raw Ranking')

# Visualization 7: Histogram of number of reviews
ggplot(data, aes(x = numberOfReviews)) +
  geom_histogram(binwidth = 50, fill = 'blue', color = 'black') +
  geom_text(stat='bin', binwidth = 50, aes(label = ..count..), vjust = -0.5) +
  labs(title = 'Distribution of Number of Reviews', x = 'Number of Reviews', y = 'Count')

# Visualization 8: Bar chart of top 10 features
feature_counts <- data %>% 
  count(Features, sort = TRUE) %>% 
  top_n(10)

ggplot(feature_counts, aes(x = reorder(Features, n), y = n)) +
  geom_bar(stat = 'identity', fill = 'red') +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = 'Top 10 Features', x = 'Feature', y = 'Count') +
  coord_flip()

# Visualization 9: Average reviews by cuisine
reviews_by_cuisine <- data %>% 
  group_by(cuisine) %>% 
  summarise(avg_reviews = mean(numberOfReviews, na.rm = TRUE)) %>% 
  top_n(10, avg_reviews)

ggplot(reviews_by_cuisine, aes(x = reorder(cuisine, avg_reviews), y = avg_reviews)) +
  geom_bar(stat = 'identity', fill = 'cyan') +
  geom_text(aes(label = round(avg_reviews, 1)), vjust = -0.5) +
  labs(title = 'Average Number of Reviews by Cuisine', x = 'Cuisine', y = 'Average Reviews') +
  coord_flip()

# Visualization 10: Pie chart of top 5 states by count
state_counts <- data %>% 
  count(addressObj.state, sort = TRUE) %>% 
  top_n(5)

ggplot(state_counts, aes(x = "", y = n, fill = addressObj.state)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar(theta = 'y') +
  geom_text(aes(label = paste0(addressObj.state, ' (', n, ')')), 
            position = position_stack(vjust = 0.5)) +
  labs(title = 'Top 5 States by Restaurant Count') +
  theme_void()

# Visualization 11: Correlation between ranking and reviews
correlation_data <- data %>% 
  filter(!is.na(rankingPosition) & !is.na(numberOfReviews))

ggplot(correlation_data, aes(x = rankingPosition, y = numberOfReviews)) +
  geom_point(color = 'darkred', alpha = 0.6) +
  geom_smooth(method = 'lm', color = 'blue') +
  labs(title = 'Correlation between Ranking and Number of Reviews', x = 'Ranking Position', y = 'Number of Reviews')

# Visualization 12: Top cuisines with high ratings
cuisine_ratings <- data %>% 
  group_by(cuisine) %>% 
  summarise(avg_rating = mean(rating, na.rm = TRUE)) %>% 
  top_n(10, avg_rating)

ggplot(cuisine_ratings, aes(x = reorder(cuisine, avg_rating), y = avg_rating)) +
  geom_bar(stat = 'identity', fill = 'lightgreen') +
  geom_text(aes(label = round(avg_rating, 2)), vjust = -0.5)+
  labs(title = 'Top Cuisines with High Ratings', x = 'Cuisine', y = 'Average Rating') +
  coord_flip()

# Visualization 13: Scatter plot of latitude vs longitude
restaurant_locations <- data %>% 
  filter(!is.na(latitude) & !is.na(longitude))

ggplot(restaurant_locations, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.4, color = 'purple') +
  labs(title = 'Scatter Plot of Restaurant Locations', x = 'Longitude', y = 'Latitude')

# Visualization 14: Distribution of raw ranking
raw_ranking_data <- data %>% 
  filter(!is.na(rawRanking))

ggplot(raw_ranking_data, aes(x = rawRanking)) +
  geom_histogram(binwidth = 0.1, fill = 'gold', color = 'black') +
  geom_text(stat = 'bin', binwidth = 0.1, aes(label = ..count..), vjust = -0.5) +
  labs(title = 'Distribution of Raw Ranking', x = 'Raw Ranking', y = 'Count')

# Visualization 15: Bar chart of top states with average reviews
state_avg_reviews <- data %>% 
  group_by(addressObj.state) %>% 
  summarise(avg_reviews = mean(numberOfReviews, na.rm = TRUE)) %>% 
  top_n(10, avg_reviews)

ggplot(state_avg_reviews, aes(x = reorder(addressObj.state, avg_reviews), y = avg_reviews)) +
  geom_bar(stat = 'identity', fill = 'orange') +
  geom_text(aes(label = round(avg_reviews, 1)), vjust = -0.5) +
  labs(title = 'Top States by Average Reviews', x = 'State', y = 'Average Reviews') +
  coord_flip()

# Visualization 16: Line chart of average raw ranking by state
raw_ranking_state <- data %>% 
  group_by(addressObj.state) %>% 
  summarise(avg_raw_ranking = mean(rawRanking, na.rm = TRUE))

ggplot(raw_ranking_state, aes(x = reorder(addressObj.state, avg_raw_ranking), y = avg_raw_ranking, group = 1)) +
  geom_line(color = 'darkblue') +
  geom_point() +
  geom_text(aes(label = round(avg_raw_ranking, 2)), vjust = -0.5) +
  labs(title = 'Average Raw Ranking by State', x = 'State', y = 'Average Raw Ranking') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization 17: Density plot of ratings by meal type
meal_rating_data <- data %>% 
  filter(!is.na(rating))

ggplot(meal_rating_data, aes(x = rating, fill = Meal.Type)) +
  geom_density(alpha = 0.5) +
  labs(title = 'Density Plot of Ratings by Meal Type', x = 'Rating', y = 'Density')

# Visualization 18: Bar chart of top cuisines by rating count
rating_cuisine_count <- data %>% 
  count(cuisine, wt = rating, sort = TRUE) %>% 
  top_n(10)

ggplot(rating_cuisine_count, aes(x = reorder(cuisine, n), y = n)) +
  geom_bar(stat = 'identity', fill = 'pink') +
  geom_text(aes(label = round(n, 1)), vjust = -0.5) +
  labs(title = 'Top Cuisines by Rating Count', x = 'Cuisine', y = 'Rating Count') +
  coord_flip()

# Visualization 19: Heatmap of rankings and reviews
rank_review_heatmap_data <- data %>% 
  filter(!is.na(rankingPosition) & !is.na(numberOfReviews))

ggplot(rank_review_heatmap_data, aes(x = rankingPosition, y = numberOfReviews)) +
  geom_bin2d(binwidth = c(50, 50), fill = 'steelblue', alpha = 0.7) +
  labs(title = 'Heatmap of Ranking and Reviews', x = 'Ranking Position', y = 'Number of Reviews')

# Visualization 20: Scatter plot with trend of raw ranking vs number of reviews
trend_data <- data %>% 
  filter(!is.na(rawRanking) & !is.na(numberOfReviews))

ggplot(trend_data, aes(x = rawRanking, y = numberOfReviews)) +
  geom_point(alpha = 0.6, color = 'brown') +
  geom_smooth(method = 'lm', color = 'red') +
  labs(title = 'Trend of Raw Ranking vs Number of Reviews', x = 'Raw Ranking', y = 'Number of Reviews')
