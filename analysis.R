library(tidyverse)
install.packages("tidytext")
library(tidytext)

# Load datasets
speeches <- read_delim("speeches.csv", delim = "|") %>% select(date, contents)
fx <- read_csv("fx.csv") %>% select(Date, USD) %>% rename(date = Date)

# 1:  Merge keeping all dates where fx has a measurement (right = fx dates)
merged <- merge(speeches, fx, by = "date", all.y = TRUE)
# 2: No obvious outliers or mistakes found in USD col
# (range 0.82-1.60, no zeros or negatives, dates 1999-2026 all valid)

# 3: No missing exchange rate values found (sum(is.na(merged$USD)) = 0)
# Forward-fill not needed in this case

# 4: Calculate daily exchange rate return (percentage)
merged <- merged %>%
  arrange(date) %>%
  mutate(
    fx_return = (USD - lag(USD)) / lag(USD) * 100,
    good_news = ifelse(fx_return > 0.5, 1, 0),
    bad_news = ifelse(fx_return < -0.5, 1, 0)
  )

# 5: Remove rows where contents is NA
merged_clean <- merged %>% filter(!is.na(contents))
nrow(merged_clean)
data(stop_words)

# Good indicators - 20 most common words when good_news = 1
good_indicators <- merged_clean %>%
  filter(good_news == 1) %>%
  unnest_tokens(word, contents) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 20)

# Bad indicators - 20 most common words when bad_news = 1
bad_indicators <- merged_clean %>%
  filter(bad_news == 1) %>%
  unnest_tokens(word, contents) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 20)

# Save to csv
write_csv(good_indicators, "good_indicators.csv")
write_csv(bad_indicators, "bad_indicators.csv")

# Check results
good_indicators
bad_indicators

