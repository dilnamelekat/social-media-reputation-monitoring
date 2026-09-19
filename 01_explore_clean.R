library(tidyverse)
library(textclean)

# Load dataset
df <- read.csv(
  "data/social_media_data.csv",
  header = TRUE,
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)

# Give proper column names
names(df) <- c("id", "topic", "sentiment", "text")

# Remove duplicate tweets and empty tweets
df <- df %>%
  distinct(text, .keep_all = TRUE) %>%
  drop_na(text)

# Clean tweet text
clean_text <- function(x) {
  x <- replace_url(x)
  x <- replace_tag(x, replacement = "")
  x <- str_replace_all(x, "#", "")
  x <- str_replace_all(x, "[^A-Za-z\\s]", "")
  x <- str_to_lower(x)
  x <- str_squish(x)
  x
}

# Create cleaned text
df <- df %>%
  mutate(clean_text = clean_text(text))

# Check result
dim(df)
table(df$sentiment)
write.csv(df, "data/cleaned_tweets.csv", row.names = FALSE)

