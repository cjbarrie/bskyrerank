library(dplyr)
library(ggplot2)
library(scales)
library(tidylog)
library(tidyr)  # for fill()

# Load the combined replies data
replies <- read.csv("data/processed/bluesky_replies_labelled.csv", stringsAsFactors = FALSE)

# Remove rows with NA in 'positive_sentiment_gpt' and 'constructive_gpt'
replies <- replies %>% 
  filter(!is.na(positive_sentiment_gpt) & !is.na(constructive_gpt))

# Convert the columns into factors
replies$positive_sentiment_gpt <- factor(replies$positive_sentiment_gpt, 
                                         levels = c(0, 1), 
                                         labels = c("Negative", "Positive"))
replies$constructive_gpt <- factor(replies$constructive_gpt, 
                                   levels = c(0, 1), 
                                   labels = c("Non-Constructive", "Constructive"))

# Identify the original post for each conversation:
# For rows where in_reply_to is blank, the text is the original post.
# Then propagate that original_post value to all replies in the thread.
replies <- replies %>%
  mutate(original_post = ifelse(trimws(in_reply_to) == "", text, NA_character_)) %>%
  fill(original_post, .direction = "down")

# For plotting, we only want the replies (exclude the original post row)
replies <- replies %>% filter(trimws(in_reply_to) != "")

### SENTIMENT PLOT ###

# Calculate count and proportion for each sentiment per original post
sentiment_distribution <- replies %>%
  group_by(original_post, positive_sentiment_gpt) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(original_post) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Compute a unique ratio per original post: Negative proportion / (Positive proportion)
sentiment_ratio <- sentiment_distribution %>%
  group_by(original_post) %>%
  summarise(
    Negative = sum(proportion[positive_sentiment_gpt == "Negative"]),
    Positive = sum(proportion[positive_sentiment_gpt == "Positive"])
  ) %>%
  mutate(ratio = Negative / (Positive + 1e-6))  # small value to avoid division by zero

# Order original posts from fully negative (high ratio) to fully positive (low ratio)
order_levels_sentiment <- sentiment_ratio %>%
  arrange(desc(ratio)) %>%  
  pull(original_post)

# Apply this ordering to the sentiment_distribution data
sentiment_distribution <- sentiment_distribution %>%
  mutate(original_post = factor(original_post, levels = order_levels_sentiment))

# Create the sentiment plot.
sentiment_plot <- ggplot(sentiment_distribution, aes(x = original_post, y = proportion, fill = positive_sentiment_gpt)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Negative" = "#D55E00", "Positive" = "#009E73")) +
  labs(x = "Post (one per line)", y = "Proportion of Replies", fill = "Sentiment") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_line(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip()

### CONSTRUCTIVENESS PLOT ###

# Calculate count and proportion for each constructiveness per original post
constructive_distribution <- replies %>%
  group_by(original_post, constructive_gpt) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(original_post) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Compute a unique ratio per original post: Non-Constructive proportion / (Constructive proportion)
constructive_ratio <- constructive_distribution %>%
  group_by(original_post) %>%
  summarise(
    Non_Constructive = sum(proportion[constructive_gpt == "Non-Constructive"]),
    Constructive = sum(proportion[constructive_gpt == "Constructive"])
  ) %>%
  mutate(ratio = Non_Constructive / (Constructive + 1e-6))

# Order original posts from fully non-constructive (high ratio) to fully constructive (low ratio)
order_levels_constructive <- constructive_ratio %>%
  arrange(desc(ratio)) %>%  
  pull(original_post)

# Apply this ordering to the constructive_distribution data
constructive_distribution <- constructive_distribution %>%
  mutate(original_post = factor(original_post, levels = order_levels_constructive))

# Create the constructiveness plot.
constructiveness_plot <- ggplot(constructive_distribution, aes(x = original_post, y = proportion, fill = constructive_gpt)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Non-Constructive" = "#CC79A7", "Constructive" = "#56B4E9")) +
  labs(x = "Post (one per line)", y = "Proportion of Replies", fill = "Constructiveness") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_line(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip()

# Create the "plots" directory if it doesn't exist.
if(!dir.exists("plots")){
  dir.create("plots")
}

# Save the plots as PNG files in the "plots" directory.
ggsave("plots/sentiment_plot.png", sentiment_plot, width = 8, height = 6, dpi = 300)
ggsave("plots/constructiveness_plot.png", constructiveness_plot, width = 8, height = 6, dpi = 300)
