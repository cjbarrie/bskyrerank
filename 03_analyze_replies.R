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

# ALSO convert 'polarization_gpt' into a factor (if not already):
# Make sure your CSV has these exact strings:
#   "Not polarizing", "Somewhat polarizing", "Very polarizing"
replies$polarization_gpt <- factor(replies$polarization_gpt,
                                   levels = c("Not polarizing", 
                                              "Somewhat polarizing",
                                              "Very polarizing"))

# Identify the original post for each conversation
replies <- replies %>%
  mutate(original_post = ifelse(trimws(in_reply_to) == "", text, NA_character_)) %>%
  fill(original_post, .direction = "down")

# Exclude the original post row, so we only have replies
replies <- replies %>% filter(trimws(in_reply_to) != "")

### 1) SENTIMENT PLOT ###

sentiment_distribution <- replies %>%
  group_by(original_post, positive_sentiment_gpt) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(original_post) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Compute ratio: Negative / Positive
sentiment_ratio <- sentiment_distribution %>%
  group_by(original_post) %>%
  summarise(
    Negative = sum(proportion[positive_sentiment_gpt == "Negative"]),
    Positive = sum(proportion[positive_sentiment_gpt == "Positive"])
  ) %>%
  mutate(ratio = Negative / (Positive + 1e-6))

# Order from fully negative (high ratio) to fully positive (low ratio)
order_levels_sentiment <- sentiment_ratio %>%
  arrange(desc(ratio)) %>%  
  pull(original_post)

# Apply this ordering
sentiment_distribution <- sentiment_distribution %>%
  mutate(original_post = factor(original_post, levels = order_levels_sentiment))

sentiment_plot <- ggplot(sentiment_distribution, 
                         aes(x = original_post, y = proportion, fill = positive_sentiment_gpt)) +
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

### 2) CONSTRUCTIVENESS PLOT ###

constructive_distribution <- replies %>%
  group_by(original_post, constructive_gpt) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(original_post) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Compute ratio: Non-Constructive / Constructive
constructive_ratio <- constructive_distribution %>%
  group_by(original_post) %>%
  summarise(
    Non_Constructive = sum(proportion[constructive_gpt == "Non-Constructive"]),
    Constructive = sum(proportion[constructive_gpt == "Constructive"])
  ) %>%
  mutate(ratio = Non_Constructive / (Constructive + 1e-6))

# Order from fully non-constructive (high ratio) to fully constructive (low ratio)
order_levels_constructive <- constructive_ratio %>%
  arrange(desc(ratio)) %>%  
  pull(original_post)

constructive_distribution <- constructive_distribution %>%
  mutate(original_post = factor(original_post, levels = order_levels_constructive))

constructiveness_plot <- ggplot(constructive_distribution, 
                                aes(x = original_post, y = proportion, fill = constructive_gpt)) +
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

### 3) POLARIZATION PLOT ###

# Group by original_post and polarization_gpt, compute proportion
polarization_distribution <- replies %>%
  group_by(original_post, polarization_gpt) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(original_post) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# We need a ratio to order from "most polarizing" to "least polarizing", or vice versa.
# Example ratio: sum(Somewhat + Very) / (Not + 1e-6).
#   => Higher ratio = more polarizing, lower ratio = less polarizing.
polarization_ratio <- polarization_distribution %>%
  group_by(original_post) %>%
  summarise(
    not_pol = sum(proportion[polarization_gpt == "Not polarizing"]),
    some_pol = sum(proportion[polarization_gpt == "Somewhat polarizing"]),
    very_pol = sum(proportion[polarization_gpt == "Very polarizing"])
  ) %>%
  mutate(ratio = (some_pol + very_pol) / (not_pol + 1e-6))

# Order from fully not polarizing (low ratio) to fully polarizing (high ratio)
order_levels_polarization <- polarization_ratio %>%
  arrange(ratio) %>%  
  pull(original_post)

# Apply this ordering to the distribution data
polarization_distribution <- polarization_distribution %>%
  mutate(original_post = factor(original_post, levels = order_levels_polarization))

# Choose colors for the 3 categories:
# e.g. a small custom palette
my_pol_colors <- c("Not polarizing" = "#E69F00",
                   "Somewhat polarizing" = "#F0E442",
                   "Very polarizing" = "#D55E00")

polarization_plot <- ggplot(polarization_distribution, 
                            aes(x = original_post, y = proportion, fill = polarization_gpt)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = my_pol_colors) +
  labs(x = "Post (one per line)", y = "Proportion of Replies", fill = "Polarization") +
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

# Save the plots
ggsave("plots/sentiment_plot.png", sentiment_plot, width = 8, height = 6, dpi = 300)
ggsave("plots/constructiveness_plot.png", constructiveness_plot, width = 8, height = 6, dpi = 300)
ggsave("plots/polarization_plot.png", polarization_plot, width = 8, height = 6, dpi = 300)
