library(dplyr)
library(tidyr)

# 1. Function to remove all non-BMP characters (U+10000 and above)
#    plus escape special LaTeX characters in one pass.
sanitize_text <- function(x) {
  # Remove characters in the range U+10000 to U+10FFFF (which includes most emojis).
  # Use a Perl-compatible regex to do this:
  x <- gsub("[\U00010000-\U10FFFF]", "", x, perl = TRUE)

  # Now escape LaTeX special chars:
  # Order matters: escape backslash first
  x <- gsub("\\\\", "\\\\textbackslash ", x)
  x <- gsub("\\{", "\\\\{", x)
  x <- gsub("\\}", "\\\\}", x)
  x <- gsub("\\$", "\\\\$", x)
  x <- gsub("\\&", "\\\\&", x)
  x <- gsub("\\#", "\\\\#", x)
  x <- gsub("\\%", "\\\\%", x)
  x <- gsub("\\^", "\\\\^", x)
  x <- gsub("\\_", "\\\\_", x)
  x <- gsub("\\~", "\\\\textasciitilde ", x)

  return(x)
}

# 2. Load your CSV data.
replies <- read.csv("data/processed/bluesky_replies_labelled.csv", stringsAsFactors = FALSE)

# 3. Filter out rows with missing classifications.
replies <- replies %>%
  filter(!is.na(positive_sentiment_gpt) & !is.na(constructive_gpt))

# 4. Convert classification columns into factors (optional).
replies$positive_sentiment_gpt <- factor(
  replies$positive_sentiment_gpt,
  levels = c(0, 1),
  labels = c("Negative", "Positive")
)
replies$constructive_gpt <- factor(
  replies$constructive_gpt,
  levels = c(0, 1),
  labels = c("Non-Constructive", "Constructive")
)

# 5. Identify original posts (where in_reply_to is blank).
replies <- replies %>%
  mutate(original_post = ifelse(trimws(in_reply_to) == "", text, NA_character_)) %>%
  fill(original_post, .direction = "down")

# 6. Calculate stats for Positive examples by original_post.
positive_stats <- replies %>%
  filter(trimws(in_reply_to) != "") %>%
  group_by(original_post) %>%
  summarise(
    total_replies = n(),
    positive_count = sum(positive_sentiment_gpt == "Positive", na.rm = TRUE)
  ) %>%
  mutate(positive_proportion = positive_count / total_replies)

positive_threshold <- 0.75
high_positive_conversations <- positive_stats %>%
  filter(positive_proportion >= positive_threshold)

# 7. Calculate stats for Constructive examples by original_post.
constructive_stats <- replies %>%
  filter(trimws(in_reply_to) != "") %>%
  group_by(original_post) %>%
  summarise(
    total_replies = n(),
    constructive_count = sum(constructive_gpt == "Constructive", na.rm = TRUE)
  ) %>%
  mutate(constructive_proportion = constructive_count / total_replies)

constructive_threshold <- 0.3
high_constructive_conversations <- constructive_stats %>%
  filter(constructive_proportion >= constructive_threshold)

# 8. Extract relevant replies.
examples_positive <- replies %>%
  filter(
    trimws(in_reply_to) != "",
    original_post %in% high_positive_conversations$original_post,
    positive_sentiment_gpt == "Positive"
  )

examples_constructive <- replies %>%
  filter(
    trimws(in_reply_to) != "",
    original_post %in% high_constructive_conversations$original_post,
    constructive_gpt == "Constructive"
  )

# 9. Write out a .tex file. All non-BMP (emoji) removed, special chars escaped.
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

output_con <- file("data/output/output.tex", encoding = "UTF-8")
sink(output_con)

cat("% This file has no emojis or non-BMP characters.\n")
cat("% You can safely compile with pdfLaTeX, XeLaTeX, or LuaLaTeX.\n\n")

cat("\\section*{Examples of Original Posts with High Positive Sentiment Replies}\n\n")
for (op in head(unique(high_positive_conversations$original_post), 5)) {
  cat("\\subsubsection*{Original Post:}\n")
  cat(sanitize_text(op), "\n\n")
  cat("\\textbf{Replies:}\n\n")

  convo_replies <- examples_positive %>% filter(original_post == op)
  for (reply in convo_replies$text) {
    cat("- ", sanitize_text(reply), "\n\n", sep = "")
  }
  cat("\\bigskip\n")
}

cat("\\section*{Examples of Original Posts with High Constructive Replies}\n\n")
for (op in head(unique(high_constructive_conversations$original_post), 5)) {
  cat("\\subsubsection*{Original Post:}\n")
  cat(sanitize_text(op), "\n\n")
  cat("\\textbf{Replies:}\n\n")

  convo_replies <- examples_constructive %>% filter(original_post == op)
  for (reply in convo_replies$text) {
    cat("- ", sanitize_text(reply), "\n\n", sep = "")
  }
  cat("\\bigskip\n")
}

sink()
close(output_con)
