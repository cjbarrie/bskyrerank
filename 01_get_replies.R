library(atrrr)
library(dplyr)

posts <- search_post("Democrats", limit = 100000, parse = TRUE, verbose = TRUE)

# Filter posts with more than 10 replies
filtered_posts <- posts %>% filter(reply_count > 10)

# Create directory if it doesn't exist
if (!dir.exists("data/replies")) {
  dir.create("data/replies", recursive = TRUE)
}

# Iterate over filtered posts and save replies
for (i in seq_along(filtered_posts$uri)) {
  post_uri <- filtered_posts$uri[i]
  
  # Remove the part before "did"
  sanitized_uri <- sub(".*did:", "did:", post_uri)
  
  replies <- get_replies(post_uri)
  
  # Save replies to an RDS file
  file_name <- paste0("data/replies/replies_", gsub("[^a-zA-Z0-9]", "_", sanitized_uri), ".rds")
  saveRDS(replies, file_name)
}

# Read in all RDS files and combine into a single data frame
reply_files <- list.files("data/replies", full.names = TRUE, pattern = "\\.rds$")
all_replies <- lapply(reply_files, readRDS) %>% bind_rows()

# Select the desired columns
selected_columns <- c("uri", "cid", "author_handle", "author_name", "text", "in_reply_to")
combined_replies <- all_replies %>% select(all_of(selected_columns))

# Create the processed directory if it doesn't exist
if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

# Save the combined replies to a CSV file
write.csv(combined_replies, "data/processed/combined_replies.csv", row.names = FALSE)

# View the combined data frame
print(combined_replies)
