from utils import classify_content, process_dataset
from config import pd, openai, datetime, os, json

## Classify Bluesky posts dataset

function_name_posts = "classify_post"  # Note: function name cannot contain spaces
function_description_posts = (
    "This task involves reading some posts by Bluesky users. "
    "For each post, provide three labels: one label ('polarization') indicating how polarizing the post is "
    "(choose one from 'Not polarizing', 'Somewhat polarizing', or 'Very polarizing'), another label ('constructive') "
    "which should be 1 if the post contains a constructive political suggestion and 0 if it does not, "
    "and a final label ('positive_sentiment') which should be 1 if the sentiment of the post is positive and 0 if it is not."
)

output_variables_posts = {
    "type": "object",
    "properties": {
        "post": {
            "type": "string",
            "description": "The content of the post to be classified."
        },
        "polarization": {
            "type": "string",
            "enum": [
                "Not polarizing",
                "Somewhat polarizing",
                "Very polarizing"
            ],
            "description": (
                "Your best judgment of how polarizing the text is. "
                "If the post expresses no polarizing content, label it as 'Not polarizing'. "
                "If it is somewhat polarizing, label it as 'Somewhat polarizing'. "
                "If it is very polarizing, label it as 'Very polarizing'."
            )
        },
        "constructive": {
            "type": "integer",
            "enum": [0, 1],
            "description": "Your best judgment of whether the post contains a constructive political suggestion (1) or not (0)."
        },
        "positive_sentiment": {
            "type": "integer",
            "enum": [0, 1],
            "description": "Your best judgment of whether the sentiment of the post is positive (1) or not (0)."
        }
    },
    "required": ["post", "polarization", "constructive", "positive_sentiment"]
}

# Load the Bluesky dataset
posts = pd.read_csv("data/processed/combined_replies.csv")  # Update with your actual file path

# Specify the column containing the text to classify
text_column = 'text'  # Ensure this matches the column name in your Bluesky data

# Output CSV file location
output_csv = "data/processed/bluesky_replies_labelled.csv"

# Process the dataset (or subsample)
processed_dfsamp = process_dataset(
    posts, 
    classify_content, 
    output_variables_posts, 
    function_description_posts, 
    function_name_posts, 
    text_column,
    output_csv
)
