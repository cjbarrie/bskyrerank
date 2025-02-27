from config import pd, openai, datetime, os, json, client

def classify_content(content, output_variables, function_description, function_name):
    function = {
        "name": function_name,
        "description": function_description,
        "parameters": output_variables,
    }
    message_content = f"{function_description}: \n\n'{content}'"

    error_response = {var: pd.NA for var in output_variables['properties'].keys()}

    try:
        response = client.chat.completions.create(
            model="gpt-4o-mini",
            messages=[{"role": "user", "content": message_content}],
            functions=[function],
            max_tokens=1000
        )
        # Check if function call and arguments exist
        if response.choices and response.choices[0].message and response.choices[0].message.function_call:
            function_call_arguments = response.choices[0].message.function_call.arguments
            if function_call_arguments:  # Check if arguments is not empty
                # Parse the function call arguments as JSON
                result = json.loads(function_call_arguments)
            else:
                print("Function call arguments are empty.")
                return error_response
        else:
            print("Expected function call data not found in response.")
            return error_response
        
    except json.JSONDecodeError:
        print("Failed to decode JSON from function call arguments.")
        return error_response
    except Exception as e:
        print(f"An error occurred: {e}")
        return error_response

    return result

def process_dataset(dfsamp, classify_function, output_variables, function_description, function_name, text_column, output_csv):
    import os

    if dfsamp is None:
        print("No data to process.")
        return

    # File that records processed post IDs.
    processed_ids_file = "processed_ids.txt"

    # Read already processed post IDs from file, if it exists.
    if os.path.exists(processed_ids_file):
        with open(processed_ids_file, "r") as f:
            processed_ids = set(line.strip() for line in f if line.strip())
    else:
        processed_ids = set()

    total_rows = len(dfsamp)
    print(f"Total rows: {total_rows}. Already processed: {len(processed_ids)}.")

    for index, row in dfsamp.iterrows():
        post_id = str(row.get("uri", ""))  # Adjust to match your data identifier
        if post_id in processed_ids:
            print(f"Skipping post {post_id} (already processed).")
            continue

        print(f"Processing row {index + 1}/{total_rows} (post_id: {post_id})...")
        content = row[text_column]
        result = classify_function(content, output_variables, function_description, function_name)

        # Update each classification column based on the result.
        for key in output_variables['properties'].keys():
            dfsamp.at[index, key + '_gpt'] = result.get(key, pd.NA)

        # Append the processed post id to the file
        with open(processed_ids_file, "a") as f:
            f.write(post_id + "\n")

        # Extract only the processed row
        processed_row = dfsamp.iloc[[index]]
        # Append this row to the output CSV:
        # If the file doesn't exist, write with headers; otherwise, append without headers.
        if not os.path.exists(output_csv):
            processed_row.to_csv(output_csv, index=False, mode='w', header=True)
        else:
            processed_row.to_csv(output_csv, index=False, mode='a', header=False)

    print("\nProcessing complete.")
    return dfsamp
