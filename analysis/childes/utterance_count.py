import re
import os
import argparse

# Set the path to the folder containing your files
if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="count utterances")
    parser.add_argument("--input", "-i", type=str, default="../../data/child")
    args = parser.parse_args()

    folder_path = args.input

    # Initialize a counter
    total_utterances = 0

    # Loop through each file in the folder
    for filename in os.listdir(folder_path):
        if filename.endswith(".txt"):
            file_path = os.path.join(folder_path, filename)
            with open(file_path, "r", encoding="utf-8") as f:
                content = f.read()
                # Extract and sum all utterance counts
                utterance_counts = [int(match) for match in re.findall(r"Number of: utterances = (\d+)", content)]
                total_utterances += sum(utterance_counts)

    print("Total utterances across all files:", total_utterances)
