import re
import csv
import argparse
from collections import Counter

# Regex to capture sent_id and the file info line
SENT_ID = re.compile(r'^\s*sent_id:\s*(\d+)', re.MULTILINE)
KEYWORD = re.compile(r'Keywords?:\s*([^,\s]+)')
FILE_HEADR = re.compile(r'^\*\*\*\s*File\s+"([^"]+)"\s*:\s*line\s*(\d+)', re.MULTILINE)
UTTERANCES_RE = re.compile(r'^\*([^:]+):\s*(.*)', re.MULTILINE)

speaker_mapping = {
    "MOT": "mother",
    "FAT": "father",
    "CHI": "child",
    "SIS": "sister",
    "BRO": "brother",
    "GMO": "grandma",
    "ADU": "adult",
    "ADU1": "adult",
    "ADU2": "adult",
    "CHI2": "other_child",
    "SHE": "investigator",
    "EXP": "investigator",
    "EXA": "investigator",
    "INV": "investigator",
    "TSA": "investigator",
    "TEA": "teacher"
}


def extract_text(text):
    rows = []
    block_id = 0
    # Find each block in the file
    blocks = re.split(r'-{20,}', text)
    for block in blocks:
        block = block.strip()
        if not block:
            continue
        block_id += 1
        m_header = FILE_HEADR.search(block)
        m_id = SENT_ID.search(block)
        if m_id and m_header:
            sent_id = m_id.group(1)
            sent_header = m_header.group(1)
        else:
            if m_id is None:
                print(f"Warning: Could not find sent_id in block {block_id}:\n{block}")
            if m_header is None:
                print(f"Warning: Could not find file header in block {block_id}:\n{block}")
            raise ValueError(f"Error: Could not find sent_id or file header in block {block_id}:\n{block}")
        
        m_keyword = KEYWORD.search(block)
        keyword = m_keyword.group(1) if m_keyword else None

        lines = block.splitlines()

        # remove the first two lines (sent_id and file header)
        lines = lines[2:]
        utterance_count = 0

        if len(lines) == 11:
            m = UTTERANCES_RE.match(lines[5])
            speaker, sentence = m.groups()
            if keyword in sentence:
                clean_sentence = f"{speaker}: {sentence}"
                speaker_full = speaker_mapping.get(speaker, speaker)
                rows.append([sent_id, sent_header, speaker_full, clean_sentence])
                continue
        else:
            for line in lines:
                m = UTTERANCES_RE.match(line)
                if not m:
                    continue
                speaker, sentence = m.groups()
                utterance_count += 1

                if speaker == "CHI":
                    continue

                if keyword in sentence:
                    clean_sentence = f"{speaker}: {sentence}"
                    speaker_full = speaker_mapping.get(speaker, speaker)
                    rows.append([sent_id, sent_header, speaker_full, clean_sentence])
                
                if utterance_count >= 6:
                    break
    return rows


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="clean files")
    parser.add_argument("--input", "-i", type=str, default="renwei_clean.txt")
    parser.add_argument("--check", "-c", type=str, default=None)

    args = parser.parse_args()
    
    file_name = re.sub(r"\.txt$", "", args.input)
    
    with open(args.input, "r", encoding="utf-8") as f:
        text = f.read()
    
    # to get the csv file for single sentencs
    if not args.check:
        rows = extract_text(text)

        with open(f"{file_name}.csv", "w", encoding="utf-8") as f:
            writer = csv.writer(f)
            writer.writerow(["id","file_name", "speaker", "sentence"])
            writer.writerows(rows)
    
    # to check the generated csv file for duplicated ids and missing ids
    else:
        # 1. get the duplicated ids
        csv_ids = []
        csv_file = args.check
        with open(csv_file,"r", encoding="utf-8") as f:
            reader = csv.DictReader(f)
            for row in reader:
                csv_ids.append(int(row["id"]))
        id_counts = Counter(csv_ids)
        duplicate_ids = [i for i, count in id_counts.items() if count > 1]
        print("Duplicate IDs:", duplicate_ids)

        # 2. check if there are missing ids from the original txt file
        text_ids = [int(x) for x in SENT_ID.findall(text)]
        text_ids = set(text_ids)
        csv_ids = set(csv_ids)

        missing_ids = text_ids - csv_ids
        print("Missing IDs in CSV:", missing_ids)

        



