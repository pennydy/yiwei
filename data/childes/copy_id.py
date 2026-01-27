import re
import argparse

# Regex to capture sent_id and the file info line
FILE_HEADR = re.compile(r'^\*\*\*\s*File\s+"([^"]+)"\s*:\s*line\s*(\d+)', re.MULTILINE)
SENT_ID = re.compile(r'^\s*sent_id:\s*(\d+)', re.MULTILINE)

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="clean files")
    parser.add_argument("--input_id", "-id", type=str, default="juede_clean.txt")
    parser.add_argument("--input", "-i", type=str, default="juede_child_clean.txt")
    parser.add_argument("--output", "-o", type=str, default="juede_child_clean_id.txt")
    args = parser.parse_args()

    # input_id: file that contains the ids to be matched
    # input: file without the ids
    # output: output file

    # Step 1: Build dictionary file_path, file_line -> sent_id from input_id
    mapping = {}
    with open(args.input_id, "r") as f:
        text = f.read()

    # Find each block in file A
    blocks = text.split("----------------------------------------")
    for block in blocks:
        m_id = SENT_ID.search(block)
        m_header = FILE_HEADR.search(block)
        if m_id and m_header:
            sent_id = m_id.group(1)
            path = m_header.group(1)
            ln = int(m_header.group(2))
            mapping[(path, ln)] = sent_id

    # Step 2: Go through file B and annotate blocks with recovered sent_id
    output = []
    with open(args.input, "r") as f:
        current_id = None
        for line in f:
            # Detect *** File line
            header = FILE_HEADR.match(line)
            if header:
                path = header.group(1)
                ln = int(header.group(2))
                current_id = mapping.get((path, ln))
                output.append(f"sent_id: {current_id if current_id else 'UNKNOWN'}\n")
            output.append(line)

    with open(args.output, "w") as f:
        f.writelines(output)

