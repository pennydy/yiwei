import re
import sys
from pathlib import Path
import argparse

def add_id(input_path: str, output_path: str):
    text = Path(input_path).read_text(encoding="utf-8")
    lines = text.splitlines(keepends=True)

    re_header = re.compile(r'^\*\*\* File .+?(Keyword|Keywords):\s*.*$', re.M)

    out_lines = []
    block_id = 0
    for ln in lines:
        # Insert the sent_id line immediately before the header line
        if re_header.match(ln):
            block_id += 1
            out_lines.append(f"sent_id: {block_id}\n")
        out_lines.append(ln)

    Path(output_path).write_text("".join(out_lines), encoding="utf-8")
    print(f"Numbered {block_id} blocks → {output_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="clean files")
    parser.add_argument("--input", "-i", type=str, default="renwei_clean.txt")
    parser.add_argument("--output", "-o", type=str, default="renwei_clean_format.txt")
    args = parser.parse_args()

    input_file = args.input
    output_file = args.output

    add_id(input_file, output_file)
