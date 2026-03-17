import re
import sys
from pathlib import Path
import argparse

FILE_HEADR = re.compile(r'^\*\*\*\s*File\s+"([^"]+)"\s*:\s*line\s*(\d+)', re.MULTILINE)

def add_id(input_path: str, output_path: str, start_number: int):
    text = Path(input_path).read_text(encoding="utf-8")
    lines = text.splitlines(keepends=True)

    out_lines = []
    block_id = start_number
    for ln in lines:
        # Insert the sent_id line immediately before the header line
        if FILE_HEADR.match(ln):
            block_id += 1
            out_lines.append(f"sent_id: {block_id}\n")
        out_lines.append(ln)

    Path(output_path).write_text("".join(out_lines), encoding="utf-8")
    print(f"Numbered {block_id} blocks → {output_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="clean files")
    parser.add_argument("--input", "-i", type=str, default="renwei_clean.txt")
    parser.add_argument("--num", "-n", type=int, default=0)
    args = parser.parse_args()

    input_file = args.input
    file_name = re.sub(r"\.txt$", "", args.input)
    output_file = f"{file_name}_id.txt"
    start_number = args.num

    add_id(input_file, output_file, start_number)
