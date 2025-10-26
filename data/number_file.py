import re
import sys
from pathlib import Path

def main(input_path: str, output_path: str):
    text = Path(input_path).read_text(encoding="utf-8")
    lines = text.splitlines(keepends=True)

    re_header = re.compile(r'^\*\*\* File .+?Keyword:\s*.*$', re.M)

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
    if len(sys.argv) < 3:
        print("Usage: python number_blocks.py <input.txt> <output.txt>")
        sys.exit(1)
    main(sys.argv[1], sys.argv[2])
