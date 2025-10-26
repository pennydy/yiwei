import re
import argparse

end_sentence_pattern = re.compile(r'%snd:[^\s]*')

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="clean files")
    parser.add_argument("--input", "-i", type=str, default="renwei_clean.txt")
    parser.add_argument("--output", "-o", type=str, default="renwei_clean_format.txt")
    args = parser.parse_args()

    input_file = args.input
    output_file = args.output

    with open(input_file, "r", encoding="utf-8") as f_in, open(output_file, "w", encoding="utf-8") as f_out:
        for line in f_in:
            if ':\t' in line:
                head, sep, tail = line.partition(':\t')
                tail = tail.replace(' ', '')  # remove all spaces after the tab
                line = head + sep + tail
            line = re.sub(r'[‡„]', ',', line)
            line = re.sub(r'(@s:eng|&~|\[\+R\]|\[\*s:cl\]|\[\+sub\]|\[\+bln\])|\[\+ygm\]|\[\+cjm\]|\[\+blm\]|\[\+bcn\]|\+|\[,sub\]|\[,bcn\]|\[,bln\]','',line)
            line = re.sub(r'([在给])\d+', r'\1', line)
            line = re.sub(end_sentence_pattern, '', line)
            line = re.sub("sent_id: \d+\n", '', line)
            if not line.startswith('From file <'):
                f_out.write(line)