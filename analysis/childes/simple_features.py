import numpy
print(numpy.__file__)
import pandas as pd
import argparse

def contains_negation(text, verb):
    if pd.isna(text):
        raise ValueError("Input text cannot be NaN")
    
    if verb == "以为":
        alt_verb = "以為"
    elif verb == "觉得":
        alt_verb = "覺得"
    elif verb == "知道":
        alt_verb = "知道"
    else:
        raise ValueError("Unsupported verb")
    
    mei = ["没" + verb, "没有" + verb, "沒" + alt_verb, "沒有" + alt_verb]
    youmeiyou = ["有没有" + verb, "有沒有" + alt_verb]
    anota = ["以不以为", "以不以為", "以没以为", "以沒以為", "以为不以为", "以為不以為", "以为没以为", "以為沒以為",
             "觉不觉得", "覺不覺得", "觉没觉得", "覺沒覺得", "觉得不觉得", "覺得不覺得", "觉得没觉得", "覺得沒覺得",
             "知不知道",   "知没知道",  "知沒知道",  "知道不知道","知道沒知道"]
    bu = ["不" + verb, "不" + alt_verb]
    bie = ["别" + verb, "別" + alt_verb]

    if any(mei_word in text for mei_word in mei):
        if any(youmeiyou_word in text for youmeiyou_word in youmeiyou) or any(anota_word in text for anota_word in anota):
            return "0 No"
        else:
            return "1 没"
    elif any(bu_word in text for bu_word in bu):
        return "2 不"
    elif any(bie_word in text for bie_word in bie):
        return "3 others"
    else:
        return "0 No"
    

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="count utterances")
    parser.add_argument("--input", "-i", type=str, default="../../data/childes/juede_nonchild_clean_id.csv")
    parser.add_argument("--output", "-o", type=str, default="../../data/childes/juede_nonchild_clean_id_negation.csv")
    parser.add_argument("--verb", "-v", type=str, default="以为")
    args = parser.parse_args()
    
    input_file = args.input
    output_file = args.output
    verb = args.verb

    df = pd.read_csv(input_file)
    df["matrix_negation"] = df["sentence"].apply(lambda x: contains_negation(x, verb))

    df.to_csv(output_file, index=False)