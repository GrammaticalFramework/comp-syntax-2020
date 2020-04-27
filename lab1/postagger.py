import argparse
from os.path import isfile, splitext

'''
A script that takes in a CONLL file and uses it to write a file containing
all of its sentences POS-tagged, ready to be parsed by gf-ud.
'''

def postag(line):
    "Annotate a token with its POS tag, skipping comments"
    if line[0] == "#":
        return ""
    elif line == '\n':
        return line
    else:
        newline = '\t'.join(line.split()) # substitute spaces with tabs
        newline = newline.split("#", 1)[0][:-1] # remove comments
        newline = tuple(newline.split(sep="\t"))
        word = newline[1]
        tag = newline[2] if args.simplified else newline[3]
        newline = "{}:<{}> ".format(word, tag)
        return newline

if __name__== "__main__":

    parser = argparse.ArgumentParser(
        description='''A script that takes in a CONLL file and uses it to
        write a file containing all of its sentences POS-tagged, ready to
        be parsed by gf-ud.''')
    parser.add_argument('path', help='path to the CONLL file')
    parser.add_argument(
        "--simplified",
        help="use a simplified CONLL file instead.",
        action="store_true")
    args = parser.parse_args()

    filepath = args.path

    if not isfile(filepath):
        print("File not found!")
        exit(1)

    newlines = []

    # annotate word by word
    with open(filepath, 'r') as source:
        newlines = list(map(postag, source.readlines()))

    # write POS-tagged sentences to a new file in the same folder
    path, ext = splitext(filepath)
    with open("{}-pos{}".format(path, ext), 'w+') as target:
        target.writelines(newlines)
