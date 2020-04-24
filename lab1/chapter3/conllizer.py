import argparse
from os.path import isfile, splitext

def conllize(line):
    '''
    (Inefficiently) converts a line to its standard conll format (10 tokens)
    '''
    if line[0] == "#":
        return "# text = " + line[2:]
    elif line == '\n':
        return line
    else:
        newline = '\t'.join(line.split()) # substitute spaces with tabs
        newline = newline.split("#", 1)[0][:-1] # remove comments
        print(tuple(newline.split(sep="\t")))
        (i, word, pos, head, label) = tuple(newline.split(sep="\t"))
        # add missing tokens
        tokens = [i, word, '_', pos, '_', '_', head, label, '_', '_']
        newline = '\t'.join(tokens)
        newline += "\n"
        return newline

if __name__== "__main__":

    parser = argparse.ArgumentParser(
        description='''A simple script that takes in a "simplified CONLL" file
                       and converts it into actual, standard CONLL.''')
    parser.add_argument('path', help='path to the simplified CONLL file')
    args = parser.parse_args()

    filepath = args.path

    if not isfile(filepath):
        print("File not found!")
        exit(1)

    newlines = []

    # convert each line to standard CONLL
    with open(filepath, 'r') as source:
        newlines = list(map(conllize, source.readlines()))

    # write them to a new file in the same folder
    path, ext = splitext(filepath)
    with open("{}-std{}".format(path, ext), 'w+') as target:
        target.writelines(newlines)
