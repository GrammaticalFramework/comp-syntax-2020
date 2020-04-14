# Lab 1: Grammatical analysis


This lab follows Chapters 1-4 in the course notes. Each part is started after the lecture on the corresponding chapter.
The assignments are submitted via Canvas.

## Chapter 1: explore the parallel UD treebank (PUD)

1. Go to https://universaldependencies.org/ and download Version 2.5 treebanks
2. Look up the Parallel UD treebanks for those 19 languages that have it. They are named e.g. UD_English-PUD/
3. Select a language to compare with English.
4. Make statistics about the frequencies of POS tags and dependency labels in your language compared with English.
  For instance, the top-10 tags/labels and their number of occurrences.
  What does this tell you about the language?
5. Convert 2x2 tree from CoNLL format to graphical tree by hand, on paper.
  Select a short English tree and its translation.
  Then select a long English tree and its translation.
6. Draw word alignments for some non-trivial example in the PUD treebank, on paper.
  Use the same trees as in the previous question.
  What can you say about the syntactic differences between the languages?


## Chapter 2: design the morpological types of the major parts of speech in your selected language

1. It is enough to cover NOUN, ADJ, and VERB.
2. Use a traditional grammar book or a Wikipedia article to identify the inflectional and inherent features.
3. Then use data from PUD to check which morphological features actually occur in the treebank for that language. 

## Chapter 3: UD syntax analysis

Take a bilingual corpus with English and your own language, and annotate with UD.
The UD annotation that you produce manually can be simplified CoNLL, with just the fields

`position word postag head label`

This can be automatically expanded to full CoNLL by adding undescores for the lemma, morphology, and other missing fields, as well as tabs between the fields (if you didn't use tabs already)

`position    word   _    postag   _   _   head   label   _   _`

Example:

`7 world NOUN 4 nmod`

expands to

`7       world   _       NOUN    _       _       4       nmod    _       _`

Once you have full CoNLL, you can use for instance the gfud tool to visualize it.

The corpus is given in the file comp-syntax-corpus-english.txt in this directory.
Your task is to
1. write an English CoNLL file analysing this corpus
2. translate the corpus to your language
3. write a CoNLL file analysing your translation


The corpus is a combination of different sources, including the Parallel UD treebank (PUD).
If you want to cheat - or just check your own answer - you can look for those sentences in the official PUD.




## Chapter 4: phrase structure analysis

1. Write a set of liberal phrase structure rules for your selected language covering the same structures as the Notes. 
2. The English grammar gathered from the Notes will appear in this Git directory.



