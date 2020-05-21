# Examples of embedded GF grammars in Python

The grammars use the Python binding available in

https://github.com/GrammaticalFramework/gf-core

see

`src/runtime/python/` 

for installing the Python module `pgf`, and

`src/runtime/c/`

for the C runtime library that the bindings are based on.
You don't need Haskell to build these.

A general tutorial on the Python bindings can be found in

http://www.grammaticalframework.org/doc/python-api.html

A guide to GF for Python programmers can be found in

https://daherb.github.io/GF-for-Python-programmers/


## Translator

A minimal translator can be found in

[`minitranslator.py`](./minitranslator.py)

This program reads one line of input and translates it from English to Swedish by using `MiniGrammar.pgf`. Example:
```
$ echo "the cat is black" | python3 minitranslator.py`
katten är svart
```
A more general version is in

[`translator.py`](./translator.py)

This program reads input line by line, tokenizes it (by a simple tokenizer), and uses an arbitrary pgf and language codes. Example:
```
$ cat findrawscript.txt | python3 translator.py Draw Fin Eng
# translating with Draw.pgf from DrawFin to DrawEng
draw a small red circle
draw a big yellow square
move the small red circle
remove it
```



