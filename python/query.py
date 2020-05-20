# a simple query system for arithmetic, using Montague-style semantics
# and an embedded GF grammar
# examples: "is 131 prime", "which numbers are prime"

import pgf

# we assume this abstract syntax
absmodule = "Query"

# change this to change input language
langname = absmodule + "Eng"

# the meaning of "number" in unrestricted quantification
allNumbers = range(1,10000)

def answer(tree):
  fun,args = tree.unpack()
  
  if fun == "QWhich":
    ans = enumerate(lambda x: predicate(args[0])(x) and predicate(args[1])(x))
  elif fun == "QWhether":
    ans = quantifier(args[0])(predicate(args[1]))
  else: # QWhat
    ans = value(args[0])
    
  return str(ans)

# returns int -> bool
def predicate(property):
  prop,pargs = property.unpack()
  if prop == "PAnd":
    return lambda x: predicate(pargs[0])(x) and predicate(pargs[1])(x)
  elif prop == "POr":
    return lambda x: predicate(pargs[0])(x) or predicate(pargs[1])(x)
  elif prop == "PNot":
    return lambda x: not predicate(pargs[0])
  elif prop == "PEven":
    return lambda x: x % 2 == 0
  elif prop == "POdd":
    return lambda x: x % 2 != 0
  elif prop == "PSmaller":
    return lambda x: quantifier(pargs[0])(lambda y: x < y)
  elif prop == "PGreater":
    return lambda x: quantifier(pargs[0])(lambda y: x > y)
  elif prop == "PBetween":
    return lambda y: quantifier(pargs[0])(lambda x: quantifier(pargs[1])(lambda z: x < y and y < z))
  elif prop == "PPrime":
    return prime

# works also for Kind:
  elif prop == "KNumber":
    return lambda x: True
  elif prop == "KProperty":
    return lambda x: predicate(pargs[0])(x) and predicate(pargs[1])(x)
  
  else:
    print("not a valid property",property)

# returns (int -> bool) -> bool
def quantifier(term):
  fun,args = term.unpack()
  if fun == "TElement":
    return lambda p: p(value(args[0]))
  elif fun == "TAll":
    return lambda p: forAll(lambda x: not predicate(args[0])(x) or p(x))
  elif fun == "TAny":
    return lambda p: forSome(lambda x: predicate(args[0])(x) and p(x))
  else:
    print("not a valid term",term)

# returns int
def value(element):
  fun,args = element.unpack()
  if fun == "EInteger":
    return int(args[0].unpack())
  elif fun == "ESum":
    return value(args[0]) + value(args[1])
  elif fun == "EProduct":
    return value(args[0]) * value(args[1])
  else:
    print("not a valid element",element)
  
def prime(n):
  if n == 1:
    r = False
  else:
    r = True
  for k in range(2,n//2+1):
    if n % k == 0: return False
  return r

def forAll(p):
  for n in allNumbers:
    if not p(n): return False
  return True
    
def forSome(p):
  for n in allNumbers:
    if p(n): return True
  return False

def enumerate(p):
  r = []
  for n in allNumbers:
    if p(n): r = r + [n]
  return r

def factorial(n):
  r = 1
  for k in range(1,n+1):
    r = k*r
  return r
    
def main():
  # read in the grammar, set up the input language
  grammar = pgf.readPGF(absmodule + ".pgf")
  lang    = grammar.languages[langname]

  # read a line of input, parse in lang, return answer
  line  = input("")
  parseresult = lang.parse(line)
  prob,tree   = parseresult.__next__()
  print(tree)
  print(answer(tree))
 
main()
