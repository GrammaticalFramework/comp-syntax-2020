# drawing figures with natural language commands
# presupposes 'gf -make DrawEng.gf (DrawSwe.gf ...)'

import pgf
from graphics import *
import sys
from random import randrange

# we assume this abstract syntax
absmodule = "Draw"

# change this to change input language
langname = absmodule + "Eng"

# the size of canvas
hsize, vsize = 1000,1000

def execute(command,win):
  "top level: execute a Command in a window (given in main)"
  fun,args = command.unpack()

  if fun == "drawCommand":
    obj = shape(args[0])
    obj.draw(win)
    return str(args[0]),obj,fun

  if fun == "removeCommand":
    key = reference(args[0])
    return key,None,fun
  
  if fun == "moveCommand":
    key = reference(args[0])
    return key,direction(args[1]),fun

def reference(objectref):
  fun,args = objectref.unpack()
  if fun == "theObjectRef":
    return(str(args[0]))
  else:
    return(str(objectref))

def shape(obj):
  "draw a Shape of random size and position, possibly with colour and rough size specified"
  x1 = 10 + randrange(0,500,1)
  y1 = 10 + randrange(0,500,1)
  r  = 100 + randrange(0,200,1)
  x2 = 410 + randrange(0,500,1)
  y2 = 410 + randrange(0,500,1)
  
  fun,args = obj.unpack()

  sz,xx = args[0].unpack()
  if sz == "big_Size":
    factor = 2
  elif sz == "small_Size":
    factor = 0.2
  else:
    factor = 1
  x1 = 100 + randrange(0,500,1)
  y1 = 100 + randrange(0,500,1)
  r  = factor * (100 + randrange(100,200,1))
  d  = factor * (100 + randrange(100,200,1))
  x2 = x1 + d
  y2 = y1 + d
    
  sh,xx = args[2].unpack()
  if sh == "circle_Shape":
    shap = Circle(Point(x1,y1),r)
  elif sh == "square_Shape":
    shap = Rectangle(Point(x1,y1),Point(x2,y2))
  else:
    shap = None

  co,xx = args[1].unpack()
  if co == "green_Colour":
    shap.setFill('green')
  elif co == "red_Colour":
    shap.setFill('red')
  elif co == "blue_Colour":
    shap.setFill('blue')
  elif co == "yellow_Colour":
    shap.setFill('yellow')
  else: pass
    
  return shap

def direction(place):
  fun,args = place.unpack()
  if fun == "upPlace":
    return 0,-200
  if fun == "downPlace":
    return 0,200
  if fun == "leftPlace":
    return -200,0
  if fun == "rightPlace":
    return 200,0
  else:
    return randrange(-300,300,1),randrange(-300,300,1)

def addRef(ref,obj,refs):
  "adds a reference and object to the top of the stack"
  return [(ref,obj)] + refs

def lookupRef(ref,refs):
  "return the first value and index of ref in a list of pairs refs"
  i = 0
  for ro in refs:
      r,obj = ro
      if matchRef(ref,r):
        return r,obj,i
      else:
        i = i + 1
  return None,None,None

def removeRef(ref,refs):
  "remove the first pair matching ref in refs"
  r,obj,i = lookupRef(ref,refs)
  refs.pop(i)
  return refs

def matchRef(ref,r):
    if ref == "itObjectRef":
        return True
    else:
        refws = ref.split()
        rws = r.split()
        m = True
        for i in range(1,4):
          if (refws[i][0:2] != "no") and (refws[i] != rws[i]):
              return False
        return m

def main():
  "initialize with a window, process Command input line by line; optional language argument"
  win = GraphWin("GF Draw", hsize, vsize)
  refs = []
  latest = "it"
  gr  = pgf.readPGF(absmodule + ".pgf")
  lang = langname
  if len(sys.argv) > 1:
    lang = absmodule + sys.argv[1]
  eng = gr.languages[lang]
  while True:
    try:
        line = input("")
    except EOFError:
        break
    if not(line):
        pass
    else:
          try:
              px = eng.parse(line.lower())
              p,tree = px.__next__()
              key,obj,co = execute(tree,win)
              if co == "drawCommand":
                  refs = addRef(key,obj,refs)
              elif co == "removeCommand":
                  ref,sh,i = lookupRef(key,refs)
                  if sh is None:
                    print("no such object")
                  else:
                    sh.undraw()
                    refs = removeRef(key,refs)
              elif co == "moveCommand":
                  x,y = obj
                  ref,sh,i = lookupRef(key,refs)
                  if sh is None:
                    print("no such object")
                  else:
                    sh.move(x,y)
                    refs = [(ref,sh)] + refs[:i] + refs[i+1:]
              else:
                  print("nothing else can be done")
              print(refs) ## debugging
          except pgf.ParseError:
              print("# NO PARSE", line)

main()


