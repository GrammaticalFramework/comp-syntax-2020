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


def execute(command,win):
  "top level: execute a Command in a window (given in main)"
  fun,args = command.unpack()

  if fun == "drawCommand":
    obj = shape(args[0])
    obj.draw(win)
    return str(args[0]),obj,fun

  if fun == "removeCommand":
    key = str(args[0])
    return key,None,fun
  
  if fun == "moveCommand":
    key = str(args[0])
    return key,None,fun
    
  if fun == "removeItCommand":
    key = "it" 
    return key,None,fun
  
  if fun == "moveItCommand":
    key = "it" 
    return key,None,fun
    
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
    factor = 3
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

def main():
  "initialize with a window, process Command input line by line; optional language argument"
  win = GraphWin("GF Draw", 1000, 1000)
  shapes = {}
  latest = "undo"
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
              px = eng.parse(line)
              p,tree = px.__next__()
              key,obj,co = execute(tree,win)
              if co == "drawCommand":
                  shapes[key] = obj
                  latest = key
              elif co == "removeCommand" and key in shapes:
                  shapes[key].undraw()
                  del shapes[key]
              elif co == "moveCommand" and key in shapes:
                  shapes[key].move(randrange(-300,300,1),randrange(-300,300,1))
                  latest = key
              elif key == "it":
                  if latest in shapes:
                      if co == "removeItCommand":
                          shapes[latest].undraw()
                          del shapes[latest]
                      if co == "moveItCommand":
                          shapes[latest].move(randrange(-300,300,1),randrange(-300,300,1))
                  else:
                      print("cannot go back in history")
              else:
                  print("shape does not exist")
           ## print(shapes) ## debugging
          except pgf.ParseError:
              print("# NO PARSE", line)
  input()

main()


