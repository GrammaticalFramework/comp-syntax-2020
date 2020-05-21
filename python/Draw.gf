abstract Draw = {

flags startcat = Command ;

cat
  Command ;
  Object ;
  Shape ;
  Colour ;
  Size ;

fun
  drawCommand     : Object -> Command ;
  removeCommand   : Object -> Command ;
  moveCommand     : Object -> Command ;
  removeItCommand : Command ;
  moveItCommand   : Command ;

  shapeObject : Size -> Colour -> Shape -> Object ;

  circle_Shape : Shape ;
  square_Shape : Shape ;

  big_Size   : Size ;
  small_Size : Size ;
  
  noSize : Size ;

  green_Colour  : Colour ;
  red_Colour    : Colour ;
  blue_Colour   : Colour ;
  yellow_Colour : Colour ;

  noColour : Colour ;

}