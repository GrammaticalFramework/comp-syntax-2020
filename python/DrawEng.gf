concrete DrawEng of Draw =

  open SyntaxEng, ParadigmsEng, LexiconEng, IrregEng
  in {

lincat
  Command = Utt ;
  Object = CN ;
  Shape = CN ;
  Colour = AP ;
  Size = AP ;

lin
  drawCommand object =
      mkUtt (mkImp (mkVP (mkV2 draw_V) (mkNP a_Det object))) -- draw a circle
    | mkUtt (mkNP a_Det object)                              -- a circle
    | mkUtt object                                           -- circle
    ;
  removeCommand object =
      mkUtt (mkImp (mkVP (mkV2 "remove") (mkNP the_Det object))) ;
  undoCommand = mkUtt (mkImp (mkVP (mkV "undo"))) ;
      
  shapeObject size colour shape = mkCN size (mkCN colour shape) ;

  circle_Shape = mkCN (mkN "circle") ;
  square_Shape = mkCN (mkN "square") ;

  big_Size = mkAP big_A ;
  small_Size = mkAP small_A ;
  noSize = mkAP (mkA "") ; ---

  green_Colour = mkAP green_A ;
  red_Colour = mkAP red_A ;
  blue_Colour = mkAP blue_A ;
  yellow_Colour = mkAP yellow_A ;

  noColour = mkAP (mkA "") ; ---

}