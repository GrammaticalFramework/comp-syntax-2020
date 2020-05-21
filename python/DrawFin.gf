concrete DrawFin of Draw =

  open SyntaxFin, ParadigmsFin, LexiconFin
  in {

lincat
  Command = Utt ;
  Object = CN ;
  Shape = CN ;
  Colour = AP ;
  Size = AP ;

lin
  drawCommand object =
      mkUtt (mkImp (mkVP (mkV2 "piirtää") (mkNP a_Det object))) -- draw a circle
    | mkUtt (mkNP a_Det object)                                 -- a circle
    | mkUtt object                                              -- circle
    ;
  removeCommand object =
      mkUtt (mkImp (mkVP (mkV2 "poistaa") (mkNP the_Det object))) ;
  moveCommand object =
      mkUtt (mkImp (mkVP (mkV2 (mkV "siirtää") partitive) (mkNP the_Det object))) ;
  removeItCommand = mkUtt (mkImp (mkVP (mkV2 "poistaa") it_NP)) ;
  moveItCommand = mkUtt (mkImp (mkVP (mkV2 (mkV "siirtää") partitive) it_NP)) ;
      
  shapeObject size colour shape = mkCN size (mkCN colour shape) ;

  circle_Shape = mkCN (mkN "ympyrä" "ympyröitä") ;
  square_Shape = mkCN (mkN "neliö" "neliöitä") ;

  big_Size = mkAP big_A ;
  small_Size = mkAP small_A ;
  noSize = mkAP (invarA "") ; ---

  green_Colour = mkAP green_A ;
  red_Colour = mkAP red_A ;
  blue_Colour = mkAP blue_A ;
  yellow_Colour = mkAP yellow_A ;

  noColour = mkAP (invarA "") ; ---

}