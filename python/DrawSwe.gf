concrete DrawSwe of Draw =

  open SyntaxSwe, ParadigmsSwe, LexiconSwe, IrregSwe, Prelude,
  (G = GrammarSwe)
  in {

lincat
  Command = Utt ;
  Object = CN ;
  Shape = CN ;
  Colour = AP ** {isAdj : Bool} ;
  Size = AP ** {isAdj : Bool} ;

lin
  drawCommand object =
      mkUtt (mkImp (mkVP (mkV2 "rita") (mkNP a_Det object))) -- draw a circle
    | mkUtt (mkNP a_Det object)                              -- a circle
    | mkUtt object                                           -- circle
    ;
  removeCommand object =
      mkUtt (mkImp (mkVP (mkV2 (mkV (mkV "ta") "bort")) (mkNP the_Det object))) ;
  undoCommand = mkUtt (mkImp (mkVP (mkV "ångra"))) ;
      
  shapeObject size colour shape =
    G.AdjCN size (G.AdjCN colour shape ** {isMod = colour.isAdj})
      ** {isMod = orB size.isAdj colour.isAdj} ;
 

  circle_Shape = mkCN (mkN "cirkel" "cirkeln" "cirklar" "cirklarna") ;
  square_Shape = mkCN (mkN "kvadrat" "kvadrater") ;

  big_Size = mkrAP big_A ;
  small_Size = mkrAP small_A ;
  noSize = emptyAP ;

  green_Colour = mkrAP green_A ;
  red_Colour = mkrAP red_A ;
  blue_Colour = mkrAP blue_A ;
  yellow_Colour = mkrAP yellow_A ;

  noColour = emptyAP ;

oper
  emptyAP : AP ** {isAdj : Bool} = mkAP (invarA "") ** {isAdj = False} ; ---
  mkrAP : A -> AP ** {isAdj : Bool} = \a -> mkAP a ** {isAdj = True} ;

}

