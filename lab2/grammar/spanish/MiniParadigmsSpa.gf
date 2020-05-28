resource MiniParadigmsSpa = open

  MiniGrammarSpa,
  MiniResSpa
  
in {

oper
  mkN = overload {
    mkN : Str -> Noun
      = \n -> lin N (smartNoun n) ;
    -- irregular nouns, e.g. gentilhombre/gentileshombres (very rare) 
    mkN : Str -> Str -> Noun
      = \sg,pl -> lin N (mkNoun sg pl) ;
    } ;

  mkPN : Str -> PN
    = \s -> lin PN {s = s} ;

  mkA = overload {
    mkA : Str -> Adjective
      = \a -> lin A (smartAdjective a) ;
    -- not that I'm aware of the existence of very weird adjectives...
    mkA : Str -> Str -> Str -> Str -> Adjective
      = \fsg,fpl,msg,mpl -> lin A (mkAdjective fsg fpl msg mpl) ;
  } ;

  mkV = overload {
    mkV : (inf : Str) -> V -- regular or anyway manageable verbs
      = \s -> lin V (smartVerb s) ; 
    mkV : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> V  -- crazy complicated verbs
      = \inf, partpres, partpast, ger, indpressg1, indpressg2, indpressg3, indprespl1, indprespl2, indprespl3, indimpfsg13, indimpfsg2,indimpfpl1, indimpfpl2, indimpfpl3, indperfsg1, indperfsg2, indperfsg3,indperfpl1, indperfpl2, indperfpl3, indfutrsg1, indfutrsg2, indfutrsg3,indfutrpl1, indfutrpl2, indfutrpl3, subpressg13, subpressg2,subprespl1, subprespl2, subprespl3, subimpfsg13, subimpfsg2, subimpfpl1, subimpfpl2, subimpfpl3, subfutrsg13, subfutrsg2,subfutrpl1, subfutrpl2, subfutrpl3, imprpossg2,imprpospl1, imprpospl2, imprnegsg2, imprnegpl1, imprnegpl2, condsg13, condsg2, condpl1, condpl2, condpl3 -> lin V (smartVerb inf) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2 -- regular verbs with direct object, e.g. "comprar"
      = \s   -> lin V2 (smartVerb s ** {c = []}) ;
    mkV2 : Str  -> Str -> V2 -- regular verb with preposition, e.g. "soñar con"
      = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
    mkV2 : V -> V2 -- irregular verbs with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = []}) ;
    mkV2 : V -> Str -> V2 -- irregular verbs with preposition
      = \v,p -> lin V2 (v ** {c = p}) ;
    } ;

  mkVS : V -> VS
    = \v -> lin VS v ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
}