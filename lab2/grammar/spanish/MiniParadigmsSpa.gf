resource MiniParadigmsSpa = open

  MiniGrammarSpa,
  MiniResSpa,
  Prelude
  
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
    = \s -> lin PN (smartNoun s) ;

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
    mkV : (_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> V  -- crazy complicated verbs
      = \inf, partpastsgm, pressg1, pressg2, pressg3, prespl1, prespl2, prespl3, imprpossg2, imprpospl1, imprpospl2, imprnegsg2, imprnegpl1, imprnegpl2 -> lin V (mkVerb inf partpastsgm pressg1 pressg2 pressg3 prespl1 prespl2 prespl3 imprpossg2 imprpospl1 imprpospl2 imprnegsg2 imprnegpl1 imprnegpl2) ;
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

  mkAdv : Str -> Bool -> Adv
    = \s,f -> lin Adv {s = s; isFinal = f} ;

  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;
}