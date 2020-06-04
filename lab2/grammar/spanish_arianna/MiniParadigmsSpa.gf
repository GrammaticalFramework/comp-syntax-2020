resource MiniParadigmsSpa = open

  MiniGrammarSpa,
  MiniResSpa,
  Prelude
  
in {

oper
  mkN = overload {
     -- regular ordinary nouns with predictable gender
    mkN : Str -> Noun
      = \n -> lin N (smartNoun n) ;
    -- nouns with regular plural but unpredictable gender
    mkN : Str -> Gender -> Noun
      = \n,gn -> lin N ((smartNoun n) ** {g = gn}) ;
    -- nouns with irregular plural e.g. gentilhombre/gentileshombres (EXTREMELY rare) with unpredictable gender
    mkN : Str -> Str -> Gender -> Noun
      = \sg,pl,g -> lin N (mkNoun sg pl g) ;
    } ;

  -- | NOTE: proper names have no plural
  mkPN = overload {
    -- regular ordinary nouns with predictable gender
    mkPN : Str -> PN
      = \n -> lin PN (mkNoun n nonExist (getGender n)) ;
    -- nouns with but unpredictable gender
    mkN : Str -> Gender -> Noun
      = \n,g -> lin PN (mkNoun n nonExist g) ;
  } ;

  mkA = overload {
    mkA : Str -> Adjective
      = \a -> lin A (smartAdjective a) ;
    -- not that I'm aware of the existence of very weird adjectives...
    mkA : Str -> Str -> Str -> Str -> Adjective
      = \fsg,fpl,msg,mpl -> lin A (mkAdjective fsg fpl msg mpl) ;
  } ;

  mkV = overload {
    mkV : (inf : Str) -> V -- regular verbs
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