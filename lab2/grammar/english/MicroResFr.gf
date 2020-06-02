resource MicroResFr = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc ;
  Gender = F | M ;
  Agreement = Agr Number ;
  Person = P1 | P2 | P3 ;

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  VForm = VInf | VPres ; 

oper
  Noun : Type = {s : Number => Str ; gen : Gender} ;

  mkNoun : Str -> Str -> Noun = \sg,pl -> {
    s = table {Sg => sg ; Pl => pl} ;
    gen = mkGen sg
    } ;

  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "s") ;

  -- smart paradigm
  smartNoun : Str -> Noun = \sg -> case sg of {
    _ + ("é"|"o"|"e"|"n"|"t"|"r"|"i"|"il"|"ol"|"el") => regNoun sg ;
    _ + ("au"|"eu"|"eau") => mkNoun sg (sg + "x") ;
    noun + "al" => mkNoun sg (noun + "aux") ;
    noun + ("z"|"s"|"x") => mkNoun sg sg ;  --defective nouns
    ("sang"|"lait") => mkNoun sg sg         --uncountable nouns
    } ;

    mkGen: Str -> Gender = \sg -> case sg of {
	  root + ("au"|"isson"|"eu"|"t"|"o"|"age"|"é"|"ire"|"al"|"i"|"g"|"in"|"at"|"nt"|"en") => M ;     -- some general rules for masculine nouns
    ("garçon"|"livre"|"arbre") => M ;                                                              -- exceptions for masculine
	  root + ("té"|"ion"|"elle"|"me"|"ère"|"eur"|"ure"|"che"|"le"|"aire"|"ue"|"er") => F ;           -- some general rules for feminine nouns
    ("maison" | "eau") => F                                                                        -- exceptions for feminine
	  } ;

  Adjective : Type = {s : Str} ;

  Verb : Type = {s : Person => Number => Str} ;

  mkVerb : (VInf,p1sg,p1pl,p2sg,p2pl,p3sg,p3pl : Str) -> Verb
    = \VInf,p1sg,p1pl,p2sg,p2pl,p3sg,p3pl -> {
    s = table {
      inf => inf ;
      P1 => table { Sg => p1sg ; Pl => p1pl } ;
      P2 => table { Sg => p2sg ; Pl => p2pl } ;
      P3 => table { Sg => p3sg ; Pl => p3pl } 
      }
    } ;
  
  -- verbs ending in -ger that need an extra "e" for the 1st Person pl

  ger_Verb : Str -> Verb = \manger ->
     let mang = init manger
     in
     mkVerb manger (mang + "e") (mang + "es") (mang + "e") (mang + "eons") (mang + "ez") (mang + "ent") ;


  er_Verb : Str -> Verb = \aimer ->
     let aim = init aimer
     in
     mkVerb aimer (aim + "e") (aim + "es") (aim + "e") (aim + "ons") (aim + "ez") (aim + "ent")  ;


  ir_Verb : Str -> Verb = \courir ->
     let cour = init courir
     in
     mkVerb courir (cour + "s") (cour + "s") (cour + "t") (cour + "ons") (cour + "ez") (cour + "ent") ;


  re_Verb : Str -> Verb = \attendre ->
     let attend = init attendre
     in
     mkVerb attendre (attend + "s") (attend + "s") (attend + "") (attend + "ons") (attend + "ez") (attend + "ent") ;


   -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
     _ + "re"	=> re_Verb ;
     _ + "ir" => ir_Verb ;
     _ + "er" => er_Verb ;
     _ + "ger" => ger_Verb
     } ;


  -- some irregular verbs e.g. acheter achète achètes
  irregVerb : (inf,p1sg,p2sg,p3sg,p1plp2pl,p3pl : Str) -> Verb =
    \inf,p1sg,p2sg,p3sg,p1plp2pl,p3pl ->
      let verb = smartVerb inf
      in mkVerb inf P1 P2 P3 ;   

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;


  be_Verb : Verb = mkVerb "suis" "es" "est" "sommes" "êtes" "sont" ; ---s to be generalized

  Determiner : Type = {s : Str ; n : Number ; gen : Gender} ;

  mkDet : Str -> Number -> Gender -> Determiner ;
  mkDet   str   num        gen     =  {s=str ; n = num ; gen = gen} ;


---s a very simplified verb agreement function for Micro
  agr2vform : Agreement -> VForm = \a -> case a of {
    Agr Sg => VPres ;
    Agr Pl => VInf
    } ;


}