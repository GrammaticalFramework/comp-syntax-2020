resource MicroResSwe = open Prelude in {

param
  Gender  = Utr | Neutr ;
  Number = Sg | Pl ;
  Species = Indef | Def ;
  Case = Nom | Acc ;

  AForm = ASg Gender | APl ;

  VForm = Inf | Pres ; 

oper
  Noun : Type = {
    s : Number => Species => Str ;  -- number and species are inflectional features
    g : Gender  -- gender is an inherent feature of nouns 
    } ;  -- Case is only used for pronouns, who are defined in Microlang


  mkNoun : (sgi,sgd,pli,pld : Str) -> Gender -> Noun = \sgi,sgd,pli,pld,g -> {
    s = table {
          Sg => table {Indef => sgi ; Def => sgd} ;
          Pl => table {Indef => pli ; Def => pld}
          } ;
    g = g
    } ; 

  decl_1 : Str -> Noun = \sgi ->
    case sgi of {
      ap + "a" => mkNoun sgi (sgi + "n") (ap + "or") (ap + "orna") Utr ;
      _ => Predef.error ("decl_1" ++ sgi)
      } ;

  decl_2 : Str -> Noun = \sgi ->
    case sgi of {
      pojk + "e" => mkNoun sgi (sgi + "n") (pojk + "ar") (pojk + "arna") Utr ;
      cyk + "e" + c@("l"|"n"|"r") => mkNoun sgi (sgi + "n") (cyk + c + "ar") (cyk + c + "arna") Utr ;
      _ => mkNoun sgi (sgi + "en") (sgi + "ar") (sgi + "arna") Utr
      } ;
      
  decl_3 : Str -> Noun = \sgi ->
    case sgi of {
      k + "o" => mkNoun sgi (sgi + "n") (sgi + "r") (sgi + "rna") Utr ;
      _ => mkNoun sgi (sgi + "en") (sgi + "er") (sgi + "erna") Utr
      } ;

  decl_4 : Str -> Noun = \sgi ->
    case sgi of {
      äppl + "e" => mkNoun sgi (sgi + "t") (sgi + "n") (sgi + "na") Neutr ;
      _ => Predef.error ("decl_4" ++ sgi)
      } ;

  decl_5 : Str -> Noun = \sgi ->
    case sgi of {
      _ => mkNoun sgi (sgi + "et") sgi (sgi + "en") Neutr
      } ;

  plurNoun : (sg,pl : Str) -> Noun = \sg,pl -> case pl of {
     flick + "or" => decl_1 sg ;
     pojk + "ar" => decl_2 sg ;
     flod + "er" => mkNoun sg (sg+"en") pl (pl+"na") Utr ;
     äppl + "en" => decl_4 sg ;
     _ => decl_5 sg
     } ; 

  -- smart paradigm
  smartNoun : Str -> Noun = \sg -> case sg of {
    flick + "a" => decl_1 sg ;
    äppl + "e" => decl_4 sg ;
    k + "o" => decl_3 sg ;
    _ => decl_2 sg                     
    } ;

  Adjective : Type = {s : AForm => Str} ;

  mkAdjective : (utr,neutr,pl_sgd : Str) -> Adjective 
    = \utr,neutr,pl_sgd -> {
    s = table {
      ASg Utr => utr ; 
      ASg Neutr => neutr ; 
      APl => pl_sgd}
    } ;

  smartAdjective : Str -> Adjective = \utr -> case utr of {
    svar + "t" => mkAdjective utr utr (utr + "a") ;
    fr + ("i"|"y"|"å"|"ö") => mkAdjective utr (utr + "tt") (utr + "a") ;
    rö + "d" => mkAdjective utr (rö + "tt") (utr + "a") ;
    _ => mkAdjective utr (utr + "t") (utr + "a")
    } ;
  
  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,pres: Str) -> Verb
    = \inf,pres -> {
    s = table {
      Inf => inf ;
      Pres => pres 
      }
   } ;

  smartVerb : Str -> Verb = \pres -> case pres of {
    läs + "er" => mkVerb (läs + "a") pres ;
    först + c@("ö"|"ä") + "r" => mkVerb (först + c + "ra") pres ;
    _ => mkVerb (init pres) pres 
    } ;

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  be_Verb : Verb = mkVerb "vara" "är" ;

}