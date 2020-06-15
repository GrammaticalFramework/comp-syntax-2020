resource MicroResSwe = open Prelude in {

param
  Number = Sg | Pl ;
  Gender = Ut | Neu ;
  Defenitivness = Def | Indef ;
  Case = Nom | Acc ;

  VForm = Pres | Inf | Preteritum | Supinum  ;


--  Case = Nom | Acc ;
--  Gender = Utr | Neutr

--  Agreement = Agr Number ; ---s Person to be added

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
--  VForm = Inf | PresSg3 | Past | PastPart | PresPart ;

oper



  Noun : Type = {s : Number => Defenitivness => Str ; g : Gender } ;


 regN : (sgIndef, sgDefi, plIndef, plDefi : Str ) ->  Noun
    =\sgIndef, sgDefi, plIndef, plDefi -> {s = table { Sg    => table { Indef => sgIndef ;
                                                                               Def  => sgDefi } ;

                                                           Pl    => table { Indef => plIndef ;
                                                                            Def  => plDefi }
                                         } ;
                                                                 g = taGender sgIndef }  ;
taGender : Str -> Gender
 = \sg -> case sg of {
      x + ("el"|"a"|"t"|"r"|"än"|"d"|"ke") => Neu ;
      x + ("v"|"in"|"en"|"öd"|"od"|"åk"|"e"|"g"|"ln"|"äd") => Ut ;
      _ => Neu

} ;

smartN : Str -> Noun
 = \sgIndef -> case sgIndef of {
  x + "el"  => regN sgIndef (x + "len") (x + "lar") (x + "larna") ;
  x + ("e"|"il")       => regN sgIndef (x + "en") (x + "ar") (x + "arna")  ;
  x + ("t"|"k")    => regN sgIndef (sgIndef + "en") (sgIndef +"ar") (sgIndef + "arna")  ;
  x + ("d"|"r")       => regN sgIndef (sgIndef + "en") (sgIndef + "er") (sgIndef + "erna") ;
  x + "än"    => regN sgIndef (sgIndef + "en") (sgIndef + "ner") (sgIndef + "nerna") ;
  x + "a"    => regN sgIndef  (sgIndef + "n" ) (x + "or" ) (x + "orna") ;
  x + "v"    => regN sgIndef  (sgIndef + "et" ) sgIndef  (sgIndef + "en") ;
  x + "in"    => regN sgIndef  (sgIndef + "et" ) (sgIndef + "er")  (sgIndef + "enrna") ;
  x + "en"  => regN sgIndef  (x + "net" ) (x + "nen")  (x + "net") ;
  _  => regN sgIndef  (sgIndef + "net" ) (sgIndef + "nen")  (sgIndef + "net")
 } ;

irregN : (sgIndef, plindef : Str) ->  Noun
= \ sgIndef, plIndef -> case sgIndef of {
  x + ("ln"|"od"|"öd"|"g"|"åk"|"e")  => regN sgIndef (sgIndef + "et") plIndef (sgIndef + "en");
  x + ("lk"|"ik") =>   regN sgIndef (sgIndef + "en") plIndef (sgIndef + "arna") ;
  x + ("ok"|"ad"|"än") => regN sgIndef (sgIndef + "en") plIndef (plIndef + "na") ;
  x + "l" => regN sgIndef (sgIndef + "en") plIndef sgIndef ;
  _ => regN sgIndef (sgIndef + "en") plIndef sgIndef

} ;

  Adjective : Type = {s : Number => Gender => Defenitivness => Str } ;


regA : (sgUtIndef,sgUtDef, sgNeuIndef, sgNeuDef, plUtDef, plUtIndef, plNeuDef, plNeuIndef : Str ) ->  Adjective
   =\ sgUtDef, sgUtIndef, sgNeuDef, sgNeuIndef, plUtDef, plUtIndef, plNeuDef, plNeuIndef -> {s = table

   { Sg => table { Ut => table { Def => sgUtDef ;
                                 Indef => sgUtIndef } ;

                  Neu  => table { Def =>  sgNeuDef ;
                                  Indef => sgNeuIndef} }  ;

    Pl => table { Ut => table { Def => plUtDef ;
                                Indef => plUtIndef} ;

                  Neu => table { Def => plNeuDef ;
                                 Indef => plNeuIndef} } } } ;

smartA : Str -> Adjective
  = \ sg -> regA (sg + "a") (sg + "t") (sg + "a") sg (sg + "a") (sg + "a") (sg + "a") (sg + "a") ;


Verb : Type = {s : VForm => Str} ;


  mkVerb : (inf,pres,preteritum,supinum : Str) -> Verb
    = \inf,pres,preteritum,supinum -> {
    s = table {
      Inf => inf ;
      Pres => pres ;
      Preteritum => preteritum ;
      Supinum => supinum
      }
    } ;

  regVerb : (inf : Str) -> Verb = \inf ->
    mkVerb inf (inf + "s") (inf + "ed") (inf + "ed") ;

  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \pres -> case pres of {
     x  +  "ar"  => mkVerb (x + "a") pres (x + "ade") (x + "at") ;
     x  +  "er" =>  mkVerb (x + "a") pres (x + "te") (x + "t");
     x + "t"  =>  mkVerb (pres + "a") pres (x + "te") (x + "t");
     x + "år"  =>  mkVerb (x + "å") pres (x + "te") (x + "t");
    -- lov + "e"  => mkVerb inf (inf + "s") (lov + "ed") (lov + "ed");
    -- kis + ("s"|"sh"|"x"|"o") => mkVerb inf (inf + "es") (inf + "ed") (inf + "ed");
     _ => regVerb pres
     } ;

  -- normal irregular verbs e.g. drink,drank,drunk
  irregVerb : (pres,preteritum,supinum : Str) -> Verb =
    \pres,preteritum,supinum ->
      let verb = smartVerb pres
      in mkVerb (verb.s! Inf) pres preteritum supinum ;

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  be_Verb : Verb = mkVerb "vara" "är" "var" "varit"  ; ---s to be generalized


---s a very simplified verb agreement function for Micro
--  agr2vform : Agreement -> VForm = \a -> case a of {
  --  Agr Sg => PresSg3 ;
  --  Agr Pl => Inf
  --  } ;











 --Determiner : Type = {s : Str ; d : Defenitivness ; g : Gender ; n : Number} ;
 --mkDet : Str -> Defenitivness -> Gender -> Number -> Determiner ;
 --mkDet str, def, gen, num  = {s = str ; d = def ; g = gen ; n = num} ;


 --ettdet =  {s = table { Def => "det" ; Indef => "ett" } ; n = Sg ; g = Ut }
 --endet = {s = table { Def => "den" ; Indef => "en" } ; n = Sg ; g = Neu }
 --dedet = {s = table { Def => "de" ; Indef => "" } ; n = Pl ; g = Neu|Ut }

  --table { Indef => table { Sg => "en" ; Pl => "de"};
    --                                 Def => table {Sg => "den"; Pl => "de"} } ;
      --                Ut => table { Indef => table { Ut => "ett" ; Pl => ""};
        --                            Def => table {Sg => "det"; Pl => "de"} }}} ;



  --Verb : Type = {s : Str} ;



--  mkA : Str -> {s : Str} ;
--  mkA str = {s = str} ;

--  mkV : Str -> {s : Str} ;
--  mkV str = {s = str} ;

--  mkV2 : Str -> {s : Str} ;
--  mkV2 str = {s = str} ;

--  mkAdv : Str -> {s : Str} ;
--  mkAdv str = {s = str} ;

--  mkPrep : Str -> {s : Str} ;
--  mkPrep str = {s = str} ;

--  mkPron : Str -> {s : Str} ;
--  mkPron str = {s = str} ;

}
