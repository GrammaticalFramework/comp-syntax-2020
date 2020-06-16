resource MicroResDan = open Prelude in {

param
  Number = Sg | Pl ;
  Definiteness = Def | Indef ;
  Case = Nom | Acc ;
  Gender = Utrum | Neutrum ; -- Neut = et Utrum = en

  Agreement = Agr Number Gender ; ---s Person to be added

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  -- VForm = Inf | PresSg3 ; 

oper
   Noun = {s : Number => Definiteness => Str ; g : Gender} ;

  worstN : Str -> Str -> Str -> Str -> Gender -> Noun ;
    worstN   sgIndf sgDef  plIndf plDef  gender = {
      s = table {Sg => table {Indef => sgIndf ;
                              Def   => sgDef } ;
                 Pl => table {Indef => plIndf ;
                              Def   => plDef }
                 };
      g = gender ; 
      }  ;

    -- smart paradigm
   smartN : Str -> Gender -> Noun ;
  smartN sgIndf gender = case <gender,sgIndf> of {
     <Utrum, sky + ("a"|"i"|"o"|"u"|"y"|"æ"|"ø"|"å")> --en kopi en ko en sky en sø
       => worstN sgIndf (sky + "en") (sky + "er") (sky + "erne") Utrum ;
     <Utrum, tåg + "e" > -- en tåge
       => worstN sgIndf (sgIndf +"n") (sgIndf + "r") (sgIndf + "erne") Utrum ;
     <Utrum, ka + "t" > -- en kat
       => worstN sgIndf (sgIndf + "ten") (sgIndf + "te") (sgIndf + "tene") Utrum ;
     <Utrum, _ > -- en computer
       => worstN sgIndf (sgIndf + "en") (sgIndf + "er") (sgIndf + "erne") Utrum ;
     <Neutrum, hjert + "e"> -- et hjerte
       => worstN sgIndf (sgIndf + "t") (hjert + "er") (hjert + "erne") Neutrum ;
     <Neutrum, _ >
       => worstN sgIndf (sgIndf + "et") (sgIndf) (sgIndf + "ene") Neutrum 
    } ;

  Adjective : Type = {s : Str} ;

  Verb : Type = {s : Str} ;

  Adj = {s: Gender => Str} ;


---s a very simplified verb agreement function for Micro
  agr2vform : Agreement -> Gender = \a -> case a of {
    Agr Sg Utrum => Utrum ;
    Agr Pl Neutrum => Neutrum ;
    Agr Sg Neutrum => Neutrum ;
    Agr Pl Utrum => Utrum
    
    } ;

}