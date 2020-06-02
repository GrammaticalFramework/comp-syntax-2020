resource MicroResFra = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc | Dat ;
  Gender = F | M ;
  Person = P1 | P2 | P3 ;


oper
  Noun : Type = {s : Number => Str; g : Gender} ;

  mkNoun : (sg,pl : Str) -> Noun 
    = \sg,pl -> {
    s = table {Sg => sg ; Pl => pl} ;
    g = getGender sg
    } ;

  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "s") ;

  -- smart paradigm
  smartNoun : Str -> Noun = \sg -> case sg of {
    x + "au"       		=> mkNoun sg (x + "aux") ;
    x + "al"			=> mkNoun sg (x + "aux") ;
    _ + ("s"|"z"|"x")		=> mkNoun sg (sg) ;
    _                       	=> regNoun sg
    } ;

  getGender : Str -> Gender = \s -> case s of {
    ("eau" | "maison" | "fleur" | "femme" ) => F ;
    x + ("ière" | "ure" | "ille" | "ache" | "ie" | "aire" | "ngue" | "que" | "oile" ) => F ;
    x + ("o" | "é" | "eau" | "eur" | "on" | "re" | "ant" | "in" | "at" | "age" | "me" | "en" | "eu" | "al" | "ait" | "ire" | "ang"| "an") => M ;
    _ => Predef.error ("getGender" ++ s)
    } ;

  Adjective : Type = {s : Gender => Number => Str ; isPre : Bool } ;

  mkAdj : (ASgMasc,ASgFem,APlMasc,APlFem : Str) -> Bool -> Adjective
    = \ASgMasc,ASgFem,APlMasc,APlFem,pos -> {
    s = table {
      M => table { Sg => ASgMasc ; Pl => APlMasc } ;
      F => table { Sg => ASgFem ; Pl => APlFem }} ;
    isPre = pos ;
    } ;


  regAdj : (ASgMasc : Str) -> Adjective = \sg ->
    mkAdj sg (sg + "e") (sg + "s") (sg + "es") False ;

  smartAdj : Str -> Adjective = \sg -> case sg of {
    x + "on"			=> mkAdj sg (x + "onne") (x + "ons") (x + "onnes") False ;
    x + "e"			=> mkAdj sg (x + "e") (x + "es") (x + "es") False ;
    x + "ieux"			=> mkAdj sg (x + "ielle") (x + "ieux") (x + "ieilles") False ;
    x + "eux"			=> mkAdj sg (x + "euse") (x + "eux") (x + "euses") False ;
    x + "eau"			=> mkAdj sg (x + "elle") (x + "aux") (x + "elles") False ;
    x + "anc"			=> mkAdj sg (x + "anche") (x + "ancs") (x + "anches") False ;
    _ 				=> regAdj sg
    } ;
  

 
  Verb : Type = {s : Person => Number => Str} ;

  mkVerb : (Inf,p1sg,p1pl,p2sg,p2pl,p3sg,p3pl : Str) -> Verb
    = \Inf,p1sg,p1pl,p2sg,p2pl,p3sg,p3pl -> {
    s = table {
      P1 => table { Sg => p1sg ; Pl => p1pl } ;
      P2 => table { Sg => p2sg ; Pl => p2pl } ;
      P3 => table { Sg => p3sg ; Pl => p3pl } 
      }
    } ;


  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \stem -> case stem of {
     stem + "eter" 			=> verb4eter stem ;
     stem + "ger"			=> verb5ger stem ;
     stem + "er" 			=> verb1er stem ;
     stem + "ire"			=> verb6ire stem ;
     stem + "oir"			=> verb7oir stem ;
     stem + "mir"			=> verb9mir stem ; 
     stem + "ir"			=> verb2ir stem ; 
     stem + "vre"			=> verb8vre stem ;
     stem + "prendre"			=> verb10prendre stem ;
     stem + "re"			=> verb3re stem 
     } ;

  verb1er: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "e" ; Pl => stem + "ons" } ;
      P2 => table { Sg => stem + "es" ; Pl => stem + "ez" } ;
      P3 => table { Sg => stem + "e" ; Pl => stem + "ent" } 
      }
    } ;

  verb2ir: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "s" ; Pl => stem + "ons" } ;
      P2 => table { Sg => stem + "s" ; Pl => stem + "ez" } ;
      P3 => table { Sg => stem + "t" ; Pl => stem + "ent" } 
      }
    } ;

  verb3re: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "s" ; Pl => stem + "ons" } ;
      P2 => table { Sg => stem + "s" ; Pl => stem + "ez" } ;
      P3 => table { Sg => stem + "" ; Pl => stem + "ent" } 
      }
    } ;

  verb4eter: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "ète" ; Pl => stem + "ètons" } ;
      P2 => table { Sg => stem + "ètes" ; Pl => stem + "ètez" } ;
      P3 => table { Sg => stem + "ète" ; Pl => stem + "ètent" } 
      }
    } ;

  verb5ger: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "ge" ; Pl => stem + "geons" } ;
      P2 => table { Sg => stem + "ges" ; Pl => stem + "gez" } ;
      P3 => table { Sg => stem + "ge" ; Pl => stem + "gent" } 
      }
    } ;

  verb6ire: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "is" ; Pl => stem + "isons" } ;
      P2 => table { Sg => stem + "is" ; Pl => stem + "isez" } ;
      P3 => table { Sg => stem + "it" ; Pl => stem + "isent" } 
      }
    } ;

  verb7oir: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "ois" ; Pl => stem + "oyons" } ;
      P2 => table { Sg => stem + "ois" ; Pl => stem + "oyez" } ;
      P3 => table { Sg => stem + "oit" ; Pl => stem + "oient" } 
      }
    } ;

  verb8vre: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "s" ; Pl => stem + "vons" } ;
      P2 => table { Sg => stem + "s" ; Pl => stem + "vez" } ;
      P3 => table { Sg => stem + "t" ; Pl => stem + "vent" } 
      }
    } ;

  verb9mir: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "s" ; Pl => stem + "mons" } ;
      P2 => table { Sg => stem + "s" ; Pl => stem + "mez" } ;
      P3 => table { Sg => stem + "t" ; Pl => stem + "ment" } 
      }
    } ;

  verb10prendre: (stem : Str) -> Verb = \stem -> {
    s = table {
      P1 => table { Sg => stem + "prends" ; Pl => stem + "prenons" } ;
      P2 => table { Sg => stem + "prends" ; Pl => stem + "prenez" } ;
      P3 => table { Sg => stem + "prend" ; Pl => stem + "prennent" } 
      }
    } ;

  irregVerb : (Inf,p1sg,p1pl,p2sg,p2pl,p3sg,p3pl : Str) -> Verb 
    = \Inf,p1sg,p1pl,p2sg,p2pl,p3sg,p3pl -> {
    s = table {
      P1 => table { Sg => p1sg ; Pl => p1pl } ;
      P2 => table { Sg => p2sg ; Pl => p2pl } ;
      P3 => table { Sg => p3sg ; Pl => p3pl } 
      }
    } ;
         

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Gender => Number => Str} ;

  be_Verb : Verb = mkVerb "être" "suis" "sommes" "es" "êtes" "est" "sont" ; ---s to be generalized

}