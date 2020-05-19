resource MiniResSpa = open Prelude in {

param
  Number = Sg | Pl ;
  Gender = F | M ;
  -- Degree = Pos | Sup ; -- maybe another day
  Case = Nom | Acc ; -- still here for pronouns (?)
  Person = Per1 | Per2 | Per3 ;
  Tense = Pastt | Pres | Futr ; -- TODO: change to Past
  Mood = Ind | Sub | Cnd ;
  Aspect = Perf | Imp | Prog ;
  Voice = Actv | Pass ;

  Agreement = Agr Number Gender ; -- used for noun-adj agreement

  -- this will be nested
  VForm = Inf | PresSg3 | Past | PastPart | PresPart ; 

oper
  -- | NOUNS
  Noun : Type = {
    s : Number => Str ; -- inflectional
    g : Gender -- inherent
    } ;

  mkNoun : Str -> Str -> Noun = \sg,pl -> {
    s = table {Sg => sg ; Pl => pl} ;
    g = getGender sg
    } ;

  -- noun inflection (TODO: recycle for adjectives?)
  smartNoun : Str -> Noun = \sg -> case sg of {
    faral + "á" => mkNoun sg (faral + "aes") ;
    pe + "z" => mkNoun sg (pe + "ces") ;
    x + ("s" | "x") => mkNoun sg sg ;
    _ + ("a" | "e" | "i" | "o" | "u" | "é") => mkNoun sg (sg + "s") ;
    _ => mkNoun sg (sg + "es")
    } ;

  getGender : Str -> Gender = \sg -> case sg of {
    ("animal" | "bebé" | "pan" | "coche" | "ordenador" | "pez" | "idioma" | "nombre" | "árbol") => M ;
    ("sangre" | "ciudad" | "nube" | "flor" | "leche" | "mar" | "nave" | "mujer") => F ; 
    x + ("esa" | "isa" | "ina" | "triz") => F ;
    (barc + "o") => M ;
    manzan + "a" => F ;
    _ => Predef.error ("unknown")
    } ;

  -- | ADJECTIVES
  Adjective : Type = {s : Agreement => Str} ;

  mkAdjective: (fsg, fpl, msg, mpl : Str) -> Adjective {
    s = table {
      AdjectiveSg F => fsg ;
      AdjectivePl F => fpl ;
      AdjectiveSg M => msg ;
      AdjectivePl M => mpl 
    }
  } ;

  smartAdjective : Str -> Adjective = \msg -> case msg of {
    larg + "o" => 
      mkAdjective (larg + "a") (larg + "as") msg (larg + "os") ;
    grand + ("e" | "l" | "r" | "z" | "n") =>
      mkAdjective msg (grand + "es") msg (grand + "es") ;
    _ => Predef.error ("unknown")
  } ;

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,pres,past,pastpart,prespart : Str) -> Verb
    = \inf,pres,past,pastpart,prespart -> {
    s = table {
      Inf => inf ;
      PresSg3 => pres ;
      Past => past ;
      PastPart => pastpart ;
      PresPart => prespart
      }
    } ;

  regVerb : (inf : Str) -> Verb = \inf ->
    mkVerb inf (inf + "s") (inf + "ed") (inf + "ed") (inf + "ing") ;

  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
     pl  +  ("a"|"e"|"i"|"o"|"u") + "y" => regVerb inf ;
     cr  +  "y" =>  mkVerb inf (cr + "ies") (cr + "ied") (cr + "ied") (inf + "ing") ;
     lov + "e"  => mkVerb inf (inf + "s") (lov + "ed") (lov + "ed") (lov + "ing") ;
     kis + ("s"|"sh"|"x"|"o") => mkVerb inf (inf + "es") (inf + "ed") (inf + "ed") (inf + "ing") ;
     _ => regVerb inf
     } ;

  -- normal irregular verbs e.g. drink,drank,drunk
  irregVerb : (inf,past,pastpart : Str) -> Verb =
    \inf,past,pastpart ->
      let verb = smartVerb inf
      in mkVerb inf (verb.s ! PresSg3) past pastpart (verb.s ! PresPart) ;   

  negation : Bool -> Str = \b -> case b of {True => [] ; False => "not"} ; 

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  -- generalized verb, here just "be"
 param
   GVForm = VF VForm | PresSg1 | PresPl | PastPl ;

 oper
  GVerb : Type = {
     s : GVForm => Str ;
     isAux : Bool
     } ;

  be_GVerb : GVerb = {
     s = table {
       PresSg1 => "am" ;
       PresPl  => "are" ;
       PastPl  => "were" ;
       VF vf   => (mkVerb "be" "is" "was" "been" "being").s ! vf
       } ;
     isAux = True
     } ;

  -- in VP formation, all verbs are lifted to GVerb, but morphology doesn't need to know this
   verb2gverb : Verb -> GVerb = \v -> {s =
     table {
       PresSg1 => v.s ! Inf ;
       PresPl  => v.s ! Inf ;
       PastPl  => v.s ! Past ;
       VF vf   => v.s ! vf
       } ;
     isAux = False
     } ;

}