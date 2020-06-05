resource MiniResSpa = open Prelude in {

param
  {-
  Note: things commented out testify my initial, wrong assumptions on the 
  amount of things to cover.
  -}
  Number = Sg | Pl ;
  Gender = F | M ;
  -- Degree = Pos | Sup ;
  -- just for pronouns (and there's actually more cases...)
  Case = Nom | Acc | Dat | Gen | Pre ; 
  Person = P1 | P2 | P3 ;
  -- Tense = Past | Pres | Futr ;
  -- Mood = Ind | Imp; -- | Sub | Cnd 
  -- Aspect = Perf | Impf ;
  Polarity = Pos | Neg ; -- just for negative imperative
  Definiteness = Def | Ind ;

  NGAgreement = NGAgr Number Gender ; -- used e.g. for noun-adj agreement
  NPAgreement = NPAgr Number Person ; -- used e.g. for verb-subj (pron) agreement

  {-
  VForm = VFImp VImpForm | VFPers VPersForm ; -- either personal or impersonal
  VImpForm = VInf | VPart Tense | VGer ;
  VPersForm = VPers Mood Tense Aspect NPAgreement Polarity ;
  -}
  param VForm = VInf | VPartPast | VPres NPAgreement | VImp NPAgreement Polarity ;
  
  -- pronouns forms
  PronForm = PForm Case NGAgreement ; 

oper
  -- | NOUNS
  Noun : Type = {
    s : Number => Str ; -- inflectional
    g : Gender -- inherent
    } ;

  mkNoun : Str -> Str -> Gender -> Noun = \sg,pl,gd -> {
    s = table {Sg => sg ; Pl => pl} ;
    g = gd
    } ;

  -- noun inflection
  smartNoun : Str -> Noun = \sg -> case sg of {
    faral + "á" => mkNoun sg (faral + "aes") (getGender sg) ;
    pe + "z" => mkNoun sg (pe + "ces") (getGender sg) ;
    x + ("s" | "x") => mkNoun sg sg (getGender sg) ;
    _ + ("a" | "e" | "i" | "o" | "u" | "é") => mkNoun sg (sg + "s") (getGender sg) ;
    _ => mkNoun sg (sg + "es") (getGender sg)
    -- disregarding irregular plurals because they are virtually nonexistent
    } ;

  getGender : Str -> Gender = \sg -> case sg of {
    x + ("esa" | "isa" | "ina" | "triz") => F ;
    (barc + "o") => M ;
    manzan + "a" => F ;
    _ => Predef.error("gender of " ++ sg ++ " is unknown")
    } ;

  -- | ADJECTIVES
  -- no attributive form
  Adjective : Type = {s : NGAgreement => Str} ;

  mkAdjective: (_, _, _, _ : Str) -> Adjective = \fsg, fpl, msg, mpl -> {
    s = table {
      NGAgr Sg F => fsg ;
      NGAgr Pl F => fpl ;
      NGAgr Sg M => msg ;
      NGAgr Pl M => mpl 
    } 
  } ;

  smartAdjective : Str -> Adjective = \msg -> case msg of {
    larg + "o" => 
      mkAdjective (larg + "a") (larg + "as") msg (larg + "os") ;
    x + ("e" | "l" | "r" | "z" | "n") =>
      mkAdjective msg ((smartNoun msg).s ! Pl) msg ((smartNoun msg).s ! Pl) ;
    _ => Predef.error ("can't come up with a good idea for " ++ msg)
  } ;

  -- | VERBS
  Verb : Type = {s : VForm => Str} ;

  Verb2 : Type = Verb ** {c : Str} ;

  mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb
    = \inf, partpast, pressg1, pressg2, pressg3, prespl1, prespl2, prespl3, imprpossg2, imprpospl1, imprpospl2, imprnegsg2, imprnegpl1, imprnegpl2 -> {
      s = table {
        -- | Formas impersonales
        VInf => inf ;
        VPartPast => partpast ;
        -- | Formas personales
        VPres (NPAgr Sg P1) => pressg1 ;
        VPres (NPAgr Sg P2) => pressg2 ;
        VPres (NPAgr Sg P3) => pressg3 ;
        VPres (NPAgr Pl P1) => prespl1 ;
        VPres (NPAgr Pl P2) => prespl2 ;
        VPres (NPAgr Pl P3) => prespl3 ;
        VImp (NPAgr Sg P2) Pos => imprpossg2 ;
        VImp (NPAgr Pl P1) Pos => imprpospl1 ;
        VImp (NPAgr Pl P2) Pos => imprpospl2 ;
        VImp (NPAgr Sg P2) Neg => imprnegsg2 ;
        VImp (NPAgr Pl P1) Neg => imprnegpl1 ;
        VImp (NPAgr Pl P2) Neg => imprnegpl2 ;
        _ => nonExist
      }
    } ;
    
    -- not very smart actually
    smartVerb : Str -> Verb = \inf -> case inf of {
      -- irregulares pero poco
      "romper" => let orig = conjugEr "romp" in orig ** { 
          s = table { VPartPast => "roto" ; x => orig.s ! x }
        } ;
      -- regulares
      cant + "ar" => conjugAr cant ;
      aprend + "er" => conjugEr aprend ;
      sacud + "ir" => conjugIr sacud
    } ;

    -- conjucaciones regulares
    conjugAr : Str -> Verb = \cant -> mkVerb (cant + "ar") (cant + "ado") (cant + "o") (cant + "as") (cant + "a") (cant + "amos") (cant + "áis") (cant + "an") (cant + "a") (cant + "emos") (cant + "ad") (cant + "es") (cant + "emos") (cant + "éis") ;

    conjugEr : Str -> Verb = \aprend -> mkVerb (aprend + "er") (aprend + "ido") (aprend + "o") (aprend + "es") (aprend + "e") (aprend + "emos") (aprend + "éis") (aprend + "en") (aprend + "e") (aprend + "amos") (aprend + "ed") (aprend + "as") (aprend + "amos") (aprend + "áis") ;

    conjugIr : Str -> Verb = \sacud -> mkVerb (sacud + "ir") (sacud + "ido") (sacud + "o") (sacud + "es") (sacud + "e") (sacud + "imos") (sacud + "ís") (sacud + "en") (sacud + "e") (sacud + "amos") (sacud + "id") (sacud + "as") (sacud + "amos") (sacud + "áis") ;

    -- verbos auxiliares
    haber : Verb = mkVerb "haber" "habido" "he" "has" "ha" "hemos" "habéis" "han" "he" "hayamos" "habed" "hayas" "hayamos" "hayáis" ;

    ser : Verb = mkVerb "ser" "sido" "soy" "eres" "es" "somos" "sois" "son" "sé" "seamos" "sed" "seas" "seamos" "seáis" ;

    -- | DETERMINERS (oh well actually articles)
    Determiner : Type = { 
      s : Gender => Str ;
      n : Number ;
      d : Definiteness 
    } ;

    mkDeterminer : Number -> Definiteness -> Determiner = \nm, df -> {
      s = case <nm,df> of {
        <Sg,Def> => table {
          M => "el" ;
          F => "la" -- even though that's another story for "el agua" y "el aguila" (their gender is feminine but their article is masculine)
        } ;
        <Sg,Ind> => table {
          M => "un" ;
          F => "una" 
        } ;
        <Pl,Def> => table {
          M => "los" ;
          F => "las"
        } ;
        <Pl,Ind> => table {
          M => "unos" ;
          F => "unas"
          }
        } ;  
      n = nm ;
      d = df 
    } ;

    -- | PRONOUNS
    Pronoun : Type = { 
      s : PronForm => Str ; 
      a : NPAgreement ;
      g : Gender
    } ;

    mkPronoun : NPAgreement -> Gender -> Pronoun = \npa, gnd -> {
      s = case <npa, gnd> of {
        <(NPAgr Sg P1),_> => table {
          PForm Nom _ => "yo" ; 
          PForm Acc _ => "me" ;
          PForm Dat _ => "me" ;
          PForm Gen (NGAgr Sg M) => "mío" ;
          PForm Gen (NGAgr Sg F) => "mía" ;
          PForm Gen (NGAgr Pl M) => "míos" ;
          PForm Gen (NGAgr Pl F) => "mías" ;
          PForm Pre _ => "mí"
        } ;
        <(NPAgr Sg P2),_> => table {
          PForm Nom _ => "tú" ; 
          PForm Acc _ => "te" ;
          PForm Dat _ => "te" ;
          PForm Gen (NGAgr Sg M) => "tuyo" ;
          PForm Gen (NGAgr Sg F) => "tuya" ;
          PForm Gen (NGAgr Pl M) => "tuyos" ;
          PForm Gen (NGAgr Pl F) => "tuyas" ;
          PForm Pre _ => "ti"
        } ;
        <(NPAgr Sg P3),M> => table {
          PForm Nom _ => "él" ; 
          PForm Acc _ => "lo" ;
          PForm Dat _ => "le" ;
          PForm Gen (NGAgr Sg M) => "suyo" ;
          PForm Gen (NGAgr Sg F) => "suya" ;
          PForm Gen (NGAgr Pl M) => "suyos" ;
          PForm Gen (NGAgr Pl F) => "suyas" ;
          PForm Pre _ => "se"
        } ;
        <(NPAgr Sg P3),F> => table {
          PForm Nom _ => "ella" ; 
          PForm Acc _ => "la" ;
          PForm Dat _ => "le" ;
          PForm Gen (NGAgr Sg M) => "suyo" ;
          PForm Gen (NGAgr Sg F) => "suya" ;
          PForm Gen (NGAgr Pl M) => "suyos" ;
          PForm Gen (NGAgr Pl F) => "suyas" ; 
          PForm Pre _ => "se"
        } ;
        <(NPAgr Pl P1),_> => table { -- actually missing "nosotras"
          PForm Nom _ => "nosotros" ;
          PForm Acc _ => "nos" ;
          PForm Dat _ => "nos" ;
          PForm Gen (NGAgr Sg M) => "nuestro" ;
          PForm Gen (NGAgr Sg F) => "nuestra" ;
          PForm Gen (NGAgr Pl M) => "nuestros" ;
          PForm Gen (NGAgr Pl F) => "nuestras" ;
          PForm Pre _ => "nosotros"
        } ;
        <(NPAgr Pl P2),_> => table { -- actually missing "vosotras"
          PForm Nom _ => "vosotros" ;
          PForm Acc _ => "os" ;
          PForm Dat _ => "os" ;
          PForm Gen (NGAgr Sg M) => "vuestro" ;
          PForm Gen (NGAgr Sg F) => "vuestra" ;
          PForm Gen (NGAgr Pl M) => "vuestros" ;
          PForm Gen (NGAgr Pl F) => "vuestras" ; 
          PForm Pre _ => "vosotros"
        } ;
        <(NPAgr Pl P3),_> => table { -- actually missing "ellas"
          PForm Nom _ => "ellos" ;
          PForm Acc _ => "los" ;
          PForm Dat _ => "les" ;
          PForm Gen (NGAgr Sg M) => "suyo" ;
          PForm Gen (NGAgr Sg F) => "suya" ;
          PForm Gen (NGAgr Pl M) => "suyos" ;
          PForm Gen (NGAgr Pl F) => "suyas" ;
          PForm Pre _ => "se"
        }
      } ;
      a = npa ;
      g = gnd
    } ;

    -- | More or less useful helper functions
    negation : Bool -> Str = \b -> case b of {True => [] ; False => "no"} ;

    polarity : Bool -> Polarity = \b -> case b of {
      True => Pos ; 
      False => Neg
    };

    {- 
    NOTE: apparently let (NPAgr n _) = agr is a syntax error and I found no 
    smart way to do this, one day I will read this code and laugh 
    -}
    extractNumber : NPAgreement -> Number = \npa -> case npa of {
      (NPAgr n _) => n
    } ;
}