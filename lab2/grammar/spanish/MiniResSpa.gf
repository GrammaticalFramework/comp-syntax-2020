resource MiniResSpa = open Prelude in {

param
  Number = Sg | Pl ;
  Gender = F | M ;
  -- Degree = Pos | Sup ;
  Case = Nom | Acc | Dat | Gen | Pre ; -- just for pronouns (comitative & reflexive seemed a bit too much)
  Person = P1 | P2 | P3 ;
  -- Tense = Past | Pres | Futr ;
  -- Mood = Ind | Imp; -- | Sub | Cnd 
  -- Aspect = Perf | Impf ;
  Polarity = Pos | Neg ; -- just for negative imperative

  NGAgreement = NGAgr Number Gender ; -- used e.g. for noun-adj agreement
  NPAgreement = NPAgr Number Person ; -- used e.g. for verb-subj (pron) agreement

  -- what I had done initially
  {-
  VForm = VFImp VImpForm | VFPers VPersForm ; -- a VF is either personal or impersonal
  VImpForm = VInf | VPart Tense | VGer ;
  VPersForm = VPers Mood Tense Aspect NPAgreement Polarity ;
  -}
  param VForm = VInf | VPartPast NGAgreement | VPres NPAgreement | VImp NPAgreement Polarity ;
  
  -- pronouns forms
  PronForm = PForm Case NGAgreement ; 

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

  -- noun inflection
  smartNoun : Str -> Noun = \sg -> case sg of {
    faral + "á" => mkNoun sg (faral + "aes") ;
    pe + "z" => mkNoun sg (pe + "ces") ;
    x + ("s" | "x") => mkNoun sg sg ;
    _ + ("a" | "e" | "i" | "o" | "u" | "é") => mkNoun sg (sg + "s") ;
    _ => mkNoun sg (sg + "es")
    -- disregarding irregular plurals because they are virtually nonexistent
    } ;

  getGender : Str -> Gender = \sg -> case sg of {
    -- gender is hardcoded when there is no rule to determine it
    ("animal" | "bebé" | "pan" | "coche" | "ordenador" | "pez" | "idioma" | "nombre" | "árbol" | "hombre") => M ;
    ("sangre" | "ciudad" | "nube" | "flor" | "leche" | "mar" | "nave" | "mujer") => F ; 
    x + ("esa" | "isa" | "ina" | "triz") => F ;
    (barc + "o") => M ;
    manzan + "a" => F ;
    _ => Predef.error ("gender of " ++ sg ++ " is unknown")
    } ;

  -- | ADJECTIVES
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

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb
    = \inf, partpastsgm, pressg1, pressg2, pressg3, prespl1, prespl2, prespl3, imprpossg2, imprpospl1, imprpospl2, imprnegsg2, imprnegpl1, imprnegpl2 -> let pps = (smartAdjective partpastsgm).s in {
      s = table {
        -- | Formas impersonales
        VInf => inf ;
        VPartPast (NGAgr Sg M) => partpastsgm ;
        VPartPast (NGAgr Sg F) => pps ! (NGAgr Sg F) ;
        VPartPast (NGAgr Pl M) => pps ! (NGAgr Pl M) ;
        VPartPast (NGAgr Pl F) => pps ! (NGAgr Pl F) ;
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
    
    smartVerb : Str -> Verb = \inf -> case inf of {
      -- irregulars (but not enough for me to write all forms in the lexicon)
      "romper" => romper "romp" ;
      "encontrar" => encontrar "encontr" ;
      "ver" => ver "v" ;
      "entender" => entender "entend" ;
      -- auxiliares
      "haber" => haber "hab" ;
      "ser" => ser "s" ;
      -- regulares
      cant + "ar" => conjugAr cant ;
      aprend + "er" => conjugEr aprend ;
      sacud + "ir" => conjugIr sacud
    } ;

    conjugAr : Str -> Verb = \cant -> mkVerb (cant + "ar") (cant + "ado") (cant + "o") (cant + "as") (cant + "a") (cant + "amos") (cant + "áis") (cant + "an") (cant + "a") (cant + "emos") (cant + "ad") (cant + "es") (cant + "emos") (cant + "éis") ;

    conjugEr : Str -> Verb = \aprend -> mkVerb (aprend + "er") (aprend + "ido") (aprend + "o") (aprend + "es") (aprend + "e") (aprend + "emos") (aprend + "éis") (aprend + "en") (aprend + "e") (aprend + "amos") (aprend + "ed") (aprend + "as") (aprend + "amos") (aprend + "àis") ;

    conjugIr : Str -> Verb = \sacud -> mkVerb (sacud + "ir") (sacud + "ido") (sacud + "o") (sacud + "es") (sacud + "e") (sacud + "imos") (sacud + "ìs") (sacud + "en") (sacud + "e") (sacud + "amos") (sacud + "id") (sacud + "as") (sacud + "amos") (sacud + "àis") ;

    -- irregular verbs
    romper : Str -> Verb = \romp -> 
      let orig = conjugEr "romp" 
        in orig ** { 
          s = table { 
            VPartPast _ => "roto" ; -- TODO: fix gender
            x => orig.s ! x
          }
        } ;

    encontrar : Str -> Verb = \encontr -> 
      let orig = conjugAr encontr 
      in orig ** { 
        s = table { 
          VPres (NPAgr Sg P1) => "encuentro" ;
          VPres (NPAgr Sg P2) => "encuentras" ;
          VPres (NPAgr Sg P3) => "encuentra" ;
          VPres (NPAgr Pl P3) => "encuentran" ;
          VImp (NPAgr Sg P2) Pos => "encuentra" ;
          VImp (NPAgr Sg P2) Neg => "encuentres" ;
          x => orig.s ! x
        }
      } ;

    ver : Str -> Verb = \v -> 
      let orig = conjugEr v 
      in orig ** { 
        s = table {
          VPartPast _ => "visto" ; -- TODO: fix gender
          VPres (NPAgr Sg P1) => "veo" ;
          VPres (NPAgr Pl P2) => "veis" ;
          VImp (NPAgr Pl P1) Pos => "veamos" ;
          VImp (NPAgr Sg P2) Neg => "veas" ;
          VImp (NPAgr Pl P1) Neg => "veamos" ;
          VImp (NPAgr Pl P2) Neg => "veàis" ;
          x => orig.s ! x
        }
      } ;

    entender : Str -> Verb = \entend -> 
      let orig = conjugEr entend 
      in orig ** { 
        s = table {
          VPres (NPAgr Sg P1) => "entiendo" ;
          VPres (NPAgr Sg P2) => "entiendes" ;
          VPres (NPAgr Sg P3) => "entiende" ;
          VPres (NPAgr Pl P3) => "entienden" ;
          VImp (NPAgr Sg P2) Pos => "entiende" ;
          VImp (NPAgr Sg P2) Neg => "entiendas" ;
          x => orig.s ! x
        }
      } ;

    haber : Str -> Verb = \hab -> 
      let orig = conjugEr hab 
      in orig ** { 
        s = table {
          VPres (NPAgr Sg P1) => "he" ;
          VPres (NPAgr Sg P2) => "has" ;
          VPres (NPAgr Sg P3) => "ha" ;
          VPres (NPAgr Pl P1) => "hemos" ;
          VPres (NPAgr Pl P3) => "han" ;
          VImp (NPAgr Sg P2) Pos => "he" ;
          VImp (NPAgr Pl P1) Pos => "hayamos" ;
          VImp (NPAgr Sg P2) Neg => "hayas" ;
          VImp (NPAgr Pl P1) Neg => "hayamos" ;
          VImp (NPAgr Pl P2) Neg => "hayàis" ;
          x => orig.s ! x
        }
      } ;

    ser : Str -> Verb = \s -> 
      let orig = conjugEr s 
      in orig ** { 
        s = table {
          -- indicativo presente
          VPres (NPAgr Sg P1) => "soy" ;
          VPres (NPAgr Sg P2) => "eres" ;
          VPres (NPAgr Sg P3) => "es" ;
          VPres (NPAgr Pl P1) => "somos" ;
          VPres (NPAgr Pl P2) => "sois" ;
          VPres (NPAgr Pl P3) => "son" ;
          VImp (NPAgr Sg P2) Pos => "sé" ;
          VImp (NPAgr Pl P1) Pos => "seamos" ;
          VImp (NPAgr Sg P2) Neg => "seas" ;
          VImp (NPAgr Pl P1) Neg => "seamos" ;
          VImp (NPAgr Pl P2) Neg => "seàis" ;
          x => orig.s ! x
        }
      } ;

      negation : Bool -> Str = \b -> case b of {True => [] ; False => "no"} ;

}