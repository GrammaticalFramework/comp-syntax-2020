resource MiniResSpa = open Prelude in {

param
  Number = Sg | Pl ;
  Gender = F | M ;
  -- Degree = Pos | Sup ; -- maybe another day
  Case = Nom | Acc ; -- still here for pronouns (?)
  Person = P1 | P2 | P3 ;
  Tense = Past | Pres | Futr ;
  Mood = Ind | Sub | Cnd | Imp;
  Aspect = Perf | Impf | Prog ;
  Voice = Actv | Pass ; -- not used so far because passive is periphrastic
  Polarity = Pos | Neg ; -- only for negative imperative

  Agreement = Agr Number Gender ; -- used for noun-adj agreement

  -- solamente los tiempos simples, voz activa
  VForm = VFImp VImpForm | VFPers VPersForm ;
  VImpForm = VInf | VPart Tense | VGer ;
  VPersForm = VPers Mood Tense Aspect Number Person Polarity ;

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

  mkAdjective: (_, _, _, _ : Str) -> Adjective = \fsg, fpl, msg, mpl -> {
    s = table {
      Agr Sg F => fsg ;
      Agr Pl F => fpl ;
      Agr Sg M => msg ;
      Agr Pl M => mpl 
    } 
  } ;

  smartAdjective : Str -> Adjective = \msg -> case msg of {
    larg + "o" => 
      mkAdjective (larg + "a") (larg + "as") msg (larg + "os") ;
    grand + ("e" | "l" | "r" | "z" | "n") =>
      mkAdjective msg (grand + "es") msg (grand + "es") ;
    _ => Predef.error ("unknown")
  } ;

  -- | Verbs
  Verb : Type = {s : VForm => Str} ;

  mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb
    = \inf, partpres, partpast, ger, indpressg1, indpressg2, indpressg3, indprespl1, indprespl2, indprespl3, indimpfsg1, indimpfsg2, indimpfsg3,indimpfpl1, indimpfpl2, indimpfpl3, indperfsg1, indperfsg2, indperfsg3,indperfpl1, indperfpl2, indperfpl3, indfutrsg1, indfutrsg2, indfutrsg3,indfutrpl1, indfutrpl2, indfutrpl3, subpressg1, subpressg2, subpressg3,subprespl1, subprespl2, subprespl3, subimpfsg1, subimpfsg2, subimpfsg3,subimpfpl1, subimpfpl2, subimpfpl3, subfutrsg1, subfutrsg2, subfutrsg3,subfutrpl1, subfutrpl2, subfutrpl3, imprpossg2,imprpospl1, imprpospl2, imprnegsg2, imprnegpl1, imprnegpl2, condsg1, condsg2, condsg3, condpl1, condpl2, condpl3 -> {
    s = table {
      -- | Formas impersonales
      -- infinitivo
      VFImp VInf => inf ;
      -- participios
      VFImp (VPart Pres) => partpres ;
      VFImp (VPart Past) => partpast ;
      -- gerundio
      VFImp VGer => ger ;
      -- indicativo presente
      VFPers (VPers Ind Pres Impf Sg P1 _) => indpressg1 ;
      VFPers (VPers Ind Pres Impf Sg P2 _) => indpressg2 ;
      VFPers (VPers Ind Pres Impf Sg P3 _) => indpressg3 ;
      VFPers (VPers Ind Pres Impf Pl P1 _) => indprespl1 ;
      VFPers (VPers Ind Pres Impf Pl P2 _) => indprespl2 ;
      VFPers (VPers Ind Pres Impf Pl P3 _) => indprespl3 ;
      -- indicativo pretérito imperfecto
      VFPers (VPers Ind Past Impf Sg P1 _) => indimpfsg1 ;
      VFPers (VPers Ind Past Impf Sg P2 _) => indimpfsg2 ;
      VFPers (VPers Ind Past Impf Sg P3 _) => indimpfsg3 ;
      VFPers (VPers Ind Past Impf Pl P1 _) => indimpfpl1 ;
      VFPers (VPers Ind Past Impf Pl P2 _) => indimpfpl2 ;
      VFPers (VPers Ind Past Impf Pl P3 _) => indimpfpl3 ;
      -- indicativo pretérito indefinido
      VFPers (VPers Ind Past Perf Sg P1 _) => indperfsg1 ;
      VFPers (VPers Ind Past Perf Sg P2 _) => indperfsg2 ;
      VFPers (VPers Ind Past Perf Sg P3 _) => indperfsg3 ;
      VFPers (VPers Ind Past Perf Pl P1 _) => indperfpl1 ;
      VFPers (VPers Ind Past Perf Pl P2 _) => indperfpl2 ;
      VFPers (VPers Ind Past Perf Pl P3 _) => indperfpl3 ;
      -- indicativo futuro
      VFPers (VPers Ind Futr Impf Sg P1 _) => indfutrsg1 ;
      VFPers (VPers Ind Futr Impf Sg P2 _) => indfutrsg2 ;
      VFPers (VPers Ind Futr Impf Sg P3 _) => indfutrsg3 ;
      VFPers (VPers Ind Futr Impf Pl P1 _) => indfutrpl1 ;
      VFPers (VPers Ind Futr Impf Pl P2 _) => indfutrpl2 ;
      VFPers (VPers Ind Futr Impf Pl P3 _) => indfutrpl3 ;
      -- subjuntivo presente
      VFPers (VPers Sub Pres Impf Sg P1 _) => subpressg1 ;
      VFPers (VPers Sub Pres Impf Sg P2 _) => subpressg2 ;
      VFPers (VPers Sub Pres Impf Sg P3 _) => subpressg3 ;
      VFPers (VPers Sub Pres Impf Pl P1 _) => subprespl1 ;
      VFPers (VPers Sub Pres Impf Pl P2 _) => subprespl2 ;
      VFPers (VPers Sub Pres Impf Pl P3 _) => subprespl3 ;
      -- subjuntivo pretérito imperfecto
      VFPers (VPers Sub Past Impf Sg P1 _) => subimpfsg1 ;
      VFPers (VPers Sub Past Impf Sg P2 _) => subimpfsg2 ;
      VFPers (VPers Sub Past Impf Sg P3 _) => subimpfsg3 ;
      VFPers (VPers Sub Past Impf Pl P1 _) => subimpfpl1 ;
      VFPers (VPers Sub Past Impf Pl P2 _) => subimpfpl2 ;
      VFPers (VPers Sub Past Impf Pl P3 _) => subimpfpl3 ;
      -- subjuntivo futuro
      VFPers (VPers Sub Futr Impf Sg P1 _) => subfutrsg1 ;
      VFPers (VPers Sub Futr Impf Sg P2 _) => subfutrsg2 ;
      VFPers (VPers Sub Futr Impf Sg P3 _) => subfutrsg3 ;
      VFPers (VPers Sub Futr Impf Pl P1 _) => subfutrpl1 ;
      VFPers (VPers Sub Futr Impf Pl P2 _) => subfutrpl2 ;
      VFPers (VPers Sub Futr Impf Pl P3 _) => subfutrpl3 ;
      -- imperativo positivo
      VFPers (VPers Imp Pres Impf Sg P2 Pos) => imprpossg2 ;
      VFPers (VPers Imp Pres Impf Pl P1 Pos) => imprpospl1 ;
      VFPers (VPers Imp Pres Impf Pl P2 Pos) => imprpospl2 ;
      -- imperativo negativo
      VFPers (VPers Imp Pres Impf Sg P2 Neg) => imprnegsg2 ;
      VFPers (VPers Imp Pres Impf Pl P1 Neg) => imprnegpl1 ;
      VFPers (VPers Imp Pres Impf Pl P2 Neg) => imprnegpl2 ;
      -- condicional
      VFPers (VPers Cnd Pres Impf Sg P1 _) => condsg1 ;
      VFPers (VPers Cnd Pres Impf Sg P2 _) => condsg2 ;
      VFPers (VPers Cnd Pres Impf Sg P3 _) => condsg3 ;
      VFPers (VPers Cnd Pres Impf Pl P1 _) => condpl1 ;
      VFPers (VPers Cnd Pres Impf Pl P2 _) => condpl2 ;
      VFPers (VPers Cnd Pres Impf Pl P3 _) => condpl3 ;
      _ => Predef.error ("I doubt this verb form is supposed to exist")
      }
    } ;
    
    smartVerb : Str -> Verb = \inf -> case inf of {
      cant + "ar" => conjugAr cant ;
      aprend + "er" => conjugEr aprend ;
      sacud + "ir" => conjugIr sacud
    } ;

    -- how to do this without case of?
    conjugAr : Str -> Verb = \cant -> case cant of {
      _ => mkVerb (cant + "ar") (cant + "ante") (cant + "ado") (cant + "ando") (cant + "o") (cant + "as") (cant + "a") (cant + "amos") (cant + "áis") (cant + "an") (cant + "aba") (cant + "abas") (cant + "aba") (cant + "àbamos") (cant + "abais") (cant + "aban") (cant + "é") (cant + "aste") (cant + "ò") (cant + "amos") (cant + "asteis") (cant + "aron") (cant + "aré") (cant + "aràs") (cant + "arà") (cant + "aremos") (cant + "aréis") (cant + "aràn") (cant + "e") (cant + "es") (cant + "e") (cant + "emos") (cant + "éis") (cant + "en") (cant + "ara") (cant + "aras") (cant + "ara") (cant + "àramos") (cant + "arais") (cant + "aran") (cant + "are") (cant + "ares") (cant + "are") (cant + "àremos") (cant + "areis") (cant + "aren") (cant + "a") (cant + "emos") (cant + "ad") (cant + "es") (cant + "e") (cant + "emos") (cant + "arìa") (cant + "arìas") (cant + "arìa") (cant + "arìamos") (cant + "arìais") (cant + "arìan")
    } ;

    conjugEr : Str -> Verb = \aprend -> case aprend of {
      _ => mkVerb (aprend + "er") (aprend + "iente") (aprend + "ido") (aprend + "iendo") (aprend + "o") (aprend + "es") (aprend + "e") (aprend + "emos") (aprend + "éis") (aprend + "en") (aprend + "ìa") (aprend + "ìas") (aprend + "ìa") (aprend + "ìamos") (aprend + "ìais") (aprend + "ìan") (aprend + "ì") (aprend + "iste") (aprend + "iò") (aprend + "imos") (aprend + "isteis") (aprend + "ieron") (aprend + "eré") (aprend + "eràs") (aprend + "erà") (aprend + "eremos") (aprend + "eréis") (aprend + "eràn") (aprend + "a") (aprend + "as") (aprend + "a") (aprend + "amos") (aprend + "àis") (aprend + "an") (aprend + "iera") (aprend + "ieras") (aprend + "iera") (aprend + "iéramos") (aprend + "ierais") (aprend + "ieran") (aprend + "iere") (aprend + "ieres") (aprend + "iere") (aprend + "iéremos") (aprend + "iereis") (aprend + "ieren") (aprend + "e") (aprend + "amos") (aprend + "ed") (aprend + "as") (aprend + "amos") (aprend + "an") (aprend + "erìa") (aprend + "erìas") (aprend + "erìa") (aprend + "erìamos") (aprend + "erìais") (aprend + "erìan")
    } ;

    conjugIr : Str -> Verb = \sacud -> case sacud of {
      _ => mkVerb (sacud + "ir") (sacud + "iente") (sacud + "ido") (sacud + "iendo") (sacud + "o") (sacud + "es") (sacud + "e") (sacud + "imos") (sacud + "ìs") (sacud + "en") (sacud + "ìa") (sacud + "ìas") (sacud + "ìa") (sacud + "ìamos") (sacud + "ìais") (sacud + "ìan") (sacud + "ì") (sacud + "iste") (sacud + "iò") (sacud + "imos") (sacud + "isteis") (sacud + "ieron") (sacud + "iré") (sacud + "iràs") (sacud + "irà") (sacud + "iremos") (sacud + "iréis") (sacud + "iràn") (sacud + "a") (sacud + "as") (sacud + "a") (sacud + "amos") (sacud + "àis") (sacud + "an") (sacud + "iera") (sacud + "ieras") (sacud + "iera") (sacud + "iéramos") (sacud + "ierais") (sacud + "ieran") (sacud + "iere") (sacud + "ieres") (sacud + "iere") (sacud + "iéremos") (sacud + "iereis") (sacud + "ieren") (sacud + "e") (sacud + "amos") (sacud + "id") (sacud + "as") (sacud + "amos") (sacud + "àis") (sacud + "irìa") (sacud + "irìas") (sacud + "irìa") (sacud + "irìamos") (sacud + "irìais") (sacud + "irìan")
    } ;
}