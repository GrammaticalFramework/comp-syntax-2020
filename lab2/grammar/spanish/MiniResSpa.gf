resource MiniResSpa = open Prelude in {

param
  Number = Sg | Pl ;
  Gender = F | M ;
  -- Degree = Pos | Sup ; -- maybe another day
  Case = Nom | Acc ; -- still here for pronouns (?)
  Person = Per1 | Per2 | Per3 ;
  Tense = Past | Pres | Futr ;
  Mood = Ind | Sub | Cnd ;
  Aspect = Perf | Imp | Prog ;
  Voice = Actv | Pass ; -- not used so far because passive is periphrastic
  Polarity = Pos | Neg ; -- only for negative imperative

  Agreement = Agr Number Gender ; -- used for noun-adj agreement

  -- solamente los tiempos simples, voz activa
  VForm = VFImp VImpForm | VFPers VPersForm ;
  VImpForm = VInf | VPart Tense Agreement | VGer ;
  VPersForm = VPers Mood Tense Aspect Voice Number Person Polarity ;

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

  mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb
    = \inf,partpresfsg,partpresfpl,partpresmsg,partpresmpl,partpastfsg,partpastfpl,partpastmsg,partpastmpl, ger, indpressg1, indpressg2, indpressg3, indprespl1, indprespl2, indprespl3, indimpfsg1, indimpfsg2, indimpfsg3,indimpfpl1, indimpfpl2, indimpfpl3, indperfsg1, indperfsg2, indperfsg3,indperfpl1, indperfpl2, indperfpl3, indfutrsg1, indfutrsg2, indfutrsg3,indfutrpl1, indfutrpl2, indfutrpl3, subpressg1, subpressg2, subpressg3,subprespl1, subprespl2, subprespl3, subimpfsg1, subimpfsg2, subimpfsg3,subimpfpl1, subimpfpl2, subimpfpl3, subfutrsg1, subfutrsg2, subfutrsg3,subfutrpl1, subfutrpl2, subfutrpl3, imprpossg2, imprpossg3,imprpospl1, imprpospl2, imprpospl3, imprnegsg2, imprnegsg3,imprnegpl1, imprnegpl2, imprnegpl3, condsg1, condsg2, condsg3, condpl1, condpl2, condl3 -> {
    s = table {
      -- | Formas impersonales
      -- infinitivo
      VFImp VInf => inf ;
      -- participios
      VFImp (VPart Pres (Agr Sg F)) => partpresfsg ;
      VFImp (VPart Pres (Agr Pl F)) => partpresfpl ;
      VFImp (VPart Pres (Agr Sg M)) => partpresmsg ;
      VFImp (VPart Pres (Agr Pl M)) => partpresmpl ;
      VFImp (VPart Past (Agr Sg F)) => partpastfsg ;
      VFImp (VPart Past (Agr Pl F)) => partpastfpl ;
      VFImp (VPart Past (Agr Sg M)) => partpastmsg ;
      VFImp (VPart Past (Agr Pl M)) => partpastmpl ;
      -- gerundio
      VFImp VGer => ger ;
      _ => inf
      }
    } ;
}