resource MiniResSpa = open Prelude in {

param
  Number = Sg | Pl ;
  Gender = F | M ;
  -- Degree = Pos | Sup ;
  Case = Nom | Acc | Dat | Gen | Prep ; -- just for pronouns (comitative & reflexive seemed a bit too much)
  Person = P1 | P2 | P3 ;
  Tense = Past | Pres | Futr ;
  Mood = Ind | Sub | Cnd | Imp;
  Aspect = Perf | Impf ;
  Voice = Actv | Pass ; -- not in use cause pass is always periphrastic
  Polarity = Pos | Neg ; -- just for negative imperative

  Agreement = Agr Number Gender ; -- used for noun-adj agreement

  -- solamente los tiempos simples
  VForm = VFImp VImpForm | VFPers VPersForm ; -- a VF is either personal or impresonal
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
    x + ("e" | "l" | "r" | "z" | "n") =>
      mkAdjective msg ((smartNoun msg).s ! Pl) msg ((smartNoun msg).s ! Pl) ;
    _ => Predef.error ("can't come up with a good idea for " ++ msg)
  } ;

  -- | VERBS
  Verb : Type = {s : VForm => Str} ;

  mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb
    = \inf, partpres, partpast, ger, indpressg1, indpressg2, indpressg3, indprespl1, indprespl2, indprespl3, indimpfsg13, indimpfsg2,indimpfpl1, indimpfpl2, indimpfpl3, indperfsg1, indperfsg2, indperfsg3,indperfpl1, indperfpl2, indperfpl3, indfutrsg1, indfutrsg2, indfutrsg3,indfutrpl1, indfutrpl2, indfutrpl3, subpressg13, subpressg2,subprespl1, subprespl2, subprespl3, subimpfsg13, subimpfsg2, subimpfpl1, subimpfpl2, subimpfpl3, subfutrsg13, subfutrsg2,subfutrpl1, subfutrpl2, subfutrpl3, imprpossg2,imprpospl1, imprpospl2, imprnegsg2, imprnegpl1, imprnegpl2, condsg13, condsg2, condpl1, condpl2, condpl3 -> {
    s = table {
      -- | Formas impersonales
      -- infinitivo
      VFImp VInf => inf ;
      -- participios
      VFImp (VPart Pres) => partpres ;
      VFImp (VPart Past) => partpast ;
      -- gerundio
      VFImp VGer => ger ;
      -- | Formas personales
      -- indicativo presente
      VFPers (VPers Ind Pres Impf Sg P1 _) => indpressg1 ;
      VFPers (VPers Ind Pres Impf Sg P2 _) => indpressg2 ;
      VFPers (VPers Ind Pres Impf Sg P3 _) => indpressg3 ;
      VFPers (VPers Ind Pres Impf Pl P1 _) => indprespl1 ;
      VFPers (VPers Ind Pres Impf Pl P2 _) => indprespl2 ;
      VFPers (VPers Ind Pres Impf Pl P3 _) => indprespl3 ;
      -- indicativo pretérito imperfecto
      VFPers (VPers Ind Past Impf Sg P1 _) => indimpfsg13 ;
      VFPers (VPers Ind Past Impf Sg P2 _) => indimpfsg2 ;
      VFPers (VPers Ind Past Impf Sg P3 _) => indimpfsg13 ;
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
      VFPers (VPers Sub Pres Impf Sg P1 _) => subpressg13 ;
      VFPers (VPers Sub Pres Impf Sg P2 _) => subpressg2 ;
      VFPers (VPers Sub Pres Impf Sg P3 _) => subpressg13 ;
      VFPers (VPers Sub Pres Impf Pl P1 _) => subprespl1 ;
      VFPers (VPers Sub Pres Impf Pl P2 _) => subprespl2 ;
      VFPers (VPers Sub Pres Impf Pl P3 _) => subprespl3 ;
      -- subjuntivo pretérito imperfecto
      VFPers (VPers Sub Past Impf Sg P1 _) => subimpfsg13 ;
      VFPers (VPers Sub Past Impf Sg P2 _) => subimpfsg2 ;
      VFPers (VPers Sub Past Impf Sg P3 _) => subimpfsg13 ;
      VFPers (VPers Sub Past Impf Pl P1 _) => subimpfpl1 ;
      VFPers (VPers Sub Past Impf Pl P2 _) => subimpfpl2 ;
      VFPers (VPers Sub Past Impf Pl P3 _) => subimpfpl3 ;
      -- subjuntivo futuro
      VFPers (VPers Sub Futr Impf Sg P1 _) => subfutrsg13 ;
      VFPers (VPers Sub Futr Impf Sg P2 _) => subfutrsg2 ;
      VFPers (VPers Sub Futr Impf Sg P3 _) => subfutrsg13 ;
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
      VFPers (VPers Cnd Pres Impf Sg P1 _) => condsg13 ;
      VFPers (VPers Cnd Pres Impf Sg P2 _) => condsg2 ;
      VFPers (VPers Cnd Pres Impf Sg P3 _) => condsg13 ;
      VFPers (VPers Cnd Pres Impf Pl P1 _) => condpl1 ;
      VFPers (VPers Cnd Pres Impf Pl P2 _) => condpl2 ;
      VFPers (VPers Cnd Pres Impf Pl P3 _) => condpl3 ;
      _ => inf --Predef.error ("I doubt this verb form is supposed to exist")
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

    conjugAr : Str -> Verb = \cant -> mkVerb (cant + "ar") (cant + "ante") (cant + "ado") (cant + "ando") (cant + "o") (cant + "as") (cant + "a") (cant + "amos") (cant + "áis") (cant + "an") (cant + "aba") (cant + "abas") (cant + "àbamos") (cant + "abais") (cant + "aban") (cant + "é") (cant + "aste") (cant + "ò") (cant + "amos") (cant + "asteis") (cant + "aron") (cant + "aré") (cant + "aràs") (cant + "arà") (cant + "aremos") (cant + "aréis") (cant + "aràn") (cant + "e") (cant + "es") (cant + "emos") (cant + "éis") (cant + "en") (cant + "ara") (cant + "aras") (cant + "àramos") (cant + "arais") (cant + "aran") (cant + "are") (cant + "ares") (cant + "àremos") (cant + "areis") (cant + "aren") (cant + "a") (cant + "emos") (cant + "ad") (cant + "es") (cant + "e") (cant + "emos") (cant + "arìa") (cant + "arìas") (cant + "arìamos") (cant + "arìais") (cant + "arìan") ;

    conjugEr : Str -> Verb = \aprend -> mkVerb (aprend + "er") (aprend + "iente") (aprend + "ido") (aprend + "iendo") (aprend + "o") (aprend + "es") (aprend + "e") (aprend + "emos") (aprend + "éis") (aprend + "en") (aprend + "ìa") (aprend + "ìas") (aprend + "ìamos") (aprend + "ìais") (aprend + "ìan") (aprend + "ì") (aprend + "iste") (aprend + "iò") (aprend + "imos") (aprend + "isteis") (aprend + "ieron") (aprend + "eré") (aprend + "eràs") (aprend + "erà") (aprend + "eremos") (aprend + "eréis") (aprend + "eràn") (aprend + "a") (aprend + "as") (aprend + "amos") (aprend + "àis") (aprend + "an") (aprend + "iera") (aprend + "ieras") (aprend + "iéramos") (aprend + "ierais") (aprend + "ieran") (aprend + "iere") (aprend + "ieres") (aprend + "iéremos") (aprend + "iereis") (aprend + "ieren") (aprend + "e") (aprend + "amos") (aprend + "ed") (aprend + "as") (aprend + "amos") (aprend + "an") (aprend + "erìa") (aprend + "erìas") (aprend + "erìamos") (aprend + "erìais") (aprend + "erìan") ;

    conjugIr : Str -> Verb = \sacud -> mkVerb (sacud + "ir") (sacud + "iente") (sacud + "ido") (sacud + "iendo") (sacud + "o") (sacud + "es") (sacud + "e") (sacud + "imos") (sacud + "ìs") (sacud + "en") (sacud + "ìa") (sacud + "ìas") (sacud + "ìamos") (sacud + "ìais") (sacud + "ìan") (sacud + "ì") (sacud + "iste") (sacud + "iò") (sacud + "imos") (sacud + "isteis") (sacud + "ieron") (sacud + "iré") (sacud + "iràs") (sacud + "irà") (sacud + "iremos") (sacud + "iréis") (sacud + "iràn") (sacud + "a") (sacud + "as") (sacud + "amos") (sacud + "àis") (sacud + "an") (sacud + "iera") (sacud + "ieras") (sacud + "iéramos") (sacud + "ierais") (sacud + "ieran") (sacud + "iere") (sacud + "ieres") (sacud + "iéremos") (sacud + "iereis") (sacud + "ieren") (sacud + "e") (sacud + "amos") (sacud + "id") (sacud + "as") (sacud + "amos") (sacud + "àis") (sacud + "irìas") (sacud + "irìa") (sacud + "irìamos") (sacud + "irìais") (sacud + "irìan") ;

    -- irregular verbs
    romper : Str -> Verb = \romp -> 
      let orig = conjugEr "romp" 
        in orig ** { 
          s = table { 
            VFImp (VPart Past) => "roto" ;
            x => orig.s ! x
          }
        } ;

    encontrar : Str -> Verb = \encontr -> 
      let orig = conjugAr encontr 
      in orig ** { 
        s = table { 
          -- indicativo presente
          VFPers (VPers Ind Pres Impf Sg P1 _) => "encuentro" ;
          VFPers (VPers Ind Pres Impf Sg P2 _) => "encuentras" ;
          VFPers (VPers Ind Pres Impf Sg P3 _) => "encuentra" ;
          VFPers (VPers Ind Pres Impf Pl P3 _) => "encuentran" ;
          -- subjuntivo presente
          VFPers (VPers Sub Pres Impf Sg P1 _) => "encuentre" ;
          VFPers (VPers Sub Pres Impf Sg P2 _) => "encuentres" ;
          VFPers (VPers Sub Pres Impf Sg P3 _) => "encuentre" ;
          VFPers (VPers Sub Pres Impf Pl P3 _) => "encuentren" ;
          -- imperativo positivo
          VFPers (VPers Imp Pres Impf Sg P2 Pos) => "encuentra" ;
          -- imperativo negativo
          VFPers (VPers Imp Pres Impf Sg P2 Neg) => "encuentres" ;
          x => orig.s ! x
        }
      } ;

    ver : Str -> Verb = \v -> 
      let orig = conjugEr v 
      in orig ** { 
        s = table {
          -- participios
          VFImp (VPart Past) => "visto" ;
          -- indicativo presente
          VFPers (VPers Ind Pres Impf Sg P1 _) => "veo" ;
          VFPers (VPers Ind Pres Impf Pl P2 _) => "veis" ;
          -- indicativo pretérito imperfecto
          VFPers (VPers Ind Past Impf Sg P1 _) => "veìa" ;
          VFPers (VPers Ind Past Impf Sg P2 _) => "veìas" ;
          VFPers (VPers Ind Past Impf Sg P3 _) => "veìa" ;
          VFPers (VPers Ind Past Impf Pl P1 _) => "veìamos" ;
          VFPers (VPers Ind Past Impf Pl P2 _) => "veìais" ;
          VFPers (VPers Ind Past Impf Pl P3 _) => "veìan" ;
          -- indicativo pretérito indefinido
          VFPers (VPers Ind Past Perf Sg P1 _) => "vi" ;
          VFPers (VPers Ind Past Perf Sg P3 _) => "vio" ;
          -- subjuntivo presente
          VFPers (VPers Sub Pres Impf Sg P1 _) => "vea" ;
          VFPers (VPers Sub Pres Impf Sg P2 _) => "veas" ;
          VFPers (VPers Sub Pres Impf Sg P3 _) => "vea" ;
          VFPers (VPers Sub Pres Impf Pl P1 _) => "veamos" ;
          VFPers (VPers Sub Pres Impf Pl P2 _) => "veàis" ;
          VFPers (VPers Sub Pres Impf Pl P3 _) => "vean" ;
          -- imperativo positivo
          VFPers (VPers Imp Pres Impf Pl P1 Pos) => "veamos" ;
          -- imperativo negativo
          VFPers (VPers Imp Pres Impf Sg P2 Neg) => "veas" ;
          VFPers (VPers Imp Pres Impf Pl P1 Neg) => "veamos" ;
          VFPers (VPers Imp Pres Impf Pl P2 Neg) => "veàis" ;
          x => orig.s ! x
        }
      } ;

    entender : Str -> Verb = \entend -> 
      let orig = conjugEr entend 
      in orig ** { 
        s = table {
          -- indicativo presente
          VFPers (VPers Ind Pres Impf Sg P1 _) => "entiendo" ;
          VFPers (VPers Ind Pres Impf Sg P2 _) => "entiendes" ;
          VFPers (VPers Ind Pres Impf Sg P3 _) => "entiende" ;
          VFPers (VPers Ind Pres Impf Pl P3 _) => "entienden" ;
          -- subjuntivo presente
          VFPers (VPers Sub Pres Impf Sg P1 _) => "entienda" ;
          VFPers (VPers Sub Pres Impf Sg P2 _) => "entiendas" ;
          VFPers (VPers Sub Pres Impf Sg P3 _) => "entienda" ;
          VFPers (VPers Sub Pres Impf Pl P3 _) => "entiendan" ;
          -- imperativo positivo
          VFPers (VPers Imp Pres Impf Sg P2 Pos) => "entiende" ;
          -- imperativo negativo
          VFPers (VPers Imp Pres Impf Sg P2 Neg) => "entiendas" ;
          x => orig.s ! x
        }
      } ;

    haber : Str -> Verb = \hab -> 
      let orig = conjugEr hab 
      in orig ** { 
        s = table {
          -- indicativo presente
          VFPers (VPers Ind Pres Impf Sg P1 _) => "he" ;
          VFPers (VPers Ind Pres Impf Sg P2 _) => "has" ;
          VFPers (VPers Ind Pres Impf Sg P3 _) => "ha" ;
          VFPers (VPers Ind Pres Impf Pl P1 _) => "hemos" ;
          VFPers (VPers Ind Pres Impf Pl P3 _) => "han" ;
          -- indicativo pretérito indefinido
          VFPers (VPers Ind Past Perf Sg P1 _) => "hube" ;
          VFPers (VPers Ind Past Perf Sg P2 _) => "hubiste" ;
          VFPers (VPers Ind Past Perf Sg P3 _) => "hubo" ;
          VFPers (VPers Ind Past Perf Pl P1 _) => "hubimos" ;
          VFPers (VPers Ind Past Perf Pl P2 _) => "hubisteis" ;
          VFPers (VPers Ind Past Perf Pl P3 _) => "hubieron" ;
          -- indicativo futuro
          VFPers (VPers Ind Futr Impf Sg P1 _) => "habré" ;
          VFPers (VPers Ind Futr Impf Sg P2 _) => "habràs" ;
          VFPers (VPers Ind Futr Impf Sg P3 _) => "habrà" ;
          VFPers (VPers Ind Futr Impf Pl P1 _) => "habremos" ;
          VFPers (VPers Ind Futr Impf Pl P2 _) => "habréis" ;
          VFPers (VPers Ind Futr Impf Pl P3 _) => "habràn" ;
          -- subjuntivo presente
          VFPers (VPers Sub Pres Impf Sg P1 _) => "haya" ;
          VFPers (VPers Sub Pres Impf Sg P2 _) => "hayas" ;
          VFPers (VPers Sub Pres Impf Sg P3 _) => "haya" ;
          VFPers (VPers Sub Pres Impf Pl P1 _) => "hayamos" ;
          VFPers (VPers Sub Pres Impf Pl P2 _) => "hayàis" ;
          VFPers (VPers Sub Pres Impf Pl P3 _) => "hayan" ;
          -- subjuntivo pretérito imperfecto
          VFPers (VPers Sub Past Impf Sg P1 _) => "hubiera" ;
          VFPers (VPers Sub Past Impf Sg P2 _) => "hubieras" ;
          VFPers (VPers Sub Past Impf Sg P3 _) => "hubiera" ;
          VFPers (VPers Sub Past Impf Pl P1 _) => "hubiéramos" ;
          VFPers (VPers Sub Past Impf Pl P2 _) => "hubiérais" ;
          VFPers (VPers Sub Past Impf Pl P3 _) => "hubieran" ;
          -- subjuntivo futuro
          VFPers (VPers Sub Futr Impf Sg P1 _) => "hubiere" ;
          VFPers (VPers Sub Futr Impf Sg P2 _) => "hubieres" ;
          VFPers (VPers Sub Futr Impf Sg P3 _) => "hubiere" ;
          VFPers (VPers Sub Futr Impf Pl P1 _) => "hubiéremos" ;
          VFPers (VPers Sub Futr Impf Pl P2 _) => "hubiéreis" ;
          VFPers (VPers Sub Futr Impf Pl P3 _) => "hubieren" ;
          -- imperativo positivo
          VFPers (VPers Imp Pres Impf Sg P2 Pos) => "he" ;
          VFPers (VPers Imp Pres Impf Pl P1 Pos) => "hayamos" ;
          -- imperativo negativo
          VFPers (VPers Imp Pres Impf Sg P2 Neg) => "hayas" ;
          VFPers (VPers Imp Pres Impf Pl P1 Neg) => "hayamos" ;
          VFPers (VPers Imp Pres Impf Pl P2 Neg) => "hayàis" ;
          -- condicional
          VFPers (VPers Cnd Pres Impf Sg P1 _) => "habrìa" ;
          VFPers (VPers Cnd Pres Impf Sg P2 _) => "habrìas" ;
          VFPers (VPers Cnd Pres Impf Sg P3 _) => "habrìa" ;
          VFPers (VPers Cnd Pres Impf Pl P1 _) => "habrìamos" ;
          VFPers (VPers Cnd Pres Impf Pl P2 _) => "habrìais" ;
          VFPers (VPers Cnd Pres Impf Pl P3 _) => "habrìan" ;
          x => orig.s ! x
        }
      } ;

    ser : Str -> Verb = \s -> 
      let orig = conjugEr s 
      in orig ** { 
        s = table {
          -- indicativo presente
          VFPers (VPers Ind Pres Impf Sg P1 _) => "soy" ;
          VFPers (VPers Ind Pres Impf Sg P2 _) => "eres" ;
          VFPers (VPers Ind Pres Impf Sg P3 _) => "es" ;
          VFPers (VPers Ind Pres Impf Pl P1 _) => "somos" ;
          VFPers (VPers Ind Pres Impf Pl P2 _) => "sois" ;
          VFPers (VPers Ind Pres Impf Pl P3 _) => "son" ;
          -- indicativo pretérito imperfecto
          VFPers (VPers Ind Past Impf Sg P1 _) => "era" ;
          VFPers (VPers Ind Past Impf Sg P2 _) => "eras" ;
          VFPers (VPers Ind Past Impf Sg P3 _) => "era" ;
          VFPers (VPers Ind Past Impf Pl P1 _) => "éramos" ;
          VFPers (VPers Ind Past Impf Pl P2 _) => "erais" ;
          VFPers (VPers Ind Past Impf Pl P3 _) => "eran" ;
          -- indicativo pretérito indefinido
          VFPers (VPers Ind Past Perf Sg P1 _) => "fui" ;
          VFPers (VPers Ind Past Perf Sg P2 _) => "fuiste" ;
          VFPers (VPers Ind Past Perf Sg P3 _) => "fue" ;
          VFPers (VPers Ind Past Perf Pl P1 _) => "fuimos" ;
          VFPers (VPers Ind Past Perf Pl P2 _) => "fuisteis" ;
          VFPers (VPers Ind Past Perf Pl P3 _) => "fueron" ;
          -- subjuntivo presente
          VFPers (VPers Sub Pres Impf Sg P1 _) => "sea" ;
          VFPers (VPers Sub Pres Impf Sg P2 _) => "seas" ;
          VFPers (VPers Sub Pres Impf Sg P3 _) => "sea" ;
          VFPers (VPers Sub Pres Impf Pl P1 _) => "seamos" ;
          VFPers (VPers Sub Pres Impf Pl P2 _) => "seàis" ;
          VFPers (VPers Sub Pres Impf Pl P3 _) => "sean" ;
          -- subjuntivo pretérito imperfecto
          VFPers (VPers Sub Past Impf Sg P1 _) => "fuera" ;
          VFPers (VPers Sub Past Impf Sg P2 _) => "fueras" ;
          VFPers (VPers Sub Past Impf Sg P3 _) => "fuera" ;
          VFPers (VPers Sub Past Impf Pl P1 _) => "fuéramos" ;
          VFPers (VPers Sub Past Impf Pl P2 _) => "fuerais" ;
          VFPers (VPers Sub Past Impf Pl P3 _) => "fueran" ;
          -- subjuntivo futuro
          VFPers (VPers Sub Futr Impf Sg P1 _) => "fuere" ;
          VFPers (VPers Sub Futr Impf Sg P2 _) => "fueres" ;
          VFPers (VPers Sub Futr Impf Sg P3 _) => "fuere" ;
          VFPers (VPers Sub Futr Impf Pl P1 _) => "fuéramos" ;
          VFPers (VPers Sub Futr Impf Pl P2 _) => "fuereis" ;
          VFPers (VPers Sub Futr Impf Pl P3 _) => "fueren" ;
          -- imperativo positivo
          VFPers (VPers Imp Pres Impf Sg P2 Pos) => "sé" ;
          VFPers (VPers Imp Pres Impf Pl P1 Pos) => "seamos" ;
          -- imperativo negativo
          VFPers (VPers Imp Pres Impf Sg P2 Neg) => "seas" ;
          VFPers (VPers Imp Pres Impf Pl P1 Neg) => "seamos" ;
          VFPers (VPers Imp Pres Impf Pl P2 Neg) => "seàis" ;
          x => orig.s ! x
        }
      } ;

}