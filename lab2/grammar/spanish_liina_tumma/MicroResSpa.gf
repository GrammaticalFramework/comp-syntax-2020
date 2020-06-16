resource MicroResSpa = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc ;
  Person = P1 | P2 | P3 ;
  Gender = Masc | Fem ;

oper
  
  vowels : pattern Str  = # ("a" | "e" | "i" | "o" | "u");
  vowels_tilde : pattern Str  = # ("á" | "é" | "í" | "ó" | "ú");
  consonants : pattern Str = # ("b"|"c"|"d"|"f"|"g"|"h"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|"v"|"z");

-- NOUN ------------------------------
  Noun : Type = {s : Number => Str; g : Gender } ;
 
  mkNoun : (sg, pl : Str) -> Gender -> Noun = \sg,pl,g -> {
    s = table {Sg => sg ; Pl => pl};
    g = g
    } ;

  regNoun : Str -> Gender -> Noun = \sg,g -> case sg of {
    amig + #vowels => mkNoun sg (sg + "s") g;
    _ => mkNoun sg (sg + "es") g
    };

  -- smart paradigm
  smartNoun : Str -> Gender-> Noun = \sg,g -> case sg of {
    canci + ("ón") => mkNoun sg (canci + "ones") g;
    lu + "z" => mkNoun sg (lu + "ces") g;
    beb + #vowels_tilde => mkNoun sg (beb + "es") g;
    _                         => regNoun sg g
    } ;

-- ADJECTIVE ------------------------------
  Adjective : Type = {s : Gender => Number => Str } ;
  mkAdj : (ASgMasc,ASgFem,APlMasc,APlFem : Str) -> Adjective  = \ASgMasc,ASgFem,APlMasc,APlFem -> {s = table {
    Masc => table {
      Sg => ASgMasc;
      Pl => APlMasc
      };
    Fem => table {
      Sg => ASgFem;
      Pl => APlFem }
      }
    };

  regAdj : (ASgMasc : Str) -> Adjective = \sg ->
    mkAdj sg (sg + "a") (sg + "os") (sg + "as") ;

  smartAdj : Str -> Adjective  = \sg -> case sg of {
    x + "e" => mkAdj sg sg (x + "es") (x + "es");
    x + #consonants => mkAdj sg sg (sg + "es") (sg + "es");
    x + "o" => mkAdj sg (x + "a") (x + "os") (x + "as");
    _       => regAdj sg
    };

-- VERB ------------------------------
  Verb : Type = {s : Number => Person => Str} ;

  mkVerb : (Inf,compro,compras,compra,compramos,comprais,compran : Str) -> Verb 
    = \Inf,compro,compras,compra,compramos,comprais,compran -> {
    s = table {
      Sg => table {P1 => compro; P2 => compras; P3 => compra};
      Pl => table {P1 => compramos; P2 => comprais; P3 => compran}
      }
    } ;  

  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \stem -> case stem of {
     stem + "tender"      => verb_tender stem ;    --entender
     stem + "enir"      => verb_enir stem ;        --venir
     stem + "ontrar"      => verb_ontrar stem ;    --encontrar
     stem + "gar"      => verb_gar stem ;          --jugar
     stem + "ar"      => verb_ar stem ;            --comprar
     stem + "er"     => verb_er stem ;             --beber
     stem + "ormir"     => verb_ormir stem ;       --dormir
     stem + "ir"      => verb_ir stem              --vivir
     } ;


  verb_tender: (stem : Str) -> Verb = \stem -> {
    s = table {
      Sg => table {P1 => stem + "tiende" ; P2 => stem + "tiendes"; P3 => stem + "tiende"} ;
      Pl => table {P1 => stem + "tendemos" ; P2 => stem + "tendéis"; P3 => stem + "tienden" }

      }
    } ;

  verb_enir: (stem : Str) -> Verb = \stem -> {
    s = table {
      Sg => table {P1 => stem + "engo" ; P2 => stem + "ienes"; P3 => stem + "iene"} ;
      Pl => table {P1 => stem + "enimos" ; P2 => stem + "enís"; P3 => stem + "ienen" }

      }
    } ;

  verb_ontrar: (stem : Str) -> Verb = \stem -> {
    s = table {
      Sg => table {P1 => stem + "uentro" ; P2 => stem + "uentras"; P3 => stem + "uentra"} ;
      Pl => table {P1 => stem + "ontramos" ; P2 => stem + "ontráis"; P3 => stem + "uentran" }

      }
    } ;

  verb_gar: (stem : Str) -> Verb = \stem -> {
    s = table {
      Sg => table {P1 => stem + "ego" ; P2 => stem + "egas"; P3 => stem + "ega"} ;
      Pl => table {P1 => stem + "gamos" ; P2 => stem + "gáis"; P3 => stem + "egan" }

      }
    } ;

  verb_ar: (stem : Str) -> Verb = \stem -> {
    s = table {
      Sg => table {P1 => stem + "o" ; P2 => stem + "as"; P3 => stem + "a"} ;
      Pl => table {P1 => stem + "amos" ; P2 => stem + "áis"; P3 => stem + "an" }

      }
    } ;

  verb_er: (stem : Str) -> Verb = \stem -> {
    s = table {
      Sg => table {P1 => stem + "o" ; P2 => stem + "es"; P3 => stem + "e"} ;
      Pl => table {P1 => stem + "emos" ; P2 => stem + "éis"; P3 => stem + "en" }

      }
    } ;

  verb_ormir: (stem : Str) -> Verb = \stem -> {
    s = table {
      Sg => table {P1 => stem + "uermo" ; P2 => stem + "uermes"; P3 => stem + "uerme"} ;
      Pl => table {P1 => stem + "ormimos" ; P2 => stem + "ormís"; P3 => stem + "uermen" }

      }
    } ;

  verb_ir: (stem : Str) -> Verb = \stem -> {
    s = table {
      Sg => table {P1 => stem + "o" ; P2 => stem + "es"; P3 => stem + "e"} ;
      Pl => table {P1 => stem + "imos" ; P2 => stem + "ís"; P3 => stem + "en" }

      }
    } ;


  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Gender => Number => Str} ;

  be_Verb : Verb = mkVerb "ser" "soy" "eres" "es" "somos" "sois" "son" ; ---s to be generalized


}