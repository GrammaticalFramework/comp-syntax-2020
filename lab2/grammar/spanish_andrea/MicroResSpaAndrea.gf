resource MicroResSpaAndrea = open Prelude in {


param
  Number = Sg | Pl ;
  Case = Nom | Acc ;
  Person = P1 | P2 | P3 ;
  Gender = Masc | Fem ;
  VForm = Inf | Pres Person Number ; 


oper
  vowels : pattern Str  = # ("a" | "e" | "i" | "o" | "u");
  tilde : pattern Str  = # ("á" | "é" | "í" | "ó" | "ú");
  consonants : pattern Str = # ( "b" | "c" | "ch" | "d" | "f" 
  | "g" | "h" | "j" | "k" | "l" | "ll" | "m" | "n" | "ñ" | "p" 
  | "q" | "r" | "s" | "t" | "v" | "w" | "x" | "y" | "z");


-- NOUNS --
Noun : Type = {s : Number => Str; g : Gender} ;

mkNoun : Str -> Str -> Noun = \sg,pl -> {
  s = table {Sg => sg ; Pl => pl} ;
  g = which_gender sg
  } ;

  
-- smart paradigm -- 
smartNoun : Str -> Noun = \s -> case s of {
  _ + ("a"|"e"|"o") => mkNoun s (s + "s") ;                    --libros
  _ + #tilde => mkNoun s (s + "s") ;                           --mamás
  _ + ("l"|"y"|"d"|"r"|"ch"|"n"|"j") => mkNoun s (s + "es") ;  --sándwiches, reyes, relojes
  x + "ón" => mkNoun s (x + "ones") ;                          --corazones
  x + "z" => mkNoun s (x + "ces") ;                            --peces
  -- _ + #consonants + #consonants => mkNoun s (s + "s") ;     --récords, icebergs
  _ + "x" => mkNoun s (s + "es") ;                             -- faxes
  _ + #vowels + "s" => mkNoun s s ;                            -- nouns sharing sg and pl form (viernes, torax)
  x + "ís" => mkNoun s (x + "ises") ;                          --anises
  x + "ás" => mkNoun s (x + "ases") ;                          --compases
  x + "és" => mkNoun s (x + "eses")                            --ingleses
  } ;


which_gender : Str -> Gender = \s -> case s of {
  x + "lor" => Fem ;   --flor
  x + "eche" => Fem ;  --leche
  x + "jer" => Fem ;   --mujer
  x + "ube" => Fem ;   --nube
  x + "gre" => Fem ;   --sangre
  x + ( "l" | "o" | "n" | "e" | "r" | "s" | "ma" | "é" | "dor" | "ez" | "bre" | "che" | "oma") => Masc ;
  x + ( "a" | "d" | "ón" | "z" | "is" | "ie" | "umbre" | "gua" | "ta" ) => Fem
  } ;


-- ADJECTIVES --
Adjective : Type = {s : Gender => Number => Str} ;

mkAdj : (AMascSg, AFemSg, AMascPl, AFemPl : Str) -> Adjective 
= \AMascSg, AFemSg, AMascPl, AFemPl -> 
{s = table { 
  Masc => table { Sg => AMascSg ; Pl => AMascPl} ; 
  Fem => table { Sg => AFemSg ; Pl => AFemPl }}
} ;


-- smart paradigm -- 
smartAdj : Str -> Adjective  = \s -> case s of {
    x + "o" => mkAdj s (x + "a") (x + "os") (x + "as");          --contento
    x + "e" => mkAdj s s (x + "es") (x + "es") ;                 --inteligente
    _ + #consonants => mkAdj s s (s + "es") (s + "es") ;         --joven, azul
    _ + "a" => mkAdj s s (s + "s") (s + "s") ;                   -- adjectives sharing sg form for both genders (el / la comunista)
    x + "és" => mkAdj s (x + "esa") (x + "eses") (x + "esas") ;  --inglés
    x + "z" => mkAdj s s (x + "ces") (x + "ces") ;               --feliz
    x + "án" => mkAdj s (x + "ana") (x + "anes") (x + "anas") ;
    x + "ín" => mkAdj s (x + "ina") (x + "ines") (x + "inas") ;
    x + "ón" => mkAdj s (x + "ona") (x + "ones") (x + "onas")
    } ;


-- VERBS --
Verb : Type = {s : VForm => Str} ;

mkVerb : (inf,P1sg,P2sg,P3sg,P1pl,P2pl,P3pl : Str) -> Verb
    = \inf,P1sg,P2sg,P3sg,P1pl,P2pl,P3pl -> {
    s = table {
      Inf => inf ;
      Pres P1 Sg => P1sg ;
      Pres P2 Sg => P2sg ;
      Pres P3 Sg => P3sg ;
      Pres P1 Pl => P1pl ;
      Pres P2 Pl => P2pl ;
      Pres P3 Pl => P3pl 
      } ;
    } ;


-- Regular verbs with predictable variations

-- There are 3 conjugations for verbs in Spanish:
-- 1st conjugation: verbs ending in -ar:
-- Eg.: comprar, matar, saltar, amar, nadar, enseñar, viajar, esperar, pasear
first_conj : Str -> Verb = \stem -> 
  mkVerb (stem + "ar") (stem + "o") (stem + "as") (stem + "a") (stem + "amos") (stem + "áis") (stem + "an") ;

-- 2nd conjugation: verbs ending in -er:
-- Eg.: romper, beber, comer, leer, correr, ver
second_conj : Str -> Verb = \stem -> 
  mkVerb (stem + "er") (stem + "o") (stem + "es") (stem + "e") (stem + "emos") (stem + "éis") (stem + "en") ;

-- 3rd conjugation: verbs ending in -ir:
-- Eg.: vivir
third_conj : Str -> Verb = \stem -> 
  mkVerb (stem + "ir") (stem + "o") (stem + "es") (stem + "e") (stem + "ímos") (stem + "ís") (stem + "en") ;


-- irregular verbs where conjugation rules can't be applied
verb_cer : Str -> Verb = \stem -> 
    mkVerb (stem + "cer") (stem + "zco") (stem + "ces") (stem + "ce") (stem + "cemos") (stem + "céis") (stem + "cen") ;

verb_ontrar : Str -> Verb = \stem -> 
    mkVerb (stem + "ontrar") (stem + "uentro") (stem + "uentras") (stem + "uentra") (stem + "ontramos") (stem + "ontráis") (stem + "uentran") ;

verb_gar : Str -> Verb = \stem -> 
    mkVerb (stem + "gar") (stem + "ego") (stem + "egas") (stem + "ega") (stem + "gamos") (stem + "gáis") (stem + "egan") ;

verb_ormir : Str -> Verb = \stem -> 
    mkVerb (stem + "ormir") (stem + "uermo") (stem + "uermes") (stem + "uerme") (stem + "ormimos") (stem + "ormís") (stem + "uermen") ;

verb_ender : Str -> Verb = \stem -> 
    mkVerb (stem + "ender") (stem + "iendo") (stem + "iendes") (stem + "iende") (stem + "endemos") (stem + "endéis") (stem + "ienden") ;

verb_enir : Str -> Verb = \stem -> 
    mkVerb (stem + "enir") (stem + "engo") (stem + "ienes") (stem + "iene") (stem + "enimos") (stem + "enís") (stem + "ienen") ;


-- smart paradigm -- 
smartVerb : Str -> Verb = \stem -> case stem of {
     stem + "cer"       => verb_cer stem ;      --conocer
     stem + "ontar"     => verb_ontrar stem ;   --encontrar
     stem + "gar"       => verb_gar stem ;      --jugar
     stem + "ormir"     => verb_ormir stem ;    --dormir
     stem + "ender"     => verb_ender stem ;    --entender
     stem + "enir"      => verb_enir stem ;     --venir
     stem + "ar"        => first_conj stem ;    --comprar
     stem + "er"        => second_conj stem ;   --romper
     stem + "ir"        => third_conj stem      --vivir
     } ;


-- two-place verb with "case" as preposition; for transitive verbs, c=[]
Verb2 : Type = Verb ** {c : Gender => Number => Str} ;

be_Verb : Verb = mkVerb "ser" "soy" "eres" "es" "somos" "sois" "son" ; ---s to be generalized


-- a very simplified verb agreement function for Micro
  agr_vform : Number -> VForm = \s -> case s of {
    Sg => Pres P3 Sg ;
    Pl => Pres P3 Pl
    } ;

}



-- THINGS TO BE IMPROVED IN THE FUTURE:
-- Adding more verbal tenses
-- Adding auxiliaries: haber, ser, estar
-- Adding both verbs "ser" and "estar", equivalent to verb "to be" ("el niño es sucio" vs. "el niño está sucio")
-- Even though the plural form of uncountable nouns exists in Spanish, it is uncommon to find them:
  -- leche - leches / música - músicas / agua - aguas / sangre - sangres
