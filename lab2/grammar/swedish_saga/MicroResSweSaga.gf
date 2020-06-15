resource MicroResSweSaga = open Prelude in {

param
  Number = Sg | Pl ;
  Definiteness = Def | Indef ;
  Case = Nom | Acc ;
  Gender = Utrum | Neutrum ; 
  Declension = First | Second | Third | Fourth | Fifth | Sixth | Irreg;  -- -or, -ar, -er, -r, -n, irregular e.g. man->män, mjölk->no plural exists

  AdjForm = AdjSg Gender Definiteness | AdjPl ;

  Agreement = Agr Number Gender Definiteness ; ---s Person to be added


oper

-- Nouns 
--                          INFLECTIONAL                     INHERENT
   Noun : Type = {s : Number => Definiteness => Str ; g : Gender ; dec : Declension } ;

  mkN = overload {
      mkN : Str -> Gender -> Declension -> Noun
        = smartN ;
      mkN : Str -> Str -> Str -> Str -> Gender -> Declension -> Noun 
        = worstN ;
    } ;


  worstN : Str -> Str -> Str -> Str -> Gender -> Declension -> Noun ;
    worstN   sgIndef sgDef  plIndef plDef  gender   declension = {
      s = table {Sg => table {Indef => sgIndef ;
                              Def   => sgDef } ;
                 Pl => table {Indef => plIndef ;
                              Def   => plDef }
                 };
      g = gender ; 
      dec = declension ;
      }  ;

    -- smart paradigm
   smartN : Str -> Gender -> Declension -> Noun ;
   smartN sgIndef gender declension = case <gender, declension, sgIndef> of {
     <Utrum, First, blomm + "a"> 
       => worstN sgIndef (sgIndef + "n") (blomm + "or") (blomm + "orna") Utrum First ;

     <Utrum, Second, fåg + "el" > 
       => worstN sgIndef (sgIndef + "n") (fåg + "lar") (fåg + "larna") Utrum Second ;
     <Utrum, Second, pojk + "e" > 
       => worstN sgIndef (sgIndef +"n") (pojk + "ar") (pojk + "arna") Utrum Second ;
     <Utrum, Second, _ > 
       => worstN sgIndef (sgIndef + "en") (sgIndef + "ar") (sgIndef + "arna") Utrum Second ;

     <Neutrum, Third, sgIndef> -- vin + alla ord i neutrum i 3e deklination? bryggeri, te, tyg, pris  
       => worstN sgIndef (sgIndef + "et") (sgIndef + "er") (sgIndef + "erna") Neutrum Third ;
     <Utrum, Third, dat + "or" > -- dator, motor etc
       => worstN sgIndef (sgIndef + "n") (sgIndef + "er") (sgIndef + "erna") Utrum Third ;
     <Utrum, Third, _ > -- the rest eg katt, flod
       => worstN sgIndef (sgIndef + "en") (sgIndef + "er") (sgIndef + "erna") Utrum Third ;
       
     <Utrum, Fourth, k + ("a"|"e"|"i"|"o"|"u"|"y"|"å"|"ä"|"ö")> -- ko, bakelse, radio
       => worstN sgIndef (sgIndef + "n") (sgIndef + "r") (sgIndef + "rna") Utrum Fourth ;
       
     <Neutrum, Fifth,  äppl + ("a"|"e"|"i"|"o"|"u"|"y"|"å"|"ä"|"ö")> -- märke, hjärta
     => worstN sgIndef (sgIndef + "t") (sgIndef + "n") (sgIndef + "na") Neutrum Fifth ;
       
     <Neutrum, Sixth,  djur> -- märke, hjärta,  oregelbundet??
       => worstN sgIndef (sgIndef + "et") (sgIndef) (sgIndef + "en") Neutrum Sixth ;

     <Utrum, Irreg, mjöl + "k"> -- mjölk, musik  oregelbundna och oräknebara??
       => worstN sgIndef (sgIndef + "en") (sgIndef) (sgIndef + "en") Utrum Irreg ;
     <Neutrum, Irreg, vatt + "en"> -- vatten oregelbundet och oräknebar??
       => worstN sgIndef (vatt + "net") (sgIndef) (vatt + "nen") Neutrum Irreg ;
     <Neutrum, Irreg, "blod"> -- blod  oregelbundna och oräknebara??
       => worstN sgIndef (sgIndef+ "et") (sgIndef) (sgIndef + "et") Neutrum Irreg ;
     <x, y, _>
       => worstN sgIndef (sgIndef + "??") (sgIndef + "??") (sgIndef + "??") x y
    } ;


---------- NOTE TO SELF: du har precis gjort om adjektivfunktionerna så att de 1. tar fyra argument som är utrSgIndef, pl (för pl är alltid samma), neutSgIndef, sgDef; 2. har typen {s : Gender => Number => Definiteness => Str } istället för {s : Gender => Number => Str }. Dvs du har lagt till definiteness. Nu måste du ändra i AdjCN och i UseComp så att de väljer rätt saker typ. TA DET LUGNT GUMMAN


--    Adjective : Type = {s : Gender => Number => Definiteness => Str } ;

--  mkA = overload {
--    mkA : Str -> Adjective = smartAdjective ;
--    mkA : Str -> Str -> Str -> Str -> Adjective = mkAdjective ;
--    } ;


--    mkAdjective : (utrSgIndef, pl, neutSgIndef, sgDef : Str) -> Adjective =
--      \utrSgIndef, pl, neutSgIndef, sgDef ->
--      {
--  s = table {
--    Utrum => 
--      table {
--        Sg => table {Indef => utrSgIndef ;
--                     Def => sgDef } ;
--        Pl => table {Indef => pl ; 
--                     Def => pl } 
--      } ;
--    Neutrum =>
--      table {
--        Sg => table {Indef => neutSgIndef ;
--                     Def => sgDef } ;
--        Pl => table {Indef => pl ;
--                     Def => pl }
--      }
--    }
--     } ;


    Adjective : Type = {s : AdjForm => Str } ;

  mkA = overload {
    mkA : Str -> Adjective = smartAdjective ;
    mkA : Str -> Str -> Str -> Str -> Adjective = mkAdjective ;
    } ;


    mkAdjective : (utrSgIndef, pl, neutSgIndef, sgDef : Str) -> Adjective =
      \utrSgIndef, pl, neutSgIndef, sgDef ->
      {
  s = table {
    AdjSg Utrum Indef => utrSgIndef ;
    AdjSg Utrum Def => sgDef ;
    AdjSg Neutrum Indef => neutSgIndef ;
    AdjSg Neutrum Def => sgDef ;
    AdjPl => pl
    }
     } ;



    smartAdjective : Str -> Adjective =
      \utrSg ->
      case utrSg of {
--  nyfik + "en" => mkAdjective utrSg (nyfik + "na") (nyfik + "et") (nyfik + "na") ; -- not used, but would maybe work for nyfiken, erfaren
  br + "a" => mkAdjective utrSg utrSg utrSg utrSg ; -- bra, rosa, samtida, lila, annorlunda, äkta
  n + ("y" | "å" ) => mkAdjective utrSg (utrSg + "a") (utrSg + "tt" ) (utrSg + "a") ; -- ny & blå
  rö + "d" => mkAdjective utrSg (utrSg + "a") (rö + "tt" ) (utrSg + "a") ; -- röd, död
  smar + "t" => mkAdjective utrSg (utrSg + "a") (utrSg) (utrSg + "a") ; --smart, svart
  _ => mkAdjective utrSg (utrSg + "a") (utrSg + "t") (utrSg + "a") 
      } ;
      

  Verb : Type = {s: Str} ;

--  mkV : Str -> Verb ; -- "inflection table from nounclass/gender to string"
--  mkV verb = {
--      s = table {
--            Utrum => verb ;
--            Neutrum => verb 
--          } ;
--    } ;
--    
      mkV : Str -> Verb
    = \s -> lin Verb {s = s} ;


  Verb2 : Type = Verb ** {c : Str} ;

	mkV2 = overload {
    mkV2 : Str -> Verb2          -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin Verb2 (mkV s ** {c = []}) ;
    mkV2 : Str  -> Str -> Verb2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin Verb2 (mkV s ** {c = p}) ;
    mkV2 : Verb -> Verb2            -- any verb with direct object, e.g. "drink"
      = \v   -> lin Verb2 (v ** {c = []}) ;
    mkV2 : Verb -> Str -> Verb2     -- any verb with preposition
      = \v,p -> lin Verb2 (v ** {c = p}) ;
    } ;

-- TODO kolla brittas VERB2

--  mkV2 : Str -> Verb2 ;
--  mkV2 = mkV ; 
  
  be_Verb : Verb = mkV "är";


  Adverb : Type = {s : Str} ;
  
  mkAdv : Str -> Adverb
    = \s -> lin Adverb {s = s} ;


  Preposition : Type = {s : Str} ;
  
  mkPrep : Str -> Preposition
    = \s -> lin Preposition {s = s} ;

---s a very simplified verb agreement function for Micro
--  agr2vform : Agreement -> Gender = \a -> case a of {
--    Agr Sg Utrum => Utrum ;
--    Agr Pl Neutrum => Neutrum ;
--    Agr Sg Neutrum => Neutrum ;
--    Agr Pl Utrum => Utrum
--    
--    } ;

}
