resource MiniResSwe = open Prelude in {

param
  Number  = Sg | Pl ;
  NCase   = Nom | Gen ;
  Gender  = Utr | Neutr ;
  Species = Indef | Def ;

oper
  Noun : Type = {
    s : Number => Species => NCase => Str ;
    g : Gender
    } ;

  mkNoun : (sgi,sgd,pli,pld : Str) -> Noun
    = \sgi,sgd,pli,pld -> {
      s = \\n,d,c => case <n,d,c> of {
        <Sg,Indef,Nom> => sgi ;
        <Sg,Indef,Gen> => addS sgi ;
        <Sg,Def,  Nom> => sgd ;
        <Sg,Def,  Gen> => addS sgd ;
        <Pl,Indef,Nom> => pli ;
        <Pl,Indef,Gen> => addS pli ;
        <Pl,Def,  Nom> => pld ;
        <Pl,Def,  Gen> => addS pld
	} ;
      g = getGender sgd
      } ;

  addS : Str -> Str
    = \s -> case s of {
      _ + "s" => s ;
      _ => s + "s"
      } ;
  
    
  getGender : Str -> Gender
    = \s -> case s of {
      _ + "n" => Utr ;
      _ + "t" => Neutr ;
      _ => Predef.error ("getGender" ++ s)
      } ;

  decl_1 : Str -> Noun = \s ->
    case s of {
      ap + "a" => mkNoun s (s + "n") (ap + "or") (ap + "orna") ;
      _ => Predef.error ("decl_1" ++ s)
      } ;

  decl_2 : Str -> Noun = \s ->
    case s of {
      ap + "e" => mkNoun s (s + "n") (ap + "ar") (ap + "arna") ;
      ap + "e" + c@("l"|"n"|"r") => mkNoun s (s + "n") (ap + c + "ar") (ap + c + "arna") ;
      _ => mkNoun s (s + "en") (s + "ar") (s + "arna")
      } ;
      
  decl_3 : Str -> Noun = \s ->
    case s of {
      _ + ("o" | "ö") => mkNoun s (s + "n") (s + "r") (s + "rna") ;
      _ => mkNoun s (s + "en") (s + "er") (s + "erna")
      } ;

  decl_4 : Str -> Noun = \s ->
    case s of {
      _ + "e" => mkNoun s (s + "t") (s + "n") (s + "na") ;
      _ => Predef.error ("decl_4" ++ s)
      } ;

  decl_5 : Str -> Noun = \s ->
    case s of {
      ap + "e" + c@("l"|"n"|"r") => mkNoun s (ap + c + "et") s (ap + c + "en") ;
      _ => mkNoun s (s + "et") s (s + "en")
      } ;

---- TODO (as exercise!): smart paradigms with one and two arguments


}
