resource MicroResFre = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc ;
  Person = Per1 | Per2 | Per3 ;
  Gender = Fem | Masc ;


  Agreement = Ag Number Gender ; ---s Person to be added

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  VForm = Inf | PresSg3 | Past | PastPart | PresPart ; 

oper
  Noun : Type = {s : Number => Str ; g : Gender} ;

  mkNoun : Str -> Str -> Noun = \sg,pl -> {
    s = table {Sg => sg ; Pl => pl} ;
	g = Fem
    } ;

  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "s") ;

  -- smart paradigm
  smartNoun : Str -> Noun = \sg -> case sg of {
	_ + "au"				    => mkNoun sg (sg + "x") ;
	anim + "al"					=> mkNoun sg (anim + "aux") ;
	_ + ("s"|"z"|"x")			=> mkNoun sg (sg) ;
    _	                        => regNoun sg	
--    _ + ("ay"|"ey"|"oy"|"uy") => regNoun sg ;
--    x + "y"                   => mkNoun sg (x + "ies") ;
--    _ + ("ch"|"sh"|"s"|"o")   => mkNoun sg (sg + "es") ;
--    _                         => regNoun sg
    } ;

  Adj : Type = {s : Gender => Number => Str};
  mkAdj : (SgMasc, SgFem, PlMasc, PlFem : Str) -> Adj = \grand, grande, grands, grandes -> {
  s = table {Masc => table {Sg => grand ; Pl => grands} ; Fem => table {Sg => grande ; Pl => grandes}}
  --  s = table {Ag Sg Masc => grand ; Ag Sg Fem => grande ; Ag Pl Masc => grands ; Ag Pl Fem => -- grandes}
  } ;
  
  regAdj : Str -> Adj = \SgMasc, SgFem, PlMasc, PlFem -> mkAdj SgMasc (SgMasc + "e") (SgMasc + "s") (SgMasc + "es") ;
  -- smart paradigm
 -- smartAdjective : Str -> Adjective = \Sg Masc -> gender -> number Sg Masc of {
  --	gran + "d"					=> regAdjective Sg Masc ;
	--_	                        => regAdjective Sg Masc 
  --} ;	

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

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  be_Verb : Verb = mkVerb "are" "is" "was" "been" "being" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
  agr2vform : Agreement -> VForm = \a -> case a of {
    Ag Sg Fem => PresSg3 ;
	Ag Sg Masc => PresSg3 ;
    Ag Pl Fem => Inf ;
	Ag Pl Masc => Inf 
    } ;
	}