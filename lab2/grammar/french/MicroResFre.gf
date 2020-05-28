resource MicroResFre = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc ;
  Person = Per1 | Per2 | Per3 ;
  Gender = Fem | Masc ;


  Agreement = Agr Number Gender ; ---s Person to be added ;

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
--  VForm = Inf | PresSg3 | Past | PastPart | PresPart ; 
  VForm = Inf | Sg1 | Sg2 | Sg3 | Pl1 | Pl2 | Pl3 ; -- all in present tense
--  VForm = Inf | Pres Number Person ; -- all in present tense TODO: change VForm to this?

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
    } ;

--  Adj : Type = {s : Agreement => Str} ;
--  mkAdj : (_, _, _, _ : Str) -> Adj = \mascsg, femsg, mascpl, fempl -> {
--    s = table {Agr Sg Masc => mascsg ; 
--			   Agr Sg Fem  => femsg ; 
--			   Agr Pl Masc => mascpl ; 
--			   Agr Pl Fem  => fempl}
--  } ;

Adj : Type = {s : Gender => Number => Str} ;
  mkAdj : (_, _, _, _ : Str) -> Adj = \mascsg, femsg, mascpl, fempl -> {
    s = table { Masc => table { Sg => mascsg ; Pl => mascpl} ; Fem => table { Sg => femsg ; Pl => fempl }
  }
} ;
  
  regAdj : Str -> Adj = \mascsg -> mkAdj mascsg (mascsg + "e") (mascsg + "s") (mascsg + "es") ;

  
  --smart paradigm
  smartAdj : Str -> Adj = \mascsg -> case mascsg of {
  	gran + "d"					=> regAdj mascsg ;
	--gran + "d"					=> mkAdj mascsg (mascsg + "de") (mascsg + "ds") (mascsg + "des") ;
	roug + "e"					=> mkAdj mascsg mascsg (roug + "es") (roug + "es") ;
	b + "on"					=> mkAdj mascsg (b + "onne") (b + "ons") (b + "onnes") ;
	anci + "en"					=> mkAdj mascsg (anci + "enne") (anci + "ens") (anci + "ennes") ;
	nouv + "eau"				=> mkAdj mascsg (nouv + "elle") (nouv + "eaux") (nouv + "elles") ;
	_	                        => regAdj mascsg
  } ;	
  
  irregAdj : (mascsg,femsg,mascpl,fempl : Str) -> Adj = 
    \mascsg, femsg, mascpl, fempl ->
      let adj = smartAdj mascsg
	  in mkAdj mascsg femsg mascpl fempl ;


  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,sg1,sg2,sg3,pl1, pl2, pl3 : Str) -> Verb
    = \inf,sg1,sg2,sg3,pl1, pl2, pl3 -> {
    s = table {
      Inf => inf ;
      Sg1 => sg1 ;
      Sg2 => sg2 ;
      Sg3 => sg3 ;
	  Pl1 => pl1 ;
	  Pl2 => pl2 ;
	  Pl3 => pl3 
      }
    } ;

--  regVerb : (inf : Str) -> Verb = \inf ->
  --  mkVerb inf (inf + "s") (inf + "ed") (inf + "ed") (inf + "ing") ;

  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
     saut  +  "er" =>  mkVerb inf (saut + "e") (saut + "es") (saut + "e") (saut + "ons") (saut + "ez") (saut + "ent") ;
	 cour + "ir" 	=> mkVerb inf (cour + "s") (cour + "s") (cour + "t") (cour + "ons") (cour + "ez") (cour + "ent") ;
	 comp + "rendre"	=> mkVerb inf (comp + "rends") (comp + "rends") (comp + "rend") (comp + "renons") (comp + "renez") (comp + "rennent") ;
	 attend + "re"	=> mkVerb inf (attend + "s") (attend + "s") (attend + "") (attend + "ons") (attend + "ez") (attend + "ent")
 --    pl  +  ("a"|"e"|"i"|"o"|"u") + "y" => regVerb inf ;
   --  cr  +  "y" =>  mkVerb inf (cr + "ies") (cr + "ied") (cr + "ied") (inf + "ing") ;
  --   _ => regVerb inf 
     } ;

  -- normal irregular verbs e.g. drink,drank,drunk
  irregVerb : (inf,sg1,sg2,sg3,pl1,pl2,pl3 : Str) -> Verb =
    \inf,sg1,sg2,sg3,pl1,pl2,pl3 ->
      let verb = smartVerb inf
      --in mkVerb inf (verb.s ! PresSg3) past pastpart (verb.s ! PresPart) ;   
	  in mkVerb inf sg1 sg2 sg3 pl1 pl2 pl3 ;

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

be_Verb : Verb = mkVerb "être" "suis" "es" "est" "sommes" "êtes" "sont" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
  agr2vform : Agreement -> VForm = \a -> case a of {
    Agr Sg Fem => Sg3 ;
	Agr Sg Masc => Sg3 ;
    Agr Pl Fem => Pl3 ;
	Agr Pl Masc => Pl3 
    } ;
	}