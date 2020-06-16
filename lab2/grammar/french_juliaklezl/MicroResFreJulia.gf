resource MicroResFreJulia = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Nom_s | Acc ;
  Person = Per1 | Per2 | Per3 ;
  Gender = Fem | Masc ;
 
  VForm = Inf | VPres Number Person ; -- all in present tense

oper
  Noun : Type = {s : Number => Str ; g : Gender} ;

  mkNoun : Str -> Str -> Noun = \sg,pl -> {
    s = table {Sg => sg ; Pl => pl} ;
	g = getGender sg
    } ;

  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "s") ;

  -- smart paradigm
  smartNoun : Str -> Noun = \sg -> case sg of {
	_ + "au"				    => mkNoun sg (sg + "x") ;
	anim + "al"					=> mkNoun sg (anim + "aux") ;
	_ + ("s"|"z"|"x")			=> mkNoun sg sg ;
	"lait"|"vin"|"sang"         => mkNoun sg sg ;  -- uncountable nouns
    _	                        => regNoun sg	
    } ;
	
	getGender: Str -> Gender = \sg -> case sg of {
	("livre" | "nuage" | "homme" | "fleuve" | "navire" | "arbre") => Masc ;
	("fleur" | "maison" | "eau")								  => Fem ;
	x + ("l" | "é" | "o" | "eau" | "g" | "t" | "r" | "n" | "x")   => Masc ;
	x + ("e") 													  => Fem
	} ;


Adj : Type = {s : Gender => Number => Str ; isPre : Bool} ;
  mkAdj : (_, _, _, _ : Str) -> Bool -> Adj = \mascsg, femsg, mascpl, fempl, pos -> {
    s = table { Masc => table { Sg => mascsg ; Pl => mascpl} ; Fem => table { Sg => femsg ; Pl => fempl }
  } ; isPre = pos
} ;
  
  regAdj : Str -> Adj = \mascsg -> mkAdj mascsg (mascsg + "e") (mascsg + "s") (mascsg + "es") True ;

  
  --smart paradigm
  smartAdj : Str -> Adj = \mascsg -> case mascsg of {
  	gran + "d"					=> regAdj mascsg ;
	roug + "e"					=> mkAdj mascsg mascsg (roug + "es") (roug + "es") True ;
	mauvai + "s"				=> mkAdj mascsg (mauvai + "se") mascsg (mauvai + "ses") True;
	b + "on"					=> mkAdj mascsg (b + "onne") (b + "ons") (b + "onnes") True ;
	anci + "en"					=> mkAdj mascsg (anci + "enne") (anci + "ens") (anci + "ennes") True ;
	nouv + "eau"				=> mkAdj mascsg (nouv + "elle") (nouv + "eaux") (nouv + "elles") True ;
	_	                        => regAdj mascsg
  } ;	
  
  irregAdj : (mascsg,femsg,mascpl,fempl : Str) -> Adj =   --not very frequent
    \mascsg, femsg, mascpl, fempl ->
      let adj = smartAdj mascsg
	  in mkAdj mascsg femsg mascpl fempl True;


  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,sg1,sg2,sg3,pl1, pl2, pl3 : Str) -> Verb
    = \inf,sg1,sg2,sg3,pl1, pl2, pl3 -> {
    s = table {
      Inf => inf ;
      VPres Sg Per1 => sg1 ;
      VPres Sg Per2 => sg2 ;
      VPres Sg Per3 => sg3 ;
	  VPres Pl Per1 => pl1 ;
	  VPres Pl Per2 => pl2 ;
	  VPres Pl Per3 => pl3 
      }
    } ;


  smartVerb : Str -> Verb = \inf -> case inf of {
     saut  +  "er"  	=>  mkVerb inf (saut + "e") (saut + "es") (saut + "e") (saut + "ons") (saut + "ez") (saut + "ent") ;
	 man + "ger"		=>  mkVerb inf (man + "ge") (man + "ges") (man + "ge") (man + "geons") (man + "gez") (man + "gent") ;
	 cour + "ir" 		=> mkVerb inf (cour + "s") (cour + "s") (cour + "t") (cour + "ons") (cour + "ez") (cour + "ent") ;
	 comp + "rendre"	=> mkVerb inf (comp + "rends") (comp + "rends") (comp + "rend") (comp + "renons") (comp + "renez") (comp + "rennent") ;
	 attend + "re"		=> mkVerb inf (attend + "s") (attend + "s") (attend + "") (attend + "ons") (attend + "ez") (attend + "ent")
     } ;

  -- irregular verbs  
  irregVerb : (inf,sg1,sg2,sg3,pl1,pl2,pl3 : Str) -> Verb =
    \inf,sg1,sg2,sg3,pl1,pl2,pl3 ->
      let verb = smartVerb inf 
	  in mkVerb inf sg1 sg2 sg3 pl1 pl2 pl3 ;

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Gender => Number => Str} ;

be_Verb : Verb = mkVerb "être" "suis" "es" "est" "sommes" "êtes" "sont" ; ---s to be generalized


---s a very simplified verb agreement function for Micro  
  agr2vform : Number -> VForm = \a -> case a of {
    Sg => VPres Sg Per3 ;
    Pl => VPres Pl Per3 
    } ;
	}