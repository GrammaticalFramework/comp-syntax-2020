resource MicroResItaKonstantinos = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc ;
  Person = Per1 | Per2 | Per3 ;
  Gender = Masc | Fem ;
  


  VForm = Inf | Pres Person Number ; 

oper
  Noun : Type = {
    s : Number => Str ;
    g : Gender
    } ;

  mkNoun : Str -> Str -> Noun = \sg,pl -> {
    s = table {Sg => sg ; Pl => pl} ;
    g = getGender sg
    } ;



  -- smart paradigm
  smartNoun : Str -> Noun = \b -> case b of {    
    linguagg + "io" => mkNoun b ( linguagg + "i") ;
    uo + "mo" => mkNoun b (uo + "mini") ;
    m + "o" => mkNoun b (m + "i");
    f + "a" => mkNoun b (f + "e");
    x + "e" => mkNoun b (x + "i") ;
    "città" => mkNoun b b ;
    "computer" => mkNoun b b ; -- nouns sharing sg and pl form
    "latte" => Predef.error ("uncountable noun")  --not sure if this is going to work
    } ;

  getGender : Str -> Gender = \sg -> case sg of {
    amic + ("o" | "e") => Masc ;
    birr + ("a" | "à") => Fem ;
    ("nube" | "nave" | "febbre" | "ipertensione") => Fem ;
    "computer" => Masc 
    } ;



Adj : Type = {s : Gender => Number => Str ; isAfter : Bool} ;
  mkAdj : (_, _, _, _ : Str) -> Bool -> Adj = \ms, fs, mp, fp, pos -> {
    s = table { Masc => table { Sg => ms ; Pl => mp} ; Fem => table { Sg => fs ; Pl => fp }
  } ; isAfter = pos
} ;
  

  
  --smart paradigm
  smartAdj : Str -> Adj = \ms -> case ms of {
	p + "o"					=> mkAdj ms (p + "a") (p + "i") (p + "e") True ;
  x + "e"					=> mkAdj ms ms (x + "i") (x + "i") True ;
	bl + "u"				=> mkAdj ms ms ms ms True
  } ;	
  




  Verb : Type = {s : VForm => Str} ;
  
  
  mkVerb : (inf,Per1sg,Per2sg,Per3sg,Per1pl,Per2pl,Per3pl : Str) -> Verb
    = \inf,Per1sg,Per2sg,Per3sg,Per1pl,Per2pl,Per3pl -> {
    s = table {
      Inf => inf ;
      Pres Per1 Sg => Per1sg ;
      Pres Per2 Sg => Per2sg ;
      Pres Per3 Sg => Per3sg ;
      Pres Per1 Pl => Per1pl ;
      Pres Per2 Pl => Per2pl ;
      Pres Per3 Pl => Per3pl 
      } ;
    } ;

  -- mangiare
  conj1 : Str -> Verb = \mang -> 
    mkVerb (mang + "iare") (mang + "io") (mang + "i") (mang + "ia") (mang + "iamo") (mang + "iate") (mang + "iano") ;


  -- leggere
  conj2 : Str -> Verb = \legg ->
    mkVerb (legg + "ere")(legg + "o") (legg + "i") (legg + "e") (legg + "iamo") (legg + "ete") (legg + "ono") ;


  -- dormire
  conj3 : Str -> Verb = \dorm ->
    mkVerb (dorm + "ire")(dorm + "o") (dorm + "i") (dorm + "e") (dorm + "iamo") (dorm + "ite") (dorm + "ono") ;
 

  --mangiare, viaggiare
  conj4 : Str -> Verb = \am ->
    mkVerb (am + "are")(am + "o") (am + "i") (am + "a") (am + "iamo") (am + "ate") (am + "ano") ;
 



  -- conjugations in Italian: iare-are, ere, ire
  smartVerb : Str -> Verb = \v -> case v of {
     v + "iare" 		=> conj1 v ;
     v + "ere" 			=> conj2 v ;
     v + "ire"			=> conj3 v ;
     v + "are" => conj4 v  
     } ;



  -- normal irregular verbs e.g. drink,drank,drunk(inf,Per1sg,Per2sg,Per3sg,Per1pl,Per2pl,Per3pl  : Str) -> Verb
  irregVerb : (inf,Per1sg,Per2sg,Per3sg,Per1pl,Per2pl,Per3pl : Str) -> Verb =
    \inf,Per1sg,Per2sg,Per3sg,Per1pl,Per2pl,Per3pl ->
    mkVerb inf Per1sg Per2sg Per3sg Per1pl Per2pl Per3pl ;


  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Gender => Number => Str} ;

  be_Verb : Verb = mkVerb "essere" "sono" "sei" "è" "siamo" "siete" "sono"; ---s to be generalized
--

---s a very simplified verb agreement function for Micro
  agr2vform : Number -> VForm = \a -> case a of {
    Sg => Pres Per3 Sg ;
    Pl => Pres Per3 Pl
    } ;
}
