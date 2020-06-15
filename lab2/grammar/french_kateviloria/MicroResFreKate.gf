resource MicroResFreKate = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc ;
  Gender = Masc | Fem ;
  --Agreement = Agr Number ; ---s Person to be added
  Person = FirstPer | SecondPer | ThirdPer ;

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  VForm = Inf | VPres Number Person ; 

oper
  --Determiner : Type = {s : Str ; n : Number} ;

  --mkDet : Str -> Number -> Determiner ; 
  --mkDet   str    num    = {s = str ; n = num} ;
  
  -- NOUNS
  --------

  Noun : Type = {s : Number => Str ; g : Gender} ;

  mkNoun : (sg,pl : Str) -> Noun = \sg,pl -> {
    s = table {Sg => sg ; Pl => pl} ;
    g = getGender sg
    } ;

  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "s") ;

  -- smart paradigm (ie. cheval, oiseau)
  smartNoun : Str -> Noun = \sg -> case sg of {
    x + "l"                   => mkNoun sg (x + "ux") ;
    _ + "u"                   => mkNoun sg (sg + "x") ;
    _                         => regNoun sg
    } ;

  getGender : Str -> Gender = \s -> case s of { 
    root + ("e") => Fem ; --general first then exceptions
    root + ("leur" | "ison") => Fem ;
    ("mer" | "eau") => Fem ;
    root + ("l" | "é" | "u" | "g" | "vre" | "on" | "in" | "t" | "ge" | "teur" | "en" | "i" | "bre" ) => Masc ; 
    ("homme") => Masc
  } ;

-- ADJECTIVES
-------------

  Adjective : Type = {s : Number => Gender => Str} ;

  mkAdj : (ASgMasc,ASgFem,APlMasc,APlFem : Str) -> Adjective 
  = \ASgMasc,ASgFem,APlMasc,APlFem ->
    {s = table {
      Sg => table { Masc => ASgMasc ; Fem => ASgFem } ;
      Pl => table { Masc => APlMasc ; Fem => APlFem } 
      } ;
    } ;
  
  -- regular verbs (ie. grand, bleu, frois, intelligent etc.)
  regAdj : (ASgMasc : Str) -> Adjective = 
    \ASgMasc -> mkAdj ASgMasc (ASgMasc + "e") (ASgMasc + "s") (ASgMasc + "es") ;

  -- with exceptions (in order: ends in e, -nnes, -elles, -eilles, -anches)
  smartAdj : Str -> Adjective = \ASgMasc -> case ASgMasc of {
    mauv + "ais"    => mkAdj ASgMasc (mauv + "aise") (mauv + "ais") (mauv + "aises") ;
    jeun + "e"      => mkAdj ASgMasc (jeun + "e") (jeun + "es") (jeun +"es") ;
    b + "on"        => mkAdj ASgMasc (b + "onne") (b + "ons") (b + "onnes") ;
    nouv + "eau"    => mkAdj ASgMasc (nouv + "elle") (nouv + "aux") (nouv + "elles") ;
    v + "ieux"      => mkAdj ASgMasc (v + "ielle") (v + "ieux") (v + "ieilles") ;
    bl + "anc"      => mkAdj ASgMasc (bl + "anche") (bl + "ancs") (bl + "anches") ;
    --reg verb
    _               => regAdj ASgMasc
  } ;

--runs fine without (possibly from adding all irregular verbs?)
  --irregAdj : (ASgMasc,ASgFem,APlMasc,APlFem : Str) -> Adjective 
    --= \ASgMasc,ASgFem,APlMasc,APlFem ->

-- VERBS
---------

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,FirstPerSg,SecondPerSg,ThirdPerSg,FirstPerPl,SecondPerPl,ThirdPerPl : Str) -> Verb
    = \inf,FirstPerSg,SecondPerSg,ThirdPerSg,FirstPerPl,SecondPerPl,ThirdPerPl -> {
    s = table {
      Inf => inf ;
      VPres Sg FirstPer => FirstPerSg ;
      VPres Sg SecondPer => SecondPerSg ;
      VPres Sg ThirdPer => ThirdPerSg ;
      VPres Pl FirstPer => FirstPerPl ; 
      VPres Pl SecondPer => SecondPerPl ;
      VPres Pl ThirdPer => ThirdPerPl 
      } 
    } ;

-- don't need if make smart verb?
  --regVerb : (inf : Str) -> Verb = \inf ->
    --mkVerb inf (inf + "s") (inf + "ed") (inf + "ed") (inf + "ing") ;

  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
     -- TENIR VERBS (not consistently working)
     v + "enir" => mkVerb inf (v + "iens") (v + "iens") (v + "ient") (v + "enons") (v + "enez") (v + "iennent") ;
     -- -SAVOIR VERBS 
     s + "avoir" => mkVerb inf (s + "ais") (s + "ais") (s + "ait") (s + "avons") (s + "avez") (s+ "avent") ;
     -- -LIRE VERBS - not consistent
     l + "ire" => mkVerb inf (l + "is") (l + "is") (l + "it") (l + "isons") (l + "isez") (l + "isent") ;
     -- -BOIRE VERBS
     b + "oire" => mkVerb inf (b + "ois") (b + "ois") (b + "oit") (b + "uvons") (b + "uvez") (b + "oivent") ;
     -- -VIVRE VERBS
     v + "ivre" => mkVerb inf (v + "is") (v + "is") (v + "it") (v + "ivons") (v + "ivez") (v + "ivent") ;
     -- -PRENDRE VERBS
     com + "prendre" => mkVerb inf (com + "prends") (com + "prends") (com + "prend") (com + "prenons") (com + "prenez") (com + "prennent") ;
     -- -ETER VERBS (e -> è)
     ach + "eter" => mkVerb inf (ach + "ète") (ach + "ètes") (ach + "ète") (ach + "etons") (ach + "etez") (ach + "ètent") ;
     -- -GER VERBS (manger, voyager)
     man + "ger" => mkVerb inf (man + "ge") (man + "ges") (man + "ge") (man + "geons") (man + "gez") (man + "gent") ;
     -- -MIR VERBS (dormir, endormir)
     dor + "mir" => mkVerb inf (dor + "s") (dor + "s") (dor + "t") (dor + "mons") (dor + "mez") (dor + "ment") ;
     -- -ER VERBS
     aim + "er" => mkVerb inf (aim + "e") (aim + "es") (aim + "e") (aim + "ons") (aim + "ez") (aim + "ent") ;
     -- -IR VERBS
     cour + "ir" => mkVerb inf (cour + "s") (cour + "s") (cour + "t") (cour + "ons") (cour + "ez") (cour + "ent") ;
     -- -RE VERBS
     attend + "re" => mkVerb inf (attend + "s") (attend + "s") (attend + "") (attend + "ons") (attend + "ez") (attend + "ent") 
     --_ => regVerb inf ; do i need???
     
     } ;

  -- normal irregular verbs 
  irregVerb : (inf,FirstPerSg,SecondPerSg,ThirdPerSg,FirstPerPl,SecondPerPl,ThirdPerPl : Str) -> Verb =
    \inf,FirstPerSg,SecondPerSg,ThirdPerSg,FirstPerPl,SecondPerPl,ThirdPerPl ->
      -- mkVerb inf FirstPerSg SecondPerSg ThirdPerSg FirstPerPl SecondPerPl ThirdPerPl ;
      let verb = smartVerb inf
      in mkVerb inf FirstPerSg SecondPerSg ThirdPerSg FirstPerPl SecondPerPl ThirdPerPl ;   

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Number => Gender => Str} ;

  be_Verb : Verb = mkVerb "être" "suis" "es" "est" "sommes" "êtes" "sont" ; ---s to be generalized

---s a very simplified verb agreement function for Micro
  agr2vform : Number -> VForm = \a -> case a of {
    Sg => VPres Sg ThirdPer ;
    Pl => VPres Pl ThirdPer
    } ;

-- FUTURE FIXES
-- getGender could be better (coded for specific lexicon given)
-- efficiency of irregular verbs (savoir, dormir)

}
