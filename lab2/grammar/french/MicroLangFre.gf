--# -path=.:../abstract
concrete MicroLangFre of MicroLang = open MicroResFre, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Str} ; ---s special case of Mini
    Comp = {s : Str} ;
    AP = A ;
    CN = Noun ;
    NP = {s : Case => Str ; a : Agreement} ;
    Pron = {s : Case => Str ; a : Agreement} ;
    Det = {s : Str ; n : Number} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    -- A = Adjective ;
	A = {s : Gender => Number => Str} ;
	-- A = {s : Agr => Str} ;
    -- N = Noun ;
	N = {s : Number => Str ; g : Gender};
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;

    PredVPS np vp = {
      s = np.s ! Nom ++ vp.verb.s ! agr2vform np.a ++ vp.compl
      } ;
      
    UseV v = {
      verb = v ;
      compl = [] ;
      } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      compl = v2.c ++ np.s ! Acc  -- NP object in the accusative, preposition first
      } ;
      
    UseComp comp = {
      verb = be_Verb ;     -- the verb is the copula "be"
      compl = comp.s
      } ;
      
--    CompAP ap = ap ;
      
    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;
      
--    DetCN det cn = {
--      s = \\c => det.s ++ cn.s ! det.n ;
  --    a = Agr det.n ;
    --  } ;
      
    UsePron p = p ;
            
    a_Det = {s = pre {"a"|"e"|"i"|"o" => "an" ; _ => "a"} ; n = Sg} ; --- a/an can get wrong
    aPl_Det = {s = "" ; n = Pl} ;
    the_Det = {s = "the" ; n = Sg} ;
    thePl_Det = {s = "the" ; n = Pl} ;
    
    UseN n = n ;
    
    --AdjCN ap cn = {
      --s = table {n => ap.s ! cn.g ++ cn.s}  -- agreement correct??? no, its not, ofc!
      --} ;
	  
	AdjCN ap cn = {
	  s = \\n => ap.s ! cn.g ! n ++ cn.s ! n ; g = cn.g
	  } ;
	  

    PositA a = a ;  -- original english one
	--PositA a = Agreement => a ;
	--PositA a = {
	--s = table {Agreement => a.s} 
	--} ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    in_Prep = {s = "in"} ;
    on_Prep = {s = "on"} ;
    with_Prep = {s = "with"} ;

    he_Pron = {
      s = table {Nom => "il" ; Acc => "le"} ;
      a = Agr Sg Masc ;
      } ;
    she_Pron = {
      s = table {Nom => "elle" ; Acc => "la"} ;
      a = Agr Sg Fem ;
      } ;
    they_Pron = {  -- only masculine for now, female is elles
      s = table {Nom => "ils" ; 
	  		     Acc => "les"} ;
      a = Agr Pl Masc;
      } ;
	  
-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "déjà" ;
lin animal_N = mkN "animal" ** {g = Masc} ;
lin apple_N = mkN "pomme" ;
lin baby_N = mkN "bébé" ** {g = Masc};
lin bad_A = mkA "mauvais" ;
lin beer_N = mkN "bière" ;
lin big_A = mkA "grand" ;
lin bike_N = mkN "vélo" ** {g = Masc} ;
lin bird_N = mkN "oiseau" ** {g = Masc} ;
lin black_A = mkA "noir" ;
lin blood_N = mkN "sang" ** {g = Masc} ;
lin blue_A = mkA "bleu" ;
lin boat_N = mkN "bateau" ** {g = Masc} ;
lin book_N = mkN "livre" ** {g = Masc} ;
lin boy_N = mkN "garçon" ** {g = Masc} ;
lin bread_N = mkN "pain" ** {g = Masc} ;
lin break_V2 = mkV2 (mkV "casser") ;
lin buy_V2 = mkV2 (mkV "acheter" "achète" "achètes" "achète" "achetons" "achetez" "achètent") ;
lin car_N = mkN "voiture" ;
lin cat_N = mkN "chat" ** {g = Masc} ;
lin child_N = mkN "enfant" ** {g = Masc} ;
lin city_N = mkN "ville" ;
lin clean_A = mkA "propre" ;
lin clever_A = mkA "intelligent" ;
lin cloud_N = mkN "nuage" ** {g = Masc} ;
lin cold_A = mkA "froid" ;
lin come_V = mkV ("venir" "viens" "viens" "vient" "venons" "venez" "viennent") ;
lin computer_N = mkN "ordinateur" ** {g = Masc} ;
lin cow_N = mkN "vache" ;
lin dirty_A = mkA "sale" ;
lin dog_N = mkN "chien" ** {g = Masc} ;
lin drink_V2 = mkV2 (mkV "boire" "bois" "bois" "boit" "buvons" "buvez" "boivent") ;
lin eat_V2 = mkV2 (mkV "manger" "mange" "manges" "mange" "mangeons" "mangez" "mangent") ;
lin find_V2 = mkV2 (mkV "trouver") ;
lin fire_N = mkN "feux" ** {g = Masc} ;
lin fish_N = mkN "poisson" ** {g = Masc} ;
lin flower_N = mkN "fleur" ;
lin friend_N = mkN "amie" ; --only female version for now, male would be ami
lin girl_N = mkN "fille" ;
lin good_A = mkA "bon" ;
lin go_V = mkV "aller" "vais" "vas" "va" "allons" "allez" "vont" ;
lin grammar_N = mkN "grammaire" ;
lin green_A = mkA "vert" ;
lin heavy_A = mkA "lourd" ;
lin horse_N = mkN "cheval" ** {g = Masc} ;
lin hot_A = mkA "chaud" ;
lin house_N = mkN "maison" ;
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "sauter" ;
lin kill_V2 = mkV2 "tuer" ;
-- lin know_VS = mkVS (mkV "savoir" "sais" "sais" "sait" "savons" "savez" "savent") ;
lin language_N = mkN "langue" ;
lin live_V = mkV "vivre" "vis" "vis" "vit" "vivons" "vivez" "vivent" ;
lin love_V2 = mkV2 (mkV "aimer") ;
lin man_N = mkN "homme" ** {g = Masc} ;
lin milk_N = mkN "lait" ** {g = Masc} ;
lin music_N = mkN "musique" ;
lin new_A = mkA "nouveau" ;  -- does adj rule for this make sense?
lin now_Adv = mkAdv "maintenant" ;
lin old_A = mkA "ancien" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "jouer" ;
lin read_V2 = mkV2 (mkV "lire" "lis" "lis" "lit" "lisons" "lisez" "lisent") ;
lin ready_A = mkA "prêt" ;
lin red_A = mkA "rouge" ;
lin river_N = mkN "fleuve" ** {g = Masc} ;
lin run_V = mkV "courir" ;
lin sea_N = mkN "océan" ** {g = Masc} ;
lin see_V2 = mkV2 (mkV "voir" "vois" "vois" "voit" "voyons" "voyez" "voient") ;
lin ship_N = mkN "navire" ** {g = Masc} ;
lin sleep_V = mkV "dormir" "dors" "dors" "dort" "dormons" "dormez" "dorment";
lin small_A = mkA "petit" ;
lin star_N = mkN "étoile" ;
lin swim_V = mkV "nager" "nage" "nages" "nage" "nageons" "nagez" "nagent" ;
lin teach_V2 = mkV2 (mkV "enseigner") ;
lin train_N = mkN "train" ** {g = Masc} ;
lin travel_V = mkV "voyager" "voyage" "voyages" "voyage" "voyageons" "voyagez" "voyagent" ;
lin tree_N = mkN "arbre" ** {g = Masc} ;
lin understand_V2 = mkV2 (mkV "comprendre") ;
lin wait_V2 = mkV2 "attendre" "pour" ;  -- not sure how to translate this, sometimes pour, que, /?
lin walk_V = mkV "marcher" ;
lin warm_A = mkA "chaud" ;   -- problem that hot and warm same word?
lin water_N = mkN "eau" ;
lin white_A = mkA "blanc" "blanche" "blancs" "blanches" ;
lin wine_N = mkN "vin" ** {g = Masc} ;
lin woman_N = mkN "femme" ;
lin yellow_A = mkA "jaune" ;
lin young_A = mkA "jeune" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN = overload {
    mkN : Str -> Noun   -- predictable noun, e.g. car-cars, boy-boys, fly-flies, bush-bushes
      = \n -> lin N (smartNoun n) ;
	 -- = \s -> smartNoun s ;
    mkN : Str -> Str -> Noun  -- irregular noun, e.g. man-men
      = \sg,pl -> lin N (mkNoun sg pl) ;
	  --= \sg,pl -> mkNoun sg pl ;
    } ;

  mkA : Str -> A
--    = \s -> lin A {s = s} ;
    = \s -> lin A (smartAdj s) ;

  mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    mkV : (inf,pres,part : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \inf,pres,part -> lin V (irregVerb inf pres part) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = []}) ;
    mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
    mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = []}) ;
    mkV2 : V -> Str -> V2     -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = p}) ;
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}
