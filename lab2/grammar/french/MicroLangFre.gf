--# -path=.:../abstract
concrete MicroLangFre of MicroLang = open MicroResFre, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Gender => Number => Str ; isPron : Bool ; adv : Str } ; ---s special case of Mini
    Comp = Adj ;
	AP = Adj ;
    CN = Noun ;
    NP = {s : Case => Str ; n : Number; g : Gender ; isPron : Bool } ;
    Pron = {s : Case => Str ; n : Number ; g : Gender} ;
    Det = {s : Gender => Str ; n : Number} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
	A = Adj ;
	N = Noun;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;

	PredVPS np vp = {
	s = np.s ! Nom ++
	case vp.isPron of {
	  True => vp.compl ! np.g ! np.n ++ vp.verb.s ! agr2vform np.n ++ vp.adv;
	  False => vp.verb.s ! agr2vform np.n ++ vp.compl ! np.g ! np.n ++ vp.adv
	}
	};
      
    UseV v = {
      verb = v ;
      compl = \\g, n => [] ;
	  isPron = False ; 
	  adv = []
      } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      compl = \\g, n => v2.c ! g ! n ++ np.s ! Acc ; -- NP object in the accusative, preposition first  
      isPron = np.isPron ;
	  adv = []
	  } ;
      
    UseComp comp = {
      verb = be_Verb ;     -- the verb is the copula "be"
      compl = \\g,n => comp.s ! g ! n ;
	  isPron = False ;
	  adv = []
      } ;
      
    CompAP ap = ap ;
      
    AdvVP vp adv =
	  vp ** {adv = adv.s} ;
      
    DetCN det cn = {
      s = \\c => det.s ! cn.g ++ cn.s ! det.n ;
      n = det.n ;
	  g = cn.g ;
	  isPron = False
      } ;
      
    UsePron p = { s = p.s ; isPron = True ; g = p.g ; n = p.n } ;
            
    a_Det = {s = table {Fem => "une" ; Masc => "un"} ; n = Sg} ; -- only masculine for now, feminine is une
    aPl_Det = {s = table {Fem|Masc => "des"} ; n = Pl} ;
    the_Det = {s = table {Fem => pre {"a"|"e"|"i"|"o"|"u"|"ha"|"he"|"hi"|"ho"|"hu" => "l'" ; _ => "la" } ; 
						  Masc => pre {"a"|"e"|"i"|"o"|"u"|"ha"|"he"|"hi"|"ho"|"hu" => "l'" ; _ => "le" } }; 
						  n = Sg} ; 
    thePl_Det = {s = table {Fem|Masc => "les"} ; n = Pl} ;
    
    UseN n = n ;
	
	AdjCN ap cn = {
	s = case ap.isPre of {
	True => \\n => cn.s ! n ++ ap.s ! cn.g ! n ;
	False => \\n => ap.s ! cn.g ! n ++ cn.s ! n } ;
	g = cn.g ;
	} ;
	  

    PositA a = a ;  -- original english one

    PrepNP prep np = {s = prep.s ++ np.s ! Nom_s} ;

    in_Prep = {s = "dans"} ;
    on_Prep = {s = "sur"} ;
    with_Prep = {s = "avec"} ;

    he_Pron = {
      s = table {Nom => "il" ; Nom_s => "lui" ; Acc => pre {"a"|"e"|"i"|"o"|"u"|"ha"|"he"|"hi"|"ho"|"hu" => "l'" ; _ => "le" }} ;
      n = Sg ;
	  g = Masc ;
      } ;
    she_Pron = {
      s = table {Nom => "elle" ; Nom_s => "elle" ; Acc => pre {"a"|"e"|"i"|"o"|"u"|"ha"|"he"|"hi"|"ho"|"hu" => "l'" ; _ => "la" }} ;
      n = Sg ; 
	  g = Fem ;
      } ;
    they_Pron = {  -- only masculine for now, feminine is elles
      s = table {Nom => "ils" ; 
	  		     Nom_s => "eux" ;
				 Acc => "les" } ;
      n = Pl ;
	  g = Masc ;
      } ;
	  
-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "déjà" ;
lin animal_N = mkN "animal" ;
lin apple_N = mkN "pomme" ;
lin baby_N = mkN "bébé" ;
lin bad_A = mkA "mauvais" False ;
lin beer_N = mkN "bière" ;
lin big_A = mkA "grand" False;
lin bike_N = mkN "vélo" ;
lin bird_N = mkN "oiseau" ;
lin black_A = mkA "noir" ;
lin blood_N = mkN "sang" ;
lin blue_A = mkA "bleu" ;
lin boat_N = mkN "bateau" ;
lin book_N = mkN "livre" ;
lin boy_N = mkN "garçon" ;
lin bread_N = mkN "pain" ;
lin break_V2 = mkV2 (mkV "casser") ;
lin buy_V2 = mkV2 (mkV "acheter" "achète" "achètes" "achète" "achetons" "achetez" "achètent") ;
lin car_N = mkN "voiture" ;
lin cat_N = mkN "chat" ;
lin child_N = mkN "enfant" ;
lin city_N = mkN "ville" ;
lin clean_A = mkA "propre" ;
lin clever_A = mkA "intelligent" ;
lin cloud_N = mkN "nuage" ;
lin cold_A = mkA "froid" ;
lin come_V = mkV "venir" "viens" "viens" "vient" "venons" "venez" "viennent" ;
lin computer_N = mkN "ordinateur" ;
lin cow_N = mkN "vache" ;
lin dirty_A = mkA "sale" ;
lin dog_N = mkN "chien" ;
lin drink_V2 = mkV2 (mkV "boire" "bois" "bois" "boit" "buvons" "buvez" "boivent") ;
lin eat_V2 = mkV2 (mkV "manger" "mange" "manges" "mange" "mangeons" "mangez" "mangent") ;
lin find_V2 = mkV2 (mkV "trouver") ;
lin fire_N = mkN "feux" ;
lin fish_N = mkN "poisson" ;
lin flower_N = mkN "fleur" ;
lin friend_N = mkN "amie" ; --only female version for now, male would be ami
lin girl_N = mkN "fille" ;
lin good_A = mkA "bon" False ;
lin go_V = mkV "aller" "vais" "vas" "va" "allons" "allez" "vont" ;
lin grammar_N = mkN "grammaire" ;
lin green_A = mkA "vert" ;
lin heavy_A = mkA "lourd" ;
lin horse_N = mkN "cheval" ;
lin hot_A = mkA "chaud" ;
lin house_N = mkN "maison" ;
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "sauter" ;
lin kill_V2 = mkV2 "tuer" ;
-- lin know_VS = mkVS (mkV "savoir" "sais" "sais" "sait" "savons" "savez" "savent") ;
lin language_N = mkN "langue" ;
lin live_V = mkV "vivre" "vis" "vis" "vit" "vivons" "vivez" "vivent" ;
lin love_V2 = mkV2 (mkV "aimer") ;
lin man_N = mkN "homme" ;
lin milk_N = mkN "lait" ;
lin music_N = mkN "musique" ;
lin new_A = mkA "nouveau" False ;  
lin now_Adv = mkAdv "maintenant" ;
lin old_A = mkA "ancien" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "jouer" ;
lin read_V2 = mkV2 (mkV "lire" "lis" "lis" "lit" "lisons" "lisez" "lisent") ;
lin ready_A = mkA "prêt" ;
lin red_A = mkA "rouge" ;
lin river_N = mkN "fleuve" ;
lin run_V = mkV "courir" ;
lin sea_N = mkN "océan" ;
lin see_V2 = mkV2 (mkV "voir" "vois" "vois" "voit" "voyons" "voyez" "voient") ;
lin ship_N = mkN "navire" ;
lin sleep_V = mkV "dormir" "dors" "dors" "dort" "dormons" "dormez" "dorment";
lin small_A = mkA "petit" False ;
lin star_N = mkN "étoile" ;
lin swim_V = mkV "nager" "nage" "nages" "nage" "nageons" "nagez" "nagent" ;
lin teach_V2 = mkV2 (mkV "enseigner") ;
lin train_N = mkN "train" ;
lin travel_V = mkV "voyager" "voyage" "voyages" "voyage" "voyageons" "voyagez" "voyagent" ;
lin tree_N = mkN "arbre" ;
lin understand_V2 = mkV2 (mkV "comprendre") ;
lin wait_V2 = mkV2 "attendre" ; -- doesnt take preposition in French
lin walk_V = mkV "marcher" ;
lin warm_A = mkA "chaud" ;  
lin water_N = mkN "eau" ;
lin white_A = mkA "blanc" "blanche" "blancs" "blanches" ;
lin wine_N = mkN "vin" ;
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

  mkA = overload {
    mkA : Str -> A  -- regular adjectives
	= \s -> lin A (smartAdj s) ;
	mkA : Str -> Bool -> A = \s,p -> lin A (smartAdj s ** { isPre = p }) ;
	mkA : (mascsg,femsg,mascpl,fempl : Str) -> A -- irregular adjectives
	= \mascsg,femsg,mascpl,fempl -> lin A (irregAdj mascsg femsg mascpl fempl) ;
	mkA : (mascsg,femsg,mascpl,fempl : Str) -> Bool -> A = \mascsg,femsg,mascpl,fempl,p -> lin A (irregAdj mascsg femsg mascpl fempl ** { isPre = p })
} ; 

  mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    mkV : (inf,sg1,sg2,sg3,pl1,pl2,pl3 : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \inf,sg1,sg2,sg3,pl1,pl2,pl3 -> lin V (irregVerb inf sg1 sg2 sg3 pl1 pl2 pl3) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = \\g, n => []}) ;
    mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = \\g, n => p}) ;
    mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = \\g, n => []}) ;
    mkV2 : V -> Str -> V2     -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = \\g, n => p}) ;
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}
