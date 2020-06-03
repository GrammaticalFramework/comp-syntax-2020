--# -path=.:../abstract
concrete MicroLangFra of MicroLang = open MicroResFra, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Gender => Number => Str; isPron : Bool; adv: Str} ; ---s special case of Mini
    NP = {s : Case => Str ; n : Number; p : Person ; g : Gender; isPron : Bool};
    Comp, AP, A = Adjective ;
    Pron = {s : Case => Str ; n : Number ; p : Person ; g : Gender} ;
    Det = {s : Gender => Str ; n : Number} ;
    Prep = {s : Str} ;
    V = {s : Person => Number => Str} ;
    V2 = Verb2 ;
    N, CN = Noun ;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;

    PredVPS np vp = {
      s = case vp.isPron of {
        True => np.s ! Nom ++ vp.compl ! np.g ! np.n ++ vp.verb.s ! np.p ! np.n ++ vp.adv ;
        False => np.s ! Nom ++ vp.verb.s ! np.p ! np.n ++ vp.compl ! np.g ! np.n ++ vp.adv
        }
      } ;
  
    UseV v = {
      verb = v ;
      compl = \\g,n => [] ;
      isPron = False ;
      adv = []
      } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      compl = \\g,n => v2.c ! g ! n ++ np.s ! Acc ; -- NP object in the accusative, preposition first
      isPron = np.isPron ;
      adv = []
      } ;
         
    UseComp comp = {
      compl = \\g,n => comp.s ! g ! n ;
      verb = be_Verb ;     -- the verb is the copula "be"
      isPron = False ;
      adv = []
      } ;
 
    CompAP ap = ap ;

    AdvVP vp adv =
      vp ** {adv = adv.s} ;
      
    DetCN det cn = {
      s = \\c => det.s ! cn.g ++ cn.s ! det.n ;
      g = cn.g ;
      n = det.n ;
      p = P3 ;
      isPron = False
      } ;
      
    UsePron p = p ** {isPron = True} ;
            
    a_Det = {s = table {M => "un" ; F => "une"} ; n = Sg} ;
    aPl_Det = {s = table {M => "des" ; F => "des"} ; n = Pl} ;
    the_Det = {s = table {
	M => pre {"a"|"e"|"i"|"o"|"h" => "l'" ; _ =>  "le"} ; 
	F => pre {"a"|"e"|"i"|"o"|"h" => "l'" ; _ =>  "la"} 
	} ;
	n = Sg
	} ;
    thePl_Det = {s = table {M => "les" ; F => "les"} ; n = Pl} ;
    
    UseN n = n ** {isPron = False} ;    

    AdjCN ap cn = {
      s = \\n => case ap.isPre of {
        True => ap.s ! cn.g ! n ++ cn.s ! n ;
        False => cn.s ! n ++ ap.s ! cn.g ! n } ;
      g = cn.g ;
      } ;

    PositA a = a ;

    PrepNP prep np = {
      s = prep.s ++ np.s ! Dat ;
      isPron = False
      } ;

    in_Prep = {s = "dans"} ;
    on_Prep = {s = "sur"} ;
    with_Prep = {s = "avec"} ;

    he_Pron = {
      s = table {Nom => "il" ; Acc => pre {"a"|"e"|"i"|"o"|"h" => "l'" ; _ =>  "le"} ; Dat => "lui"} ;
      n = Sg ;
      p = P3 ;
      g = M ;
      } ;
    she_Pron = {
      s = table {Nom => "elle" ; Acc => pre {"a"|"e"|"i"|"o"|"h" => "l'" ; _ =>  "la"} ; Dat => "elle"} ;
      n = Sg ;
      p = P3 ;
      g = F ;
      } ;
    they_Pron = {
      s = table {Nom => "elles" ; Acc => "les" ; Dat => "eux"} ;
      n = Pl ;
      p = P3 ;
      g = F ;
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "déjà" ;
lin animal_N = mkN "animal" ;  --m
lin apple_N = mkN "pomme" ;  --m
lin baby_N = mkN "bébé" ; --m
lin bad_A = mkA "mal" ;
lin beer_N = mkN "bière" ;  --f
lin big_A = mkA "grand" True ;
lin bike_N = mkN "vélo" ;  --m
lin bird_N = mkN "oiseau" ;  --m
lin black_A = mkA "noir" ;
lin blood_N = mkN "sang" ;  --m
lin blue_A = mkA "bleu" ;
lin boat_N = mkN "bateau" ;  --m
lin book_N = mkN "livre" ;  --m
lin boy_N = mkN "garçon" ;  --m
lin bread_N = mkN "pain" ;  --m
lin break_V2 = mkV2 (mkV "casser") ;
lin buy_V2 = mkV2 (mkV "acheter") ;
lin car_N = mkN "voiture" ;  --f
lin cat_N = mkN "chat" ;  --m
lin child_N = mkN "enfant" ;  --m
lin city_N = mkN "ville" ;  --l
lin clean_A = mkA "propre" ;
lin clever_A = mkA "malin" ;
lin cloud_N = mkN "nuage" ;  --m
lin cold_A = mkA "froid" ;
lin come_V = mkV "venir" "viens" "venons" "viens" "venez" "vient" "viennent" ;
lin computer_N = mkN "ordinateur" ;  --m
lin cow_N = mkN "vache" ;  --f
lin dirty_A = mkA "sale" ;
lin dog_N = mkN "chien" ;  --m
lin drink_V2 = mkV2 (mkV "boire") ;
lin eat_V2 = mkV2 (mkV "manger") ;
lin find_V2 = mkV2 (mkV "trouver") ;
lin fire_N = mkN "feu" ;  --m
lin fish_N = mkN "poisson" ;  --m
lin flower_N = mkN "fleur" ;  --f
lin friend_N = mkN "amie" ;  --f
lin girl_N = mkN "fille" ;  --f
lin good_A = mkA "bon" True  ;
lin go_V = mkV "aller" "vais" "allons" "vas" "allez" "va" "vont" ;
lin grammar_N = mkN "grammaire" ;  --f
lin green_A = mkA "vert" ;
lin heavy_A = mkA "lourd" ;
lin horse_N = mkN "cheval" ;  --m
lin hot_A = mkA "chaud" ;
lin house_N = mkN "maison" ;  --f
lin jump_V = mkV "bondir" "bondis" "bondissons" "bondis" "bondissez" "bondit" "bondissent" ;
lin kill_V2 = mkV2 "tuer" ;
lin language_N = mkN "langue" ;  --f
lin live_V = mkV "vivre" ;
lin love_V2 = mkV2 (mkV "aimer") ;
lin man_N = mkN "homme" ;  --m
lin milk_N = mkN "lait" ;  --m
lin music_N = mkN "musique" ;  --f
lin new_A = mkA "nouveau" True ;
lin now_Adv = mkAdv "maintenant" ;
lin old_A = mkA "vieux" True ;
lin play_V = mkV "jouer" ;
lin read_V2 = mkV2 (mkV "lire") ;
lin ready_A = mkA "prêt" ;
lin red_A = mkA "rouge" ;
lin river_N = mkN "rivière" ;  --f
lin run_V = mkV "courir" ;
lin sea_N = mkN "océan" ;  --m
lin see_V2 = mkV2 (mkV "voir") ;
lin ship_N = mkN "navire" ;  --m
lin sleep_V = mkV "dormir" ;
lin small_A = mkA "petit" True ;
lin star_N = mkN "étoile" ;  --f
lin swim_V = mkV "nager";
lin teach_V2 = mkV2 (mkV "apprendre") ;
lin train_N = mkN "train" ;  --m
lin travel_V = mkV "voyager" ;
lin tree_N = mkN "arbre" ;  --m
lin understand_V2 = mkV2 (mkV "entendre") ;
lin wait_V2 = mkV2 "attendre" ;
lin walk_V = mkV "marcher" ;
lin warm_A = mkA "chaud" ;
lin water_N = mkN "eau" ;  --f
lin white_A = mkA "blanc" ;
lin wine_N = mkN "vin" ;  --m
lin woman_N = mkN "femme" ;  --f
lin yellow_A = mkA "jaune" ;
lin young_A = mkA "jeune" True ;


---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN = overload {
    mkN : Str -> Noun   -- predictable noun, e.g. car-cars, boy-boys, fly-flies, bush-bushes
      = \n -> lin N (smartNoun n) ;
    mkN : Str -> Str -> Noun  -- irregular noun, e.g. man-men
      = \sg,pl -> lin N (mkNoun sg pl) ;
    } ;

  mkA = overload {
    mkA : Str -> A = \a -> lin A (smartAdj a) ;
    mkA : Str -> Bool -> A = \a,p -> lin A (smartAdj a ** { isPre = p }) ;
    } ;

  mkV = overload {
    mkV : (Inf: Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \stem -> lin V (smartVerb stem) ;
    mkV : (Inf,p1sg,p1pl,p2sg,p2pl,p3sg,p3pl : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \Inf,p1sg,p1pl,p2sg,p2pl,p3sg,p3pl -> lin V (irregVerb Inf p1sg p1pl p2sg p2pl p3sg p3pl) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = \\g,n => []}) ;
    mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = \\g,n => p}) ;
    mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = \\g,n => []}) ;
    mkV2 : V -> Str -> V2     -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = \\g,n => p}) ;
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}
