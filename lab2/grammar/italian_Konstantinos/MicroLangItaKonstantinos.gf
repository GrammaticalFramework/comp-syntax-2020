
--# -path=.:../abstract
concrete MicroLangItaKonstantinos of MicroLang = open MicroResItaKonstantinos, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Gender => Number => Str ; isPron : Bool }; --s special case of Mini
    Comp = Adj ;
    AP = Adj ;
    CN = Noun ;
    NP = {s : Case => Str ; n : Number ; g : Gender ; isPron : Bool } ; 
    Pron = {s : Case => Str ; n : Number ; g : Gender} ;
    Det = {s : Gender => Str ; n : Number} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adj ;
    N = Noun ;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc } ; 

    PredVPS np vp = {
      s= case vp.isPron of {
        True => np.s ! Nom ++ vp.compl !np.g ! np.n ++ vp.verb.s ! agr2vform np.n ; 
        False => np.s ! Nom ++ vp.verb.s ! agr2vform np.n ++ vp.compl ! np.g ! np.n   
      }
      } ;

	
    

    UseV v = {
      verb = v ;
      compl = \\g,n => [] ; isPron = False } ;

   ComplV2 v2 np = {
     verb = v2 ;
     compl = \\g,n => v2.c ! g ! n ++ np.s ! Acc ; isPron = np.isPron } ;

   UseComp comp = {
     verb = be_Verb ;     -- the verb is the copula "essere"
     compl = \\g,n => comp.s ! g ! n ; isPron = False } ;

   CompAP ap = ap ;

   AdvVP vp adv =
      vp ** {compl = \\g,n => vp.compl ! g ! n ++ adv.s} ; 

   DetCN det cn = {
     s = \\c => det.s ! cn.g ++ cn.s ! det.n ;
     n = det.n ;
     g = cn.g ;
     isPron = False      
     } ;

   UsePron p = { s = p.s  ; isPron = True ; g = p.g ; n = p.n} ;
 
    a_Det = {s = table {Masc => pre { "z"|"x"|"y"|"st" => "uno" ; _ => "un" } ;
    Fem => pre {"z"|"x"|"y"|"st" => "una" ; _ => "un"} } ; n = Sg} ; 
    aPl_Det = {s = \\_ => "" ; n = Pl} ;
    the_Det = {s = table {Masc => pre {"a"|"e"|"i"|"o"|"u" => "l'" ++ BIND ;
    "st"|"z"|"x"|"y" => "lo" ;
     _ => "il" } ; 
						  Fem => pre {"a"|"e"|"i"|"o"|"u" => "l'" ++ BIND ;
               _ => "la"
               } } ;
                 
						  n = Sg} ; 
    thePl_Det = {s = table {
		   Fem => "le" ;
		   Masc => pre {
		     "a"|"e"|"i"|"o"|"u"|"st"|"z" => "gli" ;
		     _ => "i"
		     }
		   } ;
		 n = Pl} ;
      


   UseN n = n ;
  

	AdjCN ap cn = {
	s = \\n => case ap.isAfter of {
	True => cn.s ! n ++ ap.s ! cn.g ! n ; 
	False => ap.s ! cn.g ! n ++ cn.s ! n } ; 
	g = cn.g ;
	} ;     

   PositA a = a ;

   PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

   in_Prep = {s = "in"} ;
   on_Prep = {s = "su"} ;
   with_Prep = {s = "con"} ;

    he_Pron = {
      s = table {Nom => "lui" ; Acc => pre {"a"|"e"|"i"|"o"|"u"|"ho" => "l'" ; _ => "lo" }} ; --covers only the words appearing in the lexicon
      n = Sg ;
	  g = Masc ;
      } ;
    she_Pron = {
      s = table {Nom => "lei" ; Acc => pre {"a"|"e"|"i"|"o"|"u"|"ho" => "l'" ; _ => "la" }} ;
      n = Sg ; 
	  g = Fem ;
      } ;
    they_Pron = { 
      s = table {Nom => "loro" ;  -- only masculine for now, feminine is le
				 Acc => "li" } ;
      n = Pl ;
	  g = Masc ;
      } ;


-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "già" ;
lin animal_N = mkN "animale" ; --masc
lin apple_N = mkN "mela" ;
lin baby_N = mkN "bambino" ;
lin bad_A = mkA "cattivo" False ;
lin beer_N = mkN "birra" ;
lin big_A = mkA "grande" False ;
lin bike_N = mkN "bicicletta" ;
lin bird_N = mkN "uccello" ;
lin black_A = mkA "nero" ;
lin blood_N = mkN "sangue" ; --masc
lin blue_A = mkA "blu" ;
lin boat_N = mkN "barca" ;
lin book_N = mkN "libro" ;
lin boy_N = mkN "ragazzo" ;
lin bread_N = mkN "pane" ; --masc
lin break_V2 = mkV2 (mkV "rompere") ; --reg
lin buy_V2 = mkV2 (mkV "acquistare") ; --reg
lin car_N = mkN "auto" ;
lin cat_N = mkN "gatta" ;
lin child_N = mkN "bambina" ;
lin city_N = mkN "città" ;
lin clean_A = mkA "pulito" ;
lin clever_A = mkA "intelligente" ;
lin cloud_N = mkN "nube" ; --Fem
lin cold_A = mkA "freddo" ;
lin come_V = mkV "arrivare" ; --reg, could change it to venire but venire is quite irregular
lin computer_N = mkN "computer" ; --masc
lin cow_N = mkN "mucca" ;
lin dirty_A = mkA "sporco" ;
lin dog_N = mkN "cane" ;
lin drink_V2 = mkV2 (mkV "bere" "bevo" "beviamo" "bevi" "bevete" "beve" "bevono") ; --irreg
lin eat_V2 = mkV2 (mkV "mangiare") ; -- reg
lin find_V2 = mkV2 (mkV "trovare") ; --reg
lin fire_N = mkN "fuoco" ;
lin fish_N = mkN "pesce" ; --masc
lin flower_N = mkN "fiore" ;
lin friend_N = mkN "amico" ;
lin girl_N = mkN "ragazza" ;
lin good_A = mkA "bello" ;
lin go_V = mkV "partire" ; --reg 3rd conjugation
lin grammar_N = mkN "grammatica" ;
lin green_A = mkA "verde" ;
lin heavy_A = mkA "pesante" ;
lin horse_N = mkN "cavalla" ;
lin hot_A = mkA "caldo" ;
lin house_N = mkN "casa" ;
--lin john_PN = mkPN "Giovanni" ;
lin jump_V = mkV "saltare" ;  --reg
lin kill_V2 = mkV2 "uccidere" ; --reg
--lin know_VS = mkVS (mkV "conoscere") ;
lin language_N = mkN "linguaggio" "linguaggi" ;
lin live_V = mkV "vivere" ;
lin love_V2 = mkV2 (mkV "amare") ; --reg
lin man_N = mkN "uomo" "uomini";
lin milk_N = mkN "latte" ; --masc
lin music_N = mkN "musica" ;
lin new_A = mkA "nuovo" False ;
lin now_Adv = mkAdv "adesso" ;
lin old_A = mkA "antico" False ;
--lin paris_PN = mkPN "Parigi" ;
lin play_V = mkV "giocare" ;
lin read_V2 = mkV2 (mkV "leggere") ; --reg
lin ready_A = mkA "pronto" ;
lin red_A = mkA "rosso" ;
lin river_N = mkN "fiume" ; --masc
lin run_V = mkV "correre" ; --reg
lin sea_N = mkN "mare" ; --masc
lin see_V2 = mkV2 (mkV "vedere") ; --reg
lin ship_N = mkN "nave" ;
lin sleep_V = mkV "dormire" ; --reg
lin small_A = mkA "piccolo" False ;
lin star_N = mkN "stella" ; --needs una as det
lin swim_V = mkV "nuotare" ; --reg
lin teach_V2 = mkV2 (mkV "insegnare") ; --reg
lin train_N = mkN "treno" ;
lin travel_V = mkV "viaggiare" ;  --reg but might need to create a case for -iare verbs
lin tree_N = mkN "albero" ;
lin understand_V2 = mkV2 (mkV "capire" "capisco" "capiamo" "capisci" "capite" "capisce" "capiscono") ; --very irregular
lin wait_V2 = mkV2 "aspettare" ; --reg
lin walk_V = mkV "camminare" ; --reg
lin warm_A = mkA "caldo" ;
lin water_N = mkN "acqua" ;
lin white_A = mkA "bianco" ;
lin wine_N = mkN "vino" ;
lin woman_N = mkN "donna" ;
lin yellow_A = mkA "giallo" ;
lin young_A = mkA "giovane" False ;

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
    mkA : Str -> Bool -> A = \a,p -> lin A (smartAdj a ** { isAfter = p }) ;

  } ;

  mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    mkV : (inf,Per1sg,Per2sg,Per3sg,Per1pl,Per2pl,Per3pl : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \inf,Per1sg,Per2sg,Per3sg,Per1pl,Per2pl,Per3pl -> lin V (irregVerb inf Per1sg Per2sg Per3sg Per1pl Per2pl Per3pl) ;
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


