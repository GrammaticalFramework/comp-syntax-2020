--# -path=.:../../abstract
concrete MicroLangIta of MicroLang = open MicroResIta, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Gender => Number => Str ; isPron : Bool } ; ---s special case of Mini
    Comp = Adjective ;
    AP = Adjective ;
    CN = Noun ;
    NP = NounPhrase ;
    Pron = Pronoun ;
    Det = {s : Gender => Str ; n : Number} ;
    Prep = Preposition ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;
      
    PredVPS np vp = {
      s = case np.isPron of {
	True => case np.n of { Sg => np.s ! Nom ; Pl => "" } ;
	False => np.det ++ np.s ! Nom
	} ++
	case vp.isPron of {
	  True => vp.compl ! np.g ! np.n ++ vp.verb.s ! Ind Present np.n P3 ;
	  False => 
	    vp.verb.s ! Ind Present np.n P3 ++ vp.compl ! np.g ! np.n
	}
      };
    
    UseV v = {
      verb = v ;
      compl = \\_,_ => [] ;
      isPron = False
      } ;

    ComplV2 v2 np = {
      verb = v2 ;
      compl = \\_,_ => v2.c.con ! np.g ! np.n ++ np.det ++ np.s ! Acc ;  -- NP object in the accusative, preposition first
      isPron = np.isPron 
      } ;
    
    UseComp comp = {
      verb = be_Verb ;     -- the verb is the copula "be"
      compl = comp.s ;
      isPron = False ;
      } ;
    
    CompAP ap = ap ;
    
    AdvVP vp adv =
      vp ** {compl = \\g,n => vp.compl ! g ! n ++ adv.s} ;

    DetCN det cn = {
      s = \\c => cn.s ! det.n ;
      det = det.s ! cn.g ;
      n = det.n ;
      g = cn.g ;
      isPron = False 
      } ;
    
    UsePron p = p ** { det = "" ; isPron = True } ;
    
    a_Det = {s = table {
	       Masc => pre {
		 "sb"|"sc"|"sd"|"sf"|"sg"|"sh"|"sk"|"sl"|"sm"|"sn"|"sp"|"sq"|"sr"|"st"|"sz"|"gn"|"pn"|"ps"|"z" => "uno" ;
		 _ => "un"} ;
	       Fem => "una" 
	       } ;
	       n = Sg} ; --- uno can get wrong
    aPl_Det = {s = \\_ => "" ; n = Pl} ;
    the_Det = {s = table {
		 Fem => pre {
		   "a"|"e"|"i"|"o"|"u" => "l'" ++ BIND ;
		   _ => "la"
		   } ;
		 Masc => pre {
		   "a"|"e"|"i"|"o"|"u" => "l'" ++ BIND ;
		   "sb"|"sc"|"sd"|"sf"|"sg"|"sh"|"sk"|"sl"|"sm"|"sn"|"sp"|"sq"|"sr"|"st"|"sz"|"gn"|"pn"|"ps"|"z" => "lo" ;
		   _ => "il"
		   }
		 } ;
	       n = Sg} ; --- lo can get wrong
    thePl_Det = {s = table {
		   Fem => "le" ;
		   Masc => pre {
		     "a"|"e"|"i"|"o"|"u"|"sb"|"sc"|"sd"|"sf"|"sg"|"sh"|"sk"|"sl"|"sm"|"sn"|"sp"|"sq"|"sr"|"st"|"sz"|"gn"|"pn"|"ps"|"z" => "gli" ;
		     _ => "i"
		     }
		   } ;
		 n = Pl} ;
    
    UseN n = n ;
    
    AdjCN ap cn = {
      s = table {
	n => case ap.isPre of {
	  True => ap.s ! cn.g ! n ++ cn.s ! n ;
	  False => cn.s ! n ++ ap.s ! cn.g ! n
	  }
	};
      g = cn.g
      } ;

    PositA a = a ;

--    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;
    PrepNP prep np = {s = prep.con ! np.g ! np.n ++ np.s ! Acc} ;

    in_Prep = mkPrep "in" ;
    on_Prep = mkPrep "su" ;
    
    with_Prep = mkPrep "con" ;

    he_Pron = {
      s = table {
	Nom => "lui" ;
	Acc => pre {
	  "a"|"e"|"i"|"o"|"u"|"ho" => "l'" ++ BIND ;
	  _ => "lo"
	  }
	} ;
        g = Masc ;
	n = Sg ;
      } ;
    she_Pron = {
      s = table {
	Nom => "lei" ;
	Acc =>
	  pre {
	    "a"|"e"|"i"|"o"|"u"|"ho" => "l'" ++ BIND ;
	    _ => "la"
	  }
	};
      g = Fem ;
      n = Sg ;
      } ;
    they_Pron = {
      s = table {
	Nom => "loro" ;
	Acc => "li"
	} ; -- only masculine form at the moment. feminine form is "le"
      g = Masc ;
      n = Pl ;
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

  lin already_Adv = mkAdv "già" ;
  lin animal_N = mkN "animale" ;
  lin apple_N = mkN "mela" ;
  lin baby_N = mkN "bambino" ;
  lin bad_A = mkA "cattivo" True ;
  lin beer_N = mkN "birra" ;
  lin big_A = mkA "grande" True ;
  lin bike_N = mkN "bicicletta" "biciclette" Fem ;
  lin bird_N = mkN "uccello" ;
  lin black_A = mkA "nero" ;
  lin blood_N = mkN "sangue";
  lin blue_A = mkA "azzurro" ;
  lin boat_N = mkN "barca" ;
  lin book_N = mkN "libro" ;
  lin boy_N = mkN "ragazzo" ;
  lin bread_N = mkN "pane" ;
  lin break_V2 = mkV2 (mkV "rompere" "rompo" "rompi" "rompe" "rompiamo" "rompete"
			 "rompono" "rotto" "rompevo" "ruppi" "rompesti" "romperò"
			 "rompa" "rompiamo" "rompessi" "rompi" "rompa" "rompiamo"
			 "rompete"  "rompano" "rompente" Essere) ;
  lin buy_V2 = mkV2 (mkV "comprare") ;
  lin car_N = mkN "macchina" ;
  lin cat_N = mkN "gatto" ;
  lin child_N = mkN "bambino" ;
  lin city_N = mkN "città" "città" Fem ;
  lin clean_A = mkA "pulito" ;
  lin clever_A = mkA "furbo" ;
  lin cloud_N = mkN "nube" ;
  lin cold_A = mkA "freddo" ;
  lin come_V = mkV "venire" "vengo" "vieni" "viene" "veniamo" "venite"
	"vengono" "venuto" "venivo" "venni" "venisti" "verrò"
	"venga" "veniamo" "venissi" "vieni" "venga" " veniamo"
	"venite" "vengano" "venente" Essere ;
  lin computer_N = mkN "calculatore" ;
  lin cow_N = mkN "mucca" ;
  lin dirty_A = mkA "sporco" ;
  lin dog_N = mkN "cane" ;
  lin drink_V2 = mkV2 (mkV "bere" "bevo" "bevi" "beve" "beviamo" "bevete" "bevono" "bevuto" "bevevo" "bevvi" "bevesti" "berrò" "beva" "beviamo" "bevessi" "bevi" "beva" "beviamo" "bevete" "bevano" "bevente" Avere) ;
  lin eat_V2 = mkV2 (mkV "mangiare") ;
  lin find_V2 = mkV2 (mkV "trovare") ;
  lin fire_N = mkN "fuoco" ;
  lin fish_N = mkN "pesce" ;
  lin flower_N = mkN "fiore" ;
  lin friend_N = mkN "amico" ;
  lin girl_N = mkN "ragazza" ;
  lin good_A = mkA "buono" True ;
  lin go_V = mkV "andare" "vado" "vai" "va" "andiamo" "andata" "vanno" "andato" "andavo" "ebbi" "avesti" "andrò" "vada" "andiamo" "andassi" "va'" "vada" "andiamo" "andate" "vadano" "andante" Essere ;
  lin grammar_N = mkN "grammatica" ;
  lin green_A = mkA "verde" ;
  lin heavy_A = mkA "pesante" ;
  lin horse_N = mkN "cavallo" ;
  lin hot_A = mkA "bollente" ;
  lin house_N = mkN "casa" ;
      -- lin john_PN = mkPN "Giovanni" ;
  lin jump_V = mkV "saltare" ;
  lin kill_V2 = mkV2 "uccidere" ;
      -- -- lin know_VS = mkVS (mkV "sapere" "so" "sai" "sa" "sappiamo" "sapete" "sanno" "saputo" "sapevo" "seppi" "sapesti" "saprò" "sappia" "sappiamo" "sapessi" "sappi" "sappia" "sappiamo" "sappiate" "sappiano" "sapente" Avere);
  lin language_N = mkN "lingua" ;
  lin live_V = mkV "vivere" "vivo" "vivi" "vive" "viviamo" "vivete" "vivono" "vissuto" "vivevo" "vissi" "vivesti" "vivrò" "viva" "viviamo" "vivessi" "vivi" "viva" "viviamo" "vivete" "vivano" "vivente" Essere ;
  lin love_V2 = mkV2 (mkV "amare") ;
  lin man_N = mkN "uomo" ;
  lin milk_N = mkN "latte" ;
  lin music_N = mkN "musica" ;
  lin new_A = mkA "nuovo" True ;
  lin now_Adv = mkAdv "adesso" ;
  lin old_A = mkA "vecchio" True ;
      -- lin paris_PN = mkPN "Parigi" ;
  lin play_V = mkV "giocare" ; --giocare as in play a game
  lin read_V2 = mkV2 (mkV "leggere" "leggo" "leggi" "legge" "leggiamo" "leggete" "leggono" "letto" "leggevo" "lessi" "leggesti" "leggerò" "legga" "leggiamo" "leggessi" "leggi" "legga" "leggiamo" "leggete" "leggano" "leggente" Avere) ;
  lin ready_A = mkA "pronto" ;
  lin red_A = mkA "rosso" ;
  lin river_N = mkN "fiume" ;
  lin run_V = mkV "correre" "corro" "corri" "corre" "corriamo" "correte" "corrono" "corso" "correvo" "corsi" "corresti" "correrò" "corra" "corriamo" "corressi" "corri" "corra" "corriamo" "correte" "corrano" "corrente" Essere ;
  lin sea_N = mkN "mare" ;
  lin see_V2 = mkV2 (mkV "vedere" "vedo" "vedi" "vede" "vediamo" "vedete" "vedono" "visto" "vedevo" "vidi" "vedesti" "vedrò" "veda" "vediamo" "vedessi" "vedi" "veda" "vediamo" "vedete" "vedano" "vedente" Avere ) ;
  lin ship_N = mkN "nave" ;
  lin sleep_V = mkV "dormire" ;
  lin small_A = mkA "piccolo" True ;
  lin star_N = mkN "stella" ;
  lin swim_V = mkV "nuotare" ;
  lin teach_V2 = mkV2 (mkV "insegnare") ;
  lin train_N = mkN "treno" ;
  lin travel_V = mkV "viaggiare" ;
  lin tree_N = mkN "albero" ;
  lin understand_V2 = mkV2 (mkV "capire") ;
  lin wait_V2 = mkV2 "aspettare" ;
  lin walk_V = mkV "camminare" ;
  lin warm_A = mkA "caldo" ;
  lin water_N = mkN "acqua" ;
  lin white_A = mkA "bianco" ;
  lin wine_N = mkN "vino" ;
  lin woman_N = mkN "donna" ;
  lin yellow_A = mkA "giallo" ;
  lin young_A = mkA "giovane" True ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN =  overload {
    mkN : Str -> N   -- predictable noun
      = \n -> lin N (smartNoun n) ;
    mkN : Str -> Str -> Gender -> N  -- irregular noun
      = \sg,pl,g -> lin N (mkNoun sg pl g) ;
    } ;

  mkA = overload {
    mkA : Str -> A = \a -> lin A (smartAdjective a) ;
    mkA : Str -> Bool -> A = \a,p -> lin A (smartAdjective a ** { isPre = p }) ;
    } ;

  mkV = overload {
    mkV : Str -> V =
      \v -> lin V (smartVerb v) ;
    mkV : V -> Aux -> V =
      \v,a -> v ** {aux = a} ;
    mkV : (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21 : Str) -> Aux -> V =
      \p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,aux ->
  lin V (irregVerb p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 aux) ;
    } ;
  -- mkV2 = overload {
  --   mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
  --     = \s   -> lin V2 (smartVerb s ** {c = []}) ;
  --   mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
  --     = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
  --   mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
  --     = \v   -> lin V2 (v ** {c = []}) ;
  --   mkV2 : V -> Str -> V2     -- any verb with preposition
  --     = \v,p -> lin V2 (v ** {c = p}) ;
  --   } ;

  mkV2 = overload {
    mkV2 : Str -> V2 =
      \v -> lin V2 (smartVerb v) ** { c = emptyPreposition } ;
    mkV2 : V -> V2 =
      \v -> lin V2 (v ** { c = emptyPreposition });
    } ;
  mkAdv : Str -> Adv =
    \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep (mkPreposition s) ;

}
