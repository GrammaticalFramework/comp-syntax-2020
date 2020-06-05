--# -path=.:../abstract
concrete MicroLangSwe of MicroLang = open MicroResSwe, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

	lincat
		Utt = {s : Str} ;
		
		S  = {s : Str} ;
		VP = {verb : Verb ; compl : AForm => Str} ; 
		Comp = {s : AForm => Str} ;  
		AP = Adjective ;	
		CN = Noun ** {hasAdj : Bool} ; 
		NP = {s : Case => Str ; a : AForm} ;
		Pron = {s : Case => Str ; a : AForm} ; 
		Det = {s : Bool => Gender => Str ; n : Number ; d : Species} ; 
		Prep = {s : Str} ;
		V = Verb ;
		V2 = Verb2 ;
		A = Adjective ;
		N = Noun ;
		Adv = {s : Str} ;

	lin
		UttS s = s ;
		UttNP np = {s = np.s ! Nom} ; 

		PredVPS np vp = {s = np.s ! Nom ++ vp.verb.s ! Pres ++ vp.compl ! np.a} ; -- Only presen from of the verb is used in Micro, didn't figure out how to make the paradigms part work with only one form
			
		UseV v = {
			verb = v ; 
			compl = \\_ => [] ; -- empty, and therefore independent of adjectival form
			} ;
			
		ComplV2 v2 np = {
			verb = v2 ;
			compl = \\_ => v2.c ++ np.s ! Acc -- never includes an adjective, and therefore independent of adjectival form
			} ;
			
		UseComp comp = {
			verb = be_Verb ;     
			compl = \\a => comp.s ! a 
			} ;
		
		CompAP ap = {s = \\a => ap.s ! a} ;  
			
		AdvVP vp adv =
			vp ** {compl = \\a => vp.compl ! a ++ adv.s} ; -- depending on adjectival form
			
		DetCN det cn = {
			s = \\_ => det.s ! cn.hasAdj ! cn.g ++ cn.s ! det.n ! det.d ;  -- the same independent of case
			a = case det.n of {Sg => ASg cn.g ; Pl => APl} ;		       -- form of the adjective
			} ;
			
		UsePron p = p ;
						
		a_Det = {s = \\_ => table {Utr => "en" ; Neutr => "ett"} ; n = Sg ; d = Indef} ; --the same both with and without adjectives - always depends on gender
		aPl_Det = {s = \\_,_ => "" ; n = Pl ; d = Indef} ;  -- always empty, independent of if there is an adjective and of the gender
		the_Det = {s = table {False  => \\_ => "" ; True => table {Utr => "den" ; Neutr => "det"} } ; n = Sg ; d = Def ; } ; -- If no adjective, always empty, independent of gender. If adjective, det depends on gender
		thePl_Det = {s = table {False => \\_ => "" ; True => \\_ => "de"} ; n = Pl ; d = Def} ; -- If no adjective, always empty, independent of gender. If adjective, always "de"
		
		UseN n = n ** {hasAdj = False} ;
		
		AdjCN ap cn = {
			s = \\n,d =>
				ap.s ! (case d of {Def => APl ; Indef => case n of {Pl => APl ; Sg => ASg cn.g}}) ++
				cn.s ! n ! d ;
			g = cn.g ;
			hasAdj = True ;
			} ;

		PositA a = a ;

		PrepNP prep np = {s = prep.s ++ np.s ! Acc} ; 

		in_Prep = {s = "i"} ;
		on_Prep = {s = "på"} ;
		with_Prep = {s = "med"} ;

		he_Pron = {
			s = table {Nom => "han" ; Acc => "honom"} ;
			a = ASg Utr ;
			} ;
		she_Pron = {
			s = table {Nom => "hon" ; Acc => "henne"} ;
			a = ASg Utr ;
			} ;
		they_Pron = {
			s = table {Nom => "de" ; Acc => "dem"} ;
			a = APl ;
			} ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "redan" ;
lin animal_N = mkN "djur" "djur" ;
lin apple_N = mkN "äpple" ;
lin baby_N = mkN "bebis" ;
lin bad_A = mkA "dålig" ;
lin beer_N = mkN "öl" "ölen" "öl" "ölen" Utr ;
lin big_A = mkA "stor" ;
lin bike_N = mkN "cykel" "cyklar" ;
lin bird_N = mkN "fågel" "fåglar" ;
lin black_A = mkA "svart" ;
lin blood_N = mkN "blod" "blod" ;
lin blue_A = mkA "blå" ;
lin boat_N = mkN "båt" ;
lin book_N = mkN "bok" "böcker";
lin boy_N = mkN "pojke" "pojkar" ;
lin bread_N = mkN "bröd" "bröd" ;
lin break_V2 = mkV2 "förstör" ;
lin buy_V2 = mkV2 "köper" ;
lin car_N = mkN "bil" ;
lin cat_N = mkN "katt" "katter";
lin child_N = mkN "barn" "barn" ;
lin city_N = mkN "stad" "städer" ;
lin clean_A = mkA "ren" ;
lin clever_A = mkA "klok" ;
lin cloud_N = mkN "moln" "moln" ;
lin cold_A = mkA "kall" ;
lin come_V = mkV "kommer" ;
lin computer_N = mkN "dator" "datorn" "datorer" "datorerna" Utr;
lin cow_N = mkN "ko" ;
lin dirty_A = mkA "smutsig" ;
lin dog_N = mkN "hund" ;
lin drink_V2 = mkV2 "dricker" ;
lin eat_V2 = mkV2 "äter" ;
lin find_V2 = mkV2 "hittar" ;
lin fire_N = mkN "eld" ;
lin fish_N = mkN "fisk" ;
lin flower_N = mkN "blomma" ;
lin friend_N = mkN "vän" "vännen" "vänner" "vännerna" Utr;
lin girl_N = mkN "flicka" ;
lin good_A = mkA "god" ;
lin go_V = mkV "går" ;
lin grammar_N = mkN "grammatik" "grammatiker" ;
lin green_A = mkA "grön" ;
lin heavy_A = mkA "tung" ;
lin horse_N = mkN "häst" ;
lin hot_A = mkA "het" ;
lin house_N = mkN "hus" "hus" ;
lin jump_V = mkV "hoppar" ;
lin kill_V2 = mkV2 "dödar" ;
lin language_N = mkN "språk" "språk" ;
lin live_V = mkV "lever" ;
lin love_V2 = mkV2 "älskar" ;
lin man_N = mkN "man" "mannen" "män" "männen" Utr ;
lin milk_N = mkN "mjölk" ;
lin music_N = mkN "musik" ;
lin new_A = mkA "ny" ;
lin now_Adv = mkAdv "nu" ;
lin old_A = mkA "gammal" "gammalt" "gamla" ;
lin play_V = mkV "spelar" ;
lin read_V2 = mkV2 "läser" ;
lin ready_A = mkA "klar" ;
lin red_A = mkA "röd" ;
lin river_N = mkN "flod" "floder" ;
lin run_V = mkV "springer" ;
lin sea_N = mkN "hav" "hav" ;
lin see_V2 = mkV2 (mkV "se" "ser" ) ;
lin ship_N = mkN "skepp" "skepp" ;
lin sleep_V = mkV "sover" ;
lin small_A = mkA "liten" "litet" "små" ;
lin star_N = mkN "stjärna" ;
lin swim_V = mkV "simmar" ;
lin teach_V2 = mkV2 "lär" ;
lin train_N = mkN "tåg" "tåg" ;
lin travel_V = mkV "reser" ;
lin tree_N = mkN "träd" "träd" ;
lin understand_V2 = mkV2 "förstår" ;
lin wait_V2 = mkV2 "väntar" "på" ;
lin walk_V = mkV "promenerar" ;
lin warm_A = mkA "varm" ;
lin water_N = mkN "vatten" "vattnet" "vatten" "vattnen" Neutr ;
lin white_A = mkA "vit" ;
lin wine_N = mkN "vin" "vinet" "viner" "vinerna" Neutr;
lin woman_N = mkN "kvinna";
lin yellow_A = mkA "gul" ;
lin young_A = mkA "ung" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
	mkN = overload {
		mkN : Str -> Noun   -- predictable noun
			= \n -> lin N (smartNoun n) ;
		mkN : (sg,pl : Str) -> Noun  -- plural is given
			= \sg,pl -> lin N (plurNoun sg pl) ;
		mkN : (sgi,sgd,pli,pld : Str) -> Gender -> Noun  -- all forms + gender is given
			= \sgi,sgd,pli,pld,g -> lin N (mkNoun sgi sgd pli pld g) ;
		} ;

	mkA = overload {
		mkA : Str -> A   -- predictable adjectives
		 = \s -> lin A (smartAdjective s) ;
		mkA : (utr,neutr,pl : Str) -> A  -- irregular adjectives, only "gammal" and "liten" ("liten" is not good anyway, since it has different forms in definite singular and plural)
		 = \utr,neutr,pl -> lin A (mkAdjective utr neutr pl) ;
		} ;

	mkV = overload {
		mkV : Str -> V  -- predictable verbs 
			= \s -> lin V (smartVerb s) ;
		mkV : (inf,pres : Str) -> V -- only for "se"
		= \inf,pres -> lin V (mkVerb inf pres) ;
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