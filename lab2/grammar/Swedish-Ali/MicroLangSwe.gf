--# -path=.:../abstract
concrete MicroLangSwe of MicroLang = open MicroResSwe,  Prelude  in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
   Utt = {s : Str} ;
   S = {s : Str} ;
    VP = {verb : Verb ; compl : Number => Gender => Defenitivness  => Str } ; ---s special case of Mini
    Comp  =  Adjective ;
    AP  = Adjective ;
    NP = {s : Case => Str ; n : Number ; g : Gender ; d : Defenitivness }; -- = {s : Case => Str ; a : Agreement} ;
    Pron =  {s : Case => Str ; n : Number ; g : Gender ; d : Defenitivness };
    Det  = {s :Gender => Str ; n : Number ; d : Defenitivness};
    Prep = {s : Str} ;
    V =  Verb ;
   V2  = Verb2 ;
   A = Adjective ;
    Adv   = {s : Str} ;
    --  = {s : Str} ;
      CN = Noun ;
    N = Noun ;


  lin
    UttS s = s ;
    UseN n = n ;

    UttNP np = {s = np.s ! Acc } ;

    UsePron p = {
    s = p.s ;    --  \\c =>
    n = p.n ;
    g = p.g ;
    d = p.d } ;


    PredVPS np vp = {
      s = np.s ! Nom ++ vp.verb.s ! Pres ++ vp.compl ! np.n ! np.g ! np.d  ---  here
      } ;

    ComplV2 v2 np = {
      verb = v2 ;
      compl = \\n,g,d => v2.c ++ np.s ! Acc} ; -- NP object in the accusative, preposition first --here
  --    AdvVP vp adv =
  AdvVP vp adv =
    vp ** {compl =\\n,g,d => vp.compl ! n ! g ! d  ++ adv.s} ;

    UseV v = {
        verb = v  ;
        compl = \\n,g,d => [];
        } ;

    UseComp comp = {
        verb = be_Verb ;     -- the verb is the copula "be"
        compl =\\n,g,d => comp.s ! n ! g ! Indef
    } ;


    CompAP ap = {s = \\n,g,d => ap.s ! n ! g ! d } ;

    DetCN det cn = {
    s = \\c => det.s ! cn.g ++ cn.s ! det.n ! det.d ;
    n = det.n ;
    d = det.d ;
    g = cn.g } ;



    a_Det =  {s = table { Ut => "ett" ; Neu => "en" } ; n = Sg ; d = Indef } ;
    the_Det = {s = table { Ut => "" ; Neu => "" } ; n = Sg ; d = Def } ;
    aPl_Det = {s = table { Ut => "de" ; Neu => "de" } ; n = Pl ; d = Def } ;
   thePl_Det = {s = table { Ut => "" ; Neu => "" } ; n = Pl ; d = Indef } ;

    AdjCN ap cn = {
      s = \\n,c =>  ap.s ! n ! cn.g ! c ++ cn.s ! n ! c  ;
      g = cn.g ;
    };

    PositA a = a ;
    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    in_Prep = {s = "i"} ;
    on_Prep = {s ="på"} ;
    with_Prep = {s = "med"} ;

    he_Pron = {
    s = table { Nom => "han" ; Acc => "honom"}; n = Sg ; g = Neu ; d = Indef } ;

    she_Pron = {
    s = table {Nom => "hon" ; Acc => "henne" } ; n = Sg ; g = Neu ;d =  Indef }  ;

    they_Pron = {
    s = table {Nom => "de" ; Acc => "dem" }; n = Pl ; g = Neu ; d = Indef};


-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin apple_N = mkN "äpple" "äpplen" ;
lin animal_N = mkN "djur" ;
--lin baby_N = mkN "baby" ;
lin bad_A = mkA "dålig" ;
lin beer_N = mkN "öl" "öl" ;
lin big_A = mkA "stor"  ;
lin bike_N = mkN "cykel"  ;
lin bird_N = mkN "fågel"  ;
lin black_A = mkA "svart" ;
lin blood_N = mkN "blod" "blod"  ;
lin blue_A = mkA "blå" ;
lin boat_N = mkN "båt"  ;
lin book_N = mkN "bok" "böcker"  ;
lin boy_N = mkN "pojke"  ;
lin bread_N = mkN "bröd" "bröd" ;
lin break_V2 = mkV2 (mkV "bryter" "bröt" "brutit") ;
lin buy_V2 = mkV2 (mkV "köper" "köpte" "köpt") ;
lin car_N = mkN "bil"  ;
lin cat_N = mkN "katt" ;
lin child_N = mkN "barn" "barn"  ;
lin city_N = mkN "stad" "städer"  ;
lin clean_A = mkA "ren" ;
lin clever_A = mkA "duktig" ;
lin cloud_N = mkN "moln" "moln"  ;
lin cold_A = mkA "kall" ;
lin come_V = mkV "kommer" "kom" "kommit" ;
lin computer_N = mkN "dator"  ;
lin cow_N = mkN "ko" "kor" ;
lin dirty_A = mkA "smutsig" ;
lin dog_N = mkN "hund"  ;
lin drink_V2 = mkV2 (mkV "drycker" "drack" "druckit") ;
lin eat_V2 = mkV2 (mkV "äter" "åt" "ätit") ;
lin find_V2 = mkV2 (mkV "hitter" "hittade" "hittat") ;
lin fire_N = mkN "bränd" "bränder" ;
lin fish_N = mkN "fisk" ;
lin flower_N = mkN "blomma" ;
lin friend_N = mkN "vän" ;
lin girl_N = mkN "flicka" ;
lin good_A = mkA "bra" ;
lin go_V = mkV "går" "gick" "gått" ;
lin grammar_N = mkN "grammatik";
lin green_A = mkA "grön" ;
lin heavy_A = mkA "tung" ;
lin horse_N = mkN "häst" ;
lin hot_A = mkA "varm" ;
lin house_N = mkN "hus" "hus" ;
--lin john_PN = mkPN "John" ;
lin jump_V = mkV "hoppar" ;
lin kill_V2 = mkV2 (mkV "dödar" "dödade" "dödat") ;
lin know_V2 = mkV2 (mkV "vet" "visste" "känt") ;
lin language_N = mkN "språk" "språk";
lin live_V = mkV "lever" ;
lin love_V2 = mkV2  "älskar" ;
lin man_N = mkN "man" "män";
lin milk_N = mkN "mjölk" "mjölk";
lin music_N = mkN "musik" "musik";
lin new_A = mkA "ny" ;
lin now_Adv = mkAdv "nu" ;
lin already_Adv = mkAdv "redan" ;
lin old_A = mkA "gammal" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "spelar" ;
lin read_V2 = mkV2 (mkV "läser" "läste" "läst") ;
lin ready_A = mkA "klar" ;
lin red_A = mkA "röd" ;
lin river_N = mkN "flod" ;
lin run_V = mkV "springer" "sprang" "sprungit" ;
lin sea_N = mkN "hav" ;
lin see_V2 = mkV2 (mkV "ser" "såg" "sett") ;
lin ship_N = mkN "fartyg" "fartyg" ;
lin sleep_V = mkV "sover" "sovit" "sov" ;
lin small_A = mkA "små" ;
lin star_N = mkN "stjärna" ;
lin swim_V = mkV "simmar" "simmade" "simmat" ;
lin teach_V2 = mkV2 (mkV "lär" "lärde" "lärt") ;
lin train_N = mkN "tåg" "tågen" ;
lin travel_V = mkV "reser" ;
lin tree_N = mkN "träd" "träd" ;
lin understand_V2 = mkV2 (mkV "förstår" "förstod" "förstått") ;
lin wait_V2 = mkV2 "vänta" "på" ;
lin walk_V = mkV "går" "gick" "gått" ;
lin warm_A = mkA "varmt" ;
lin water_N = mkN "vatten" ;
lin white_A = mkA "vit" ;
lin wine_N = mkN "vin" ;
lin woman_N = mkN "kvinna" ;
lin yellow_A = mkA "gul" ;
lin young_A = mkA "ung" ;

---------------------------
-- Paradigms part ---------
---------------------------
oper
  mkN = overload {
    mkN : Str ->  Noun
      = \ n -> lin N (smartN n) ;
    mkN : Str -> Str -> Noun
    = \ n,p -> lin N (irregN n p)
    } ;

  mkA = overload {
    mkA : Str -> Adjective
      = \ n -> lin N (smartA n) } ;


  mkV = overload {
    mkV : (pres : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    mkV : (pres,past,part : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \pres,past,part -> lin V (irregVerb pres past part) ;
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

  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

    mkAdv : Str -> Adv
      = \s -> lin Adv {s = s} ;

    }
