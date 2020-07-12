--# -path=.:../abstract
concrete MicroLangAraMhmd of MicroLang = open MicroResAraMhmd, Prelude in {


-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------


  lincat
    Utt = {s : Str} ;

    S       = {s : Str} ;
    V       = Verb ;
    V2      = Verb2 ;
    A, AP   = Adjective ;
    N       = Noun ;
    CN      = Noun ;
    NP      = {s : Case => Str; g:Gender; n:Number; p:Person} ;
    Pron    = {s : Case => Str; g:Gender; n:Number; p:Person} ;
    Det     = {s : Case => Str; st:State; n:Number} ;
    VP      = {verb : Verb ; compl :  Str} ; ---s special case of Mini
    Comp    = Adjective ;
    Prep    = {s : Str} ;
    Adv     = {s : Str} ;




  lin

    UttS s = s ;
    UttNP np = {s = np.s ! Nom} ;

    -- NP -> VP -> S
    PredVPS np vp = {
      s = np.s ! Nom ++ vp.verb.s !Impf !agr2vform np.g np.n np.p ++ vp.compl
      } ;

    UseV v = {
      verb = v ;
      compl = [] ;
      } ;

    ComplV2 v2 np = {
      verb = v2 ;
      compl = v2.c ++ np.s !Acc  -- NP object in the accusative, preposition first
      } ;

    ---- No copula in arabic "He is good ==> Howa Jayed"
    -- UseComp comp = {
    --   verb = be_Verb ;     -- the verb is the copula "be"
    --   compl = comp.s
    --   } ;


    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;

    CompAP ap = ap ;    -- AP  -> Comp
    UsePron p = p ;     -- Pron -> NP
    UseN n = n ;        -- N -> CN
    PositA a = a ;      -- AP  -> Comp

    -- Det -> CN -> NP
    DetCN det cn = {
      s = \\c => cn.s !det.st !det.n !c;
        g = cn.g  ;
        n = det.n ;
        p = Per3  ;
      } ;

    -- The determiner is a morpho- feature of nouns
    Al_Det_01 = {s = \\_ => "" ;  st=Def   ; n=Sg} ;
    Al_Det_02 = {s = \\_ => "" ;  st=Indef ; n=Sg} ;
    Al_Det_03 = {s = \\_ => "" ;  st=Def   ; n=Dl} ;
    Al_Det_04 = {s = \\_ => "" ;  st=Indef ; n=Dl} ;
    Al_Det_05 = {s = \\_ => "" ;  st=Def   ; n=Pl} ;
    Al_Det_06 = {s = \\_ => "" ;  st=Indef ; n=Pl} ;


    -- AP : CN A
    AdjCN ap cn = {
      s = \\st,n,c => cn.s !st !n !c ++
        ap.s !st !(case n of { Sg  => Asg cn.g ;
                               Dl  => Adl cn.g c ;
                               Pl =>  Apl cn.g c cn.sp   });
      g = cn.g; sp = cn.sp ;
      } ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;
    in_Prep = {s = "في"} ;
    on_Prep = {s = "على"} ;
    with_Prep = {s = "مع"} ;

    he_Pron = {
      s = table {Nom => "هو" ; Acc => "ـه"} ;
        g=Masc;
        n=Sg;
        p=Per3;
      } ;
    she_Pron = {
      s = table {Nom => "هي" ; Acc => "ـها"} ;
        g=Fem;
        n=Sg;
        p=Per3;
      } ;
    they_Pron = {
      s = table {Nom => "هم" ; Acc => "ـهم"} ;
        g=Masc;
        n=Pl;
        p=Per3;
      } ;


-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

-- Adj
lin bad_A = mkA "_" "_" "سيء" ;
lin young_A = mkA "_" "_" "شاب" ;
lin dirty_A = mkA "_" "_" "قذر" ;
lin good_A = mkA "_" "_" "جيد" ;
lin ready_A = mkA "_" "_" "جاهز" ;
lin hot_A = mkA "_" "_" "ساخن" ;
lin warm_A = mkA "_" "_" "دافيء" ;
lin cold_A = mkA "_" "_" "بارد" ;
lin clever_A = mkA "_" "_" "ماهر" ;
lin big_A = mkA "_" "_" "كبير" ;
lin clean_A = mkA "_" "_" "نظبف" ;
lin heavy_A = mkA "_" "_" "ثقيل" ;
lin new_A = mkA "_" "_" "جديد" ;
lin old_A = mkA "_" "_" "قديم" ;
lin small_A = mkA "_" "_" "صغير" ;
lin white_A = mkA "2aFoCaL" "بيض" "أبيض";
lin red_A = mkA "2aFoCaL" "حمر" "أحمر" ;
lin yellow_A = mkA "2aFoCaL" "صفر" "أصفر" ;
lin black_A = mkA "2aFoCaL" "سود" "أسود" ;
lin blue_A = mkA "2aFoCaL" "زرق" "أزرق" ;
lin green_A = mkA "2aFoCaL" "خضر" "أخضر" ;

-- Verb
lin come_V = mkV "أتى" ;
lin jump_V = mkV "قفز" ;
lin live_V = mkV "حيى" ;
lin play_V = mkV "لعب" ;
lin run_V = mkV "جرى" ;
lin sleep_V = mkV "نام" ;
lin swim_V = mkV "سبح" ;
lin travel_V = mkV "سافر" ;
lin walk_V = mkV "مشى" ;
lin ktb_V = patternVerb "كتب" ;
lin break_V2 = mkV2 "كسر" ;
lin drink_V2 = mkV2 "شرب" ;
lin eat_V2 = mkV2 "أكل" ;
lin kill_V2 = mkV2 "قتل" ;
lin read_V2 = mkV2 "قرأ" ;
lin see_V2 = mkV2 "بصر" ;
lin teach_V2 = mkV2 "علم" ;
lin understand_V2 = mkV2 "فهم" ;
lin wait_V2 = mkV2 "ذهب" "مع" ;
lin go_V2 = mkV2 "ذهب" ;
-- lin buy_V2 = mkV2 (mkV "شرى") ;
-- lin find_V2 = mkV2 (mkV "وجد") ;
-- lin love_V2 = mkV2 (mkV "حب") ;


----- Feminame nouns that have Plural of 'STEM + ـات' -----
lin tree_N = mkN "شجرة" NoHum Fem;
lin car_N = mkN "سيارة" NoHum Fem;
lin bike_N = mkN "عجلة" NoHum Fem;
lin cow_N = mkN "بقرة" NoHum Fem;
lin fish_N = mkN "سمكة" NoHum Fem;
lin language_N = mkN "لغة" NoHum Fem;
lin apple_N = mkN "تفاحة" NoHum Fem;
lin train_N = mkN "قطار" NoHum Fem;
lin animal_N = mkN "حيوان" NoHum Fem;

----- Takseer plural from root -----
lin cloud_N = mkN "سحاب" "سحب" "FaCaAL" NoHum Fem;
lin book_N = mkN "كتاب" "كتب" "FaCaAL" NoHum Masc;
lin ship_N = mkN "سفينة" "سفن" "FaCeeLah" NoHum Fem;
lin city_N = mkN "مدينة" "مدن" "FaCeeLah" NoHum Fem;
lin river_N = mkN "نهر" "نهر" "FaCaL" NoHum Masc;
lin child_N = mkN "طفل" "طفل" "FeCol" Hum Masc;
lin boy_N = mkN "ولد" "ولد" "FaCaL" Hum Masc;

----- irregular nouns -----
lin woman_N = mkN "امرأة" "نساء" Hum Fem;
-- lin milk_N = mkN "حليب" "حليب" "حليب";
lin man_N = mkN "رجل" "رجال" Hum Masc;
lin dog_N = mkN "كلب" "كلاب" NoHum Masc;
lin sea_N = mkN "بحر" "بحار" NoHum Masc;
lin girl_N = mkN "بنت" "بنات" Hum Fem;
lin star_N = mkN "نجم" "نجوم" NoHum Masc;
lin house_N = mkN "منزل" "منازل" NoHum Masc;
lin blood_N = mkN "دم" "دماء" NoHum Masc;
lin baby_N = mkN "رضيع" "رضع" Hum Masc;
lin bird_N = mkN "طائر" "طيور" NoHum Masc;
lin boat_N = mkN "قارب" "قوارب" NoHum Masc;
lin bread_N = mkN "خبز" "خبز" NoHum Masc;
lin cat_N = mkN "قط" "قطط" NoHum Masc;
lin computer_N = mkN "حاسوب" "حواسب" NoHum Masc;
lin fire_N = mkN "نار" "نيران" NoHum Fem;
lin flower_N = mkN "وردة" "ورود" NoHum Fem;
lin friend_N = mkN "صديق" "أصدقاء" Hum Masc;
lin grammar_N = mkN "نحو" "نحو" NoHum Masc;
lin horse_N = mkN "حصان" "أحصنة" NoHum Masc;

already_Adv = mkAdv "سابقا";
now_Adv = mkAdv "الآن";
-- ---------------------------
-- -- Paradigms part ---------
-- ---------------------------

oper
  -- nouns
  mkN = overload {
    -- Fem Nouns
    mkN : Str -> Species -> Gender -> Noun
      = \n,sp,g -> lin N (femNoun n sp g) ;

    -- Takseer (broken) Plural generated from singular's pattern and root
    mkN : Str -> Str -> Str -> Species -> Gender -> Noun
      = \n,p,r,sp,g -> lin N (mkTakseer n p r sp g) ;

    -- irregular nouns
    mkN : Str -> Str -> Species -> Gender -> Noun
      = \sg,pl,sp,g -> lin N (worestNoun sg pl sp g) ;
    } ;

  -- adjectives
  mkA : Str -> Str -> Str -> Adjective
    = \p,r,a -> lin A (smartAdj p r a) ;

  -- verbs
  mkV = overload {
    mkV : Str -> Verb
      = \v -> lin V (smartVerb v)
    } ;

  mkV2 = overload {
    -- verb with direct object
    mkV2 : Str -> V2
      = \s   -> lin V2 (smartVerb s ** {c = []}) ;
    mkV2 : Str -> Str -> V2   -- verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;

  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;


}
