--# -path=.:../abstract
concrete MicroLangSwe of MicroLang = open MicroResSwe in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
   Utt = {s : Str} ;
  --  S = {s : Str} ;
  --  VP, -- = {verb : Verb ; compl : Str} ; ---s special case of Mini
  --  Comp, -- = {s : Str} ;
  --  AP, -- = Adjective ;
  --  NP = {s : Case => Str ; n : Number; p : Defenitivness ; g : Gender}; -- = {s : Case => Str ; a : Agreement} ;
  --  Pron, -- = {s : Case => Str ; a : Agreement} ;
  --  Det = {s : Number => Defenitivness => Str ; g : gender } ;-- = {s : Str ; n : Number} ;
  --  Prep, -- = {s : Str} ;
  --  V, -- = Verb ;
  --  V2, -- = Verb2 ;
  --  A = {s : Str} ;      -- = Adjective ;
  --  Adv  -- = {s : Str} ;
    --  = {s : Str} ;
    --  CN,
    N = Noun ;
--  oper
--  mkN : Str -> {s:Str ; g : Gender };
--  mkN  str = {s = str} ;

  lin
    UttS s = s ;
  --  UseN n = n ;
  --  UttNP np = np ;

  --  PredVPS np vp = {
  --    s = np.s ++ vp.s
  --    } ;

  --  UseV v = v ;

  --  ComplV2 v2 np = {
  --    s = v2.s ++ np.s  -- NP object in the accusative, preposition first
  --    } ;

--    UseComp comp = {
--      s = "be" ++ comp.s      -- the verb is the copula
--      } ;

  --  CompAP ap = ap ;

  --  AdvVP vp adv = {
  --  s = vp.s ++ adv.s} ;


  --  UsePron pron = pron ;

  --  en_Det = {s = "en"} ;

  --  ett_Det = {s = "ett"} ;

  --  DetNeu = {s = table { Sg => table { Indef => "en" ; Defi => "den" };
  --                        Pl => table { Indef => "" ; Defi   => "de" } ; g = Neu } ;

  --  DetUt =  {s = table { Sg => table { Indef => "ett" ; Defi => "det" };
  --                        Pl => table { Indef => " "   ; Defi => "de" } ; g = Ut}  ;

  --  DetDfSg = {s = table { Ut => "det" ; Neu => "den" ; n = Sg ; d = Defi } };
  --  Detpl = {s = table { Ut => "de" ; Neu => "de" ; n = Pl ; d = Indef} };
    --DetCN det cn = {s = det.s ++ cn.s ! Pl} ;

  --  UseN n = n ;
  --  AdjCN ap cn = {s = ap.s ++ cn.s ;} ;
  --  PositA a = a ;
  --  PrepNP prep np = {s = prep.s ++ np.s} ;

  --  in_prep = mkPrep "in" ;
--    on_prep = mkPrep "on" ;
--    with_Prep = mkPrep "with" ;
--    he_pron = mkPron "he" ;
--    she_pron = mkPron "she" ;
--    they_pron = mkPron "they" ;
  --  a_Det = {s = pre {"a"|"e"|"i"|"o" => "an" ; _ => "a"} ; n = Sg} ; --- a/an can get wrong
  --  aPl_Det = {s = "" ; n = Pl} ;
  --  the_Det = {s = "the" ; n = Sg} ;
  --  thePl_Det = {s = "the" ; n = Pl} ;

  --  UseN n = n ;

  --  AdjCN ap cn = {
    --  s = table {n => ap.s ++ cn.s ! n}
    --  } ;

  --  PositA a = a ;

  --  PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

  --  in_Prep = {s = "in"} ;
  --  on_Prep = {s = "on"} ;
  --  with_Prep = {s = "with"} ;

  --  he_Pron = {
  --    s = table {Nom => "he" ; Acc => "him"} ;
  --    a = Agr Sg ;
  --    } ;
  --  she_Pron = {
  --    s = table {Nom => "she" ; Acc => "her"} ;
  --    a = Agr Sg ;
  --    } ;
  --  they_Pron = {
  --    s = table {Nom => "they" ; Acc => "them"} ;
  --    a = Agr Pl ;
  --    } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

--lin apple_N = mkN "äpple" "äpplen" Ut;
    --baby_N = mkN "baby" ;
--lin bad_A = mkA "dålig" ;
--lin beer_N = mkN "öl" "öl" Neu;
--lin big_A = mkA "stor"  ;
lin bike_N = mkN "cykel"  ;
lin bird_N = mkN "fågel"  ;
--lin black_A = mkA "svart" ;
--lin blood_N = mkN "blod" "blod" Ut ;
--lin blue_A = mkA "blå" ;
lin boat_N = mkN "båt"  ;
--lin book_N = mkN "bok" "böcker" Neu ;
lin boy_N = mkN "pojke"  ;
--lin bread_N = mkN "bröd" "bröd" Ut ;
--lin break_V2 = mkV2 (mkV "break" "broke" "broken") ;
--lin buy_V2 = mkV2 (mkV "buy" "bought" "bought") ;
lin car_N = mkN "bil"  ;
lin cat_N = mkN "katt" ;
--lin child_N = mkN "barn" "barn" Ut ;
--lin city_N = mkN "stad" "städer" Neu ;
--lin clean_A = mkA "ren" ;
--lin clever_A = mkA "duktig" ;
--lin cloud_N = mkN "moln" "moln" Ut ;
--lin cold_A = mkA "kallt" ;
--lin come_V = mkV "come" "came" "come" ;
lin computer_N = mkN "dator"  ;
--lin cow_N = mkN "ko" "kor" ;
--lin dirty_A = mkA "smutsig" ;
lin dog_N = mkN "hund"  ;
--lin drink_V2 = mkV2 (mkV "drink" "drank" "drunk") ;
--lin eat_V2 = mkV2 (mkV "eat" "ate" "eaten") ;
--lin find_V2 = mkV2 (mkV "find" "found" "found") ;
--lin fire_N = mkN "bränd" "bränder" Ut;
lin fish_N = mkN "fisk" ;
lin flower_N = mkN "blomma" ;
lin friend_N = mkN "vän" ;
lin girl_N = mkN "flicka" ;
--lin good_A = mkA "bra" ;
--lin go_V = mkV "gå" "went" "gone" ;
lin grammar_N = mkN "grammatik";
--lin green_A = mkA "grön" ;
--lin heavy_A = mkA "tung" ;
lin horse_N = mkN "häst" ;
--lin hot_A = mkA "varm" ;
--lin house_N = mkN "hus" "hus" Ut;
-- lin john_PN = mkPN "John" ;
--lin jump_V = mkV "hoppa" ;
--lin kill_V2 = mkV2 "dödar" ;
-- lin know_VS = mkVS (mkV "know" "knew" "known") ;
--lin language_N = mkN "språk" "språk" Ut;
--lin live_V = mkV "leva" ;
--lin love_V2 = mkV2  "älskar" ;
--lin man_N = mkN "man" "män" Neu;
--lin milk_N = mkN "mjölk" "mjölk" Neu;
--lin music_N = mkN "musik" "musik" Neu;
--lin new_A = mkA "ny" ;
--lin now_Adv = mkAdv "nu" ;
--lin old_A = mkA "gammal" ;
-- lin paris_PN = mkPN "Paris" ;
--lin play_V = mkV "spela" ;
--lin read_V2 = mkV2 (mkV "read" "read" "read") ;
--lin ready_A = mkA "redo" ;
--lin red_A = mkA "röd" ;
lin river_N = mkN "flod" ;
--lin run_V = mkV "run" "ran" "run" ;
lin sea_N = mkN "hav" ;
--lin see_V2 = mkV2 (mkV "see" "saw" "seen") ;
--lin ship_N = mkN "fartyg" "fartyg" ;
--lin sleep_V = mkV "sleep" "slept" "slept" ;
--lin small_A = mkA "små" ;
lin star_N = mkN "stjärna" ;
--lin swim_V = mkV "swim" "swam" "swum" ;
--lin teach_V2 = mkV2 (mkV "teach" "taught" "taught") ;
--lin train_N = mkN "tåg" "tågen" Ut ;
--lin travel_V = mkV "resa" ;
--lin tree_N = mkN "träd" "träd" Ut;
--lin understand_V2 = mkV2 (mkV "understand" "understood" "understood") ;
--lin wait_V2 = mkV2 "vänta" "på" ;
--lin walk_V = mkV "gå" ;
--lin warm_A = mkA "varmt" ;
lin water_N = mkN "vatten" ;
--lin white_A = mkA "vit" ;
lin wine_N = mkN "vin" ;
lin woman_N = mkN "kvinna" ;
--lin yellow_A = mkA "gul" ;
--lin young_A = mkA "ung" ;

---------------------------
-- Paradigms part ---------
---------------------------
oper
  mkN = overload {
    mkN : Str ->  Noun
      = \ n -> lin N (smartN n)} ;

  -- mkN : (sgIndef, sgDefi, plIndef, plDefi: Str ) ->  Noun
  --    = \ sgIndef, sgDefi, plIndef, plDefi  -> lin N (mkN sgIndef sgDefi plIndef plDefi ) ;
  --  mkN : (sgIndef, sgDefi, plIndef, plDefi : Str) -> Noun
  --     = \sgIndef, sgDefi, plIndef, plDefi -> lin N (irregN sgIndef sgDefi plIndef plDefi)



    }
