--# -path=.:../abstract
concrete MicroLangDan of MicroLang = open MicroResDan, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {s : Gender => Str} ; ---s special case of Mini
    Comp = {s: Gender => Str } ;
    AP, A = {s : Gender => Str} ;
    NP = {s : Case => Str ; a : Agreement} ;
    Pron = {s : Case => Str ; a : Agreement} ;
    Det = {s : Gender => Str ; n : Number; d : Definiteness} ;
    Prep = {s : Str} ;
    V = {s : Gender => Str} ;
    V2 = {s : Gender => Str} ;
    CN, N  = Noun ;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Nom} ;

   PredVPS np vp = {
     s = np.s ! Nom ++ vp.s ! agr2vform np.a
    } ;
      


    UseV v = v ;


      
    ComplV2 v2 np = {
      s = table {
          Utrum => v2.s ! Utrum ++ np.s ! Acc ;
          Neutrum => v2.s ! Neutrum ++ np.s ! Acc
          } ;
      }; 

    
    UseComp comp = {
      s = table {
            Utrum => "er" ++ comp.s ! Utrum ;
            Neutrum => "er" ++ comp.s ! Neutrum
          } ;
      } ;
      
    CompAP ap = ap ;
      
    AdvVP vp adv =
      {s = table {
            Utrum => vp.s ! Utrum ++ adv.s ;
            Neutrum => vp.s ! Neutrum ++ adv.s
        } ;
      } ;
      
    DetCN det cn = {
      s = \\c => det.s ! cn.g ++ cn.s ! det.n ! det.d ;
      a = Agr det.n cn.g ;
      } ;
      
    UsePron p = p ;
            

    a_Det = {
      s = table {
      Utrum => "en" ;
        Neutrum => "et" } ;
    n = Sg ;
    d = Indef
     } ;


    UseN n = {
      s = n.s ;
      g = n.g ;
      adj = "" 
    } ;
        
  AdjCN ap cn = {
      s = table {n => 
                  table {d => ap.s ! cn.g ++ cn.s ! n ! d }
                  } ;
      g = cn.g
      } ; 


    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    in_Prep = {s = "i"} ;
    on_Prep = {s = "på"} ;
    with_Prep = {s = "med"} ;

    he_Pron = {
      s = table {Nom => "han" ; Acc => "ham"} ;
      a = Agr Sg Utrum ;
      } ;
    she_Pron = {
      s = table {Nom => "hun" ; Acc => "hende"} ;
      a = Agr Sg Utrum ;
      } ;
    they_Pron = {
      s = table {Nom => "de" ; Acc => "dem"} ;
      a = Agr Pl Neutrum;
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "allerede" ;
lin animal_N = mkN "dyr" Neutrum ;
lin apple_N = mkN "æble" Neutrum ;
lin baby_N = mkN "baby" Utrum ;
lin bad_A = mkA "slem" ;
lin beer_N = mkN "øl" Utrum ;
lin big_A = mkA "stor" ;
lin bike_N = mkN "cykel" Utrum ;
lin bird_N = mkN "fugl" Utrum ;
lin black_A = mkA "sort" ;
lin blood_N = mkN "blod" Utrum ;
lin blue_A = mkA "blå" ;
lin boat_N = mkN "båd" Utrum;
lin book_N = mkN "bog" Utrum ;
lin boy_N = mkN "dreng" Utrum;
lin bread_N = mkN "brød" Neutrum;
lin break_V2 = mkV2 "ødelægger" ;
lin buy_V2 = mkV2 "køber" ;
lin car_N = mkN "bil" Utrum ;
lin cat_N = mkN "kat" Utrum ;
lin child_N = mkN "barn" Neutrum ;
lin city_N = mkN "by" Utrum;
lin clean_A = mkA "ren" ;
lin clever_A = mkA "klog" ;
lin cloud_N = mkN "sky" Utrum ;
lin cold_A = mkA "kold" ;
lin come_V = mkV "kommer" ;
lin computer_N = mkN "computer" Utrum;
lin cow_N = mkN "ko" Utrum;
lin dirty_A = mkA "beskidt" ;
lin dog_N = mkN "hund" Utrum ;
lin drink_V2 = mkV2 "drikker" ;
lin eat_V2 = mkV2 "spiser" ;
lin find_V2 = mkV2 "finder" ;
lin fire_N = mkN "ild" Utrum ;
lin fish_N = mkN "fisk" Utrum ;
lin flower_N = mkN "blomst" Utrum;
lin friend_N = mkN "ven" Utrum ;
lin girl_N = mkN "pige" Utrum;
lin good_A = mkA "god" ;
lin go_V = mkV "går" ;
lin grammar_N = mkN "grammar" Utrum ;
lin green_A = mkA "grøn" ;
lin heavy_A = mkA "tung" ;
lin horse_N = mkN "hest" Utrum ;
lin hot_A = mkA "varm" ;
lin house_N = mkN "hus" Neutrum ;
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "hopper";
lin kill_V2 = mkV2 "dræber" ;
-- lin know_VS = mkVS (mkV "ved") ;
lin language_N = mkN "sprog" Neutrum;
lin live_V = mkV "lever" ;
lin love_V2 = mkV2 "elsker" ;
lin man_N = mkN "mand" Utrum ;
lin milk_N = mkN "mælk" Utrum ;
lin music_N = mkN "musik" Neutrum ;
lin new_A = mkA "ny" ;
lin now_Adv = mkAdv "nu" ;
lin old_A = mkA "gammel" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "spiller" ;
lin read_V2 = mkV2 "læser" ;
lin ready_A = mkA "klar" ;
lin red_A = mkA "rød" ;
lin river_N = mkN "flod" Utrum ;
lin run_V = mkV "løber" ;
lin sea_N = mkN "hav" Neutrum ;
lin see_V2 = mkV2 "ser" ;
lin ship_N = mkN "skib" Neutrum ;
lin sleep_V = mkV "sover" ;
lin small_A = mkA "lille" ;
lin star_N = mkN "stjerne" Utrum;
lin swim_V = mkV "svømmer" ;
lin teach_V2 = mkV2 "underviser" ;
lin train_N = mkN "tog" Neutrum ;
lin travel_V = mkV "rejser" ;
lin tree_N = mkN "træ" Neutrum ;
lin understand_V2 = mkV2 "forstår" ;
lin wait_V2 = mkV2 "venter på" ;
lin walk_V = mkV "går" ;
lin warm_A = mkA "varm" ;
lin water_N = mkN "vand" Neutrum;
lin white_A = mkA "hvid" ;
lin wine_N = mkN "vin" Utrum ;
lin woman_N = mkN "kvinde" Utrum ;
lin yellow_A = mkA "gul" ;
lin young_A = mkA "ung" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
mkN = overload {
    mkN : Str -> Gender -> Noun
      = smartN ;
    mkN : Str -> Str -> Str -> Str -> Gender -> Noun 
      = worstN
  } ;

  mkA : Str -> Adj ;
  mkA adj = {
      s = table {
            Neutrum => adj ; 
            Utrum => adj  } ;
          } ;


  mkV : Str -> {s: Gender => Str} ;
  mkV verb = {
      s = table {
            Utrum => verb ;
            Neutrum => verb 
          } ;
    } ;

  mkV2 : Str -> {s : Gender => Str} ;
  mkV2 = mkV ; 


  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

  mkPron : Str -> Gender -> {s : Str ; g : Gender} ;
  mkPron str gen = {s = str ; g = gen} ;

}
