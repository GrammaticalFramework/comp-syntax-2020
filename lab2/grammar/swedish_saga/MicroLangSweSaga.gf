--# -path=.:../abstract
concrete MicroLangSweSaga of MicroLang = open MicroResSwe2, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt,

    S 
      = {s : Str} ;

    Prep
      = Preposition ; 

    V
      = Verb ;
    V2 
      = Verb2 ;
    
    VP 
      = {verb : Verb ; compl : AdjForm => Str} ;
    
    Comp,
    AP, 
    A 
      = Adjective ;
    
    
    NP,
    Pron
      = {s : Case => Str ; a : AdjForm} ;
    
    Det 
      = {s : Bool => Gender => Str ; n : Number; d : Definiteness} ;


    CN
      = Noun ** {isAdj : Bool} ;
    N  
      = Noun ;
    
    Adv 
      = Adverb ;

  lin
-- Phrase
  -- UttS      : S  -> Utt ;
    UttS s = s ;
  -- UttNP     : NP -> Utt ;
    UttNP np = {s = np.s ! Nom} ;

-- Sentence
  -- PredVPS   : NP -> VP -> S ;
   PredVPS np vp = {
     s = np.s ! Nom ++ vp.verb.s ++ vp.compl ! np.a  
    } ;


-- Verbs
    UseV s = {
      verb = s ;
      compl = \\_ => [] ;
      } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      compl = \\_ => v2.c ++ np.s ! Acc 
      } ;

    UseComp comp = { 
      verb = be_Verb ;
      compl = \\a => comp.s ! a 
      } ;

    CompAP ap = {s = \\a => ap.s ! a} ; -- ??
      

    AdvVP vp adv = -- funkar?????
       vp ** {compl = \\a => vp.compl ! a ++ adv.s} ; 

      
      
-- Noun
    DetCN det cn = {
      s = \\c => det.s ! cn.isAdj ! cn.g ++ cn.s ! det.n ! det.d ;
      a = case det.n of {Pl => AdjPl ; Sg => AdjSg cn.g det.d } 
      } ;


    UsePron p = p ; 




 
    a_Det = {
      s = table {
        isAdj => table {Utrum => "en" ; 
                       Neutrum => "ett" } 
                        } ;
    n = Sg ;
    d = Indef
     } ;
     
     
      aPl_Det = {
      s = table {
        isAdj => table { g => ""  } 
                         } ;
    n = Pl ;
    d = Indef
     } ;
     
    the_Det = {
      s = table {
        True => table {Utrum => "den" ; 
                       Neutrum => "det" } ;
        False => table {Utrum => "" ; 
                        Neutrum => "" } 
                         } ;
    n = Sg ;
    d = Def
     } ;
     
      thePl_Det = {
      s = table {
        True => table {Utrum => "de" ; 
                       Neutrum => "de" } ;
        False => table {Utrum => "" ; 
                        Neutrum => "" } 
                         } ;
    n = Pl ;
    d = Def
     } ;


     
  -- UseN      : N -> CN ;
    UseN n = {
      s = n.s ;
      g = n.g ;
      dec = n.dec ;
      isAdj = False 
    };
    
  -- AdjCN     : AP -> CN -> CN ;
    AdjCN ap cn = {
      s = \\n,d =>
      ap.s ! (case n of {Pl => AdjPl ; 
                          Sg => case d of {Def => AdjSg cn.g Def ; 
                                          Indef => AdjSg cn.g Indef}}) 
                                          ++ cn.s ! n ! d ;

      g = cn.g ;
      dec = cn.dec ;
      isAdj = True
      } ; 

-- Adjective
  -- PositA    : A  -> AP ;
    PositA a = a ;

-- Adverb
  -- PrepNP    : Prep -> NP -> Adv ;     -- in the house
    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;



-- Structural
    in_Prep = {s = "i"} ;
    on_Prep = {s = "på"} ;
    with_Prep = {s = "med"} ;

    he_Pron = {
      s = table {Nom => "han" ; Acc => "honom"} ;
      a = AdjSg Utrum Def ;
      } ;
    she_Pron = {
      s = table {Nom => "hon" ; Acc => "henne"} ;
      a = AdjSg Utrum Def ;
      } ;
    they_Pron = {
      s = table {Nom => "de" ; Acc => "dem"} ;
      a = AdjPl ;
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin
  already_Adv = mkAdv "redan" ;
  animal_N = mkN "djur" Neutrum Sixth ;
  apple_N = mkN "äpple" Neutrum Fifth ;
  baby_N = mkN "bebis" Utrum Second ;
  bad_A = mkA "dålig" ;
  beer_N = mkN "öl" Neutrum Sixth ;
  big_A = mkA "stor" ;
  bike_N = mkN "cykel" Utrum Second;
  bird_N = mkN "fågel" Utrum Second;
  black_A = mkA "svart" ;
  blood_N = mkN "blod" Neutrum Irreg;
  blue_A = mkA "blå" ;
  boat_N = mkN "båt" Utrum Second;
  book_N = mkN "bok" "boken" "böcker" "böckerna" Utrum Third ;
  boy_N = mkN "pojke" Utrum Second;
  bread_N = mkN "bröd" Neutrum Sixth ;
  break_V2 = mkV2 "krossar" ;
  buy_V2 = mkV2 "köper" ;
  car_N = mkN "bil" Utrum Second ;
  cat_N = mkN "katt" Utrum Third ;
  child_N = mkN "barn" Neutrum Sixth;
  city_N = mkN "stad" "staden" "städer" "städerna" Utrum Third ;
  clean_A = mkA "ren" ;
  clever_A = mkA "smart" ;
  cloud_N = mkN "moln" Neutrum Sixth ;
  cold_A = mkA "kall" ;
  come_V = mkV "kommer" ;
  computer_N = mkN "dator" Utrum Third ;
  cow_N = mkN "ko" Utrum Fourth ;
  dirty_A = mkA "smutsig" ;
  dog_N = mkN "hund" Utrum Second ;
  drink_V2 = mkV2 "dricker" ;
  eat_V2 = mkV2 "äter" ;
  find_V2 = mkV2 "hittar" ;
  fire_N = mkN "eld" Utrum Second ;
  fish_N = mkN "fisk" Utrum Second ;
  flower_N = mkN "blomma" Utrum First ;
  friend_N = mkN "vän" "vännen" "vänner" "vännerna" Utrum Third ;
  girl_N = mkN "flicka" Utrum First ;
  good_A = mkA "bra" ;
  go_V = mkV "åker" ;
  grammar_N = mkN "språklära" Utrum First ;
  green_A = mkA "grön" ;
  heavy_A = mkA "tung" ;
  horse_N = mkN "häst" Utrum Second ;
  hot_A = mkA "varm" ;
  house_N = mkN "hus" Neutrum Sixth ;
  -- john_PN = mkPN "John" ;
  jump_V = mkV "hoppar" ;
  kill_V2 = mkV2 "dödar" ;
  -- know_VS = mkVS (mkV "vet") ;
  language_N = mkN "språk" Neutrum Sixth ;
  live_V = mkV "lever" ;
  love_V2 = mkV2 "älskar" ;
  man_N = mkN "man" "mannen" "män" "männen" Utrum Irreg;
  milk_N = mkN "mjölk" Utrum Irreg ;
  music_N = mkN "musik" Utrum Irreg;
  new_A = mkA "ny" ;
  now_Adv = mkAdv "nu" ;
  old_A = mkA "gammal" ;
  -- paris_PN = mkPN "Paris" ;
  play_V = mkV "spelar" ;
  read_V2 = mkV2 "läser" ;
  ready_A = mkA "klar" ;
  red_A = mkA "röd" ;
  river_N = mkN "flod" Utrum Third ;
  run_V = mkV "springer" ;
  sea_N = mkN "hav" Neutrum Sixth ;
  see_V2 = mkV2 "ser" ;
  ship_N = mkN "skepp" Neutrum Sixth ;
  sleep_V = mkV "sover" ;
  small_A = mkA "liten" "små" "litet" "lilla" ; 
  star_N = mkN "stjärna" Utrum First ;
  swim_V = mkV "simmar" ;
  teach_V2 = mkV2 "undervisar" ;
  train_N = mkN "tåg" Neutrum Sixth ;
  travel_V = mkV "reser" ;
  tree_N = mkN "träd" Neutrum Sixth ;
  understand_V2 = mkV2 "förstår" ;
  wait_V2 = mkV2 "väntar" "på" ;
  walk_V = mkV "går" ;
  warm_A = mkA "varm" ;
  water_N = mkN "vatten" Neutrum Irreg ;
  white_A = mkA "vit" ;
  wine_N = mkN "vin" Neutrum Third ;
  woman_N = mkN "kvinna" Utrum First ;
  yellow_A = mkA "gul" ;
  young_A = mkA "ung" ;


}

