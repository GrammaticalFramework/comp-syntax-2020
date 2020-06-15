--# -path=.:../abstract
concrete MicroLangChiFang of MicroLang = open MicroResChiFang, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------
lincat

    Utt,
    S,
    Comp,   -- complement of copula                e.g. "warm"
    Pron,   -- personal pronoun                    e.g. "she"
    -- V2,     -- two-place verb                      e.g. "love"
    NP     -- noun phrase (subject or object)     e.g. "the red house"
      = {s : Str} ;

    CN, N = Noun ;
    V,V2 = Verb ;
    Det = Determiner ;
    Prep = Preposition ;
    Adv = Adverb ;
    Prep = Preposition ;
    A,AP = Adjective ;
    VP = {verb : Verb ; compl : Str} ;

lin
    
    UttS s = s ;
    UttNP np = {s = np.s} ;

    PredVPS np vp = {s = np.s ++ vp.verb.s ++ vp.compl} ;

    DetCN det cn = case det.detType of {
            DTFull Sg => {s = det.s ++ cn.c  ++ cn.s} ;  -- this house
            DTFull Pl => {s = det.s ++ xie_s ++ cn.s} ;  -- these houses
            DTNum     => {s = det.s ++ cn.c  ++ cn.s} ;  -- (these) five houses
            DTPoss    => {s = det.s          ++ cn.s}    -- our (five) houses
    } ;

    -- : V2  -> NP -> VP ;       -- love it
    ComplV2 v2 np = {
      verb = v2 ;
      compl = v2.s ++ np.s
    } ;

    -- : AP  -> Comp ;           -- small
    CompAP ap = ap ;

    -- : VP -> Adv -> VP ;       -- sleep here
    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;

    AdjCN ap cn = case ap.monoSyl of {
      True => {s = ap.s ++ cn.s ; c = cn.c} ;
      False => {s = ap.s ++ de_s ++ cn.s ; c = cn.c} 
    } ;

    PositA a = a ;
    PrepNP prep np = {s = prep.prepPre ++ np.s ++ prep.prepPost} ;

    UseN n = n ;
    UsePron p = p ;
    
    UseV v = {
      verb = v ;
      compl = [] ;
    } ;

    UseComp comp = {
      verb = noVerb ;
      compl = comp.s
    } ;

    he_Pron = mkPron "他" ;
    she_Pron = mkPron "她" ;
    they_Pron = mkPron "他们" ;

    in_Prep = mkPrep "在" "中"  ;  --- in the house, the car, etc
    on_Prep = mkPrep "在" "上"  ;
    with_Prep = mkPrep "和" "一起" (ATPlace True) ; -- "with you"

    a_Det = mkDet "一" Sg  ;
    aPl_Det = mkDet " " Sg ;
    the_Det = mkDet "这个" Sg ;
    thePl_Det = mkDet "多个" Sg ;


-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "已经" ;
lin animal_N = mkN "动物" ;
lin apple_N = mkN "苹果" ;
lin baby_N = mkN "宝贝" ;
lin bad_A = mkA "坏的" ;
lin beer_N = mkN "啤酒" ;
lin big_A = mkA "大的" ;
lin bike_N = mkN "自行车" ;
lin bird_N = mkN "鸟" ;
lin black_A = mkA "黑的" ;
lin blood_N = mkN "血" ;
lin blue_A = mkA "蓝色的" ;
lin boat_N = mkN "船" ;
lin book_N = mkN "书" ;
lin boy_N = mkN "玩具" ;
lin bread_N = mkN "面包" ;
lin break_V2 = mkV2 "打破" ; 
lin buy_V2 = mkV2 "买" ;
lin car_N = mkN "车" ;
lin cat_N = mkN "猫" ;
lin child_N = mkN "孩子" ;
lin city_N = mkN "城市" ;
lin clean_A = mkA "干净的" ;
lin clever_A = mkA "聪明的" ;
lin cloud_N = mkN "云" ;
lin cold_A = mkA "冷的" ;
-- lin paris_PN = mkPN "巴黎" ;
lin come_V = mkV "来";
lin computer_N = mkN "电脑" ;
lin cow_N = mkN "奶牛" ;
lin dirty_A = mkA "脏的" ;
lin dog_N = mkN "狗" ;
lin drink_V2 = mkV2 "喝" ;
lin eat_V2 = mkV2 "吃" ;
lin find_V2 = mkV2 "发现" ;
lin fire_N = mkN "火" ;
lin fish_N = mkN "鱼" ;
lin flower_N = mkN "花" ;
lin friend_N = mkN "朋友" ;
lin girl_N = mkN "女孩" ;
lin good_A = mkA "好的" ;
lin go_V = mkV "去" ;
lin grammar_N = mkN "语法" ;
lin green_A = mkA "绿色的" ;
lin heavy_A = mkA "重的" ;
lin horse_N = mkN "马" ;
lin hot_A = mkA "热的" ;
lin house_N = mkN "房子" ;
-- lin john_PN = mkPN "约翰" ;
lin jump_V = mkV "跳" ;
lin kill_V2 = mkV2 "杀" ;
-- lin know_VS = mkV "知道" ;
lin language_N = mkN "语言" ;
lin live_V = mkV "活";
lin love_V2 = mkV2 "爱" ;
lin man_N = mkN "男人" "个";
lin milk_N = mkN "牛奶" ;
lin music_N = mkN "音乐" ;
lin new_A = mkA "新的" ;
lin now_Adv = mkAdv "现在" ;
lin old_A = mkA "老的" ;
lin play_V = mkV "玩" ;
lin read_V2 = mkV2 "读" ;
lin ready_A = mkA "准备" ;
lin red_A = mkA "红色的" ;
lin river_N = mkN "河" ;
lin run_V = mkV "跑" ;
lin sea_N = mkN "海洋" ;
lin see_V2 = mkV2 "看" ;
lin ship_N = mkN "船" ;
lin sleep_V = mkV "睡觉" ;
lin small_A = mkA "小的" ;
lin star_N = mkN "星星" ;
lin swim_V = mkV "游泳" ;
lin teach_V2 = mkV2 "教" ;
lin train_N = mkN "火车" ;
lin travel_V = mkV "旅行" ;
lin tree_N = mkN "树" ;
lin understand_V2 = mkV2 "懂" ;
lin wait_V2 = mkV2 "等" ;
lin walk_V = mkV "走" ;
lin warm_A = mkA "温暖的" ;
lin water_N = mkN "水" ;
lin white_A = mkA "白色" ;
lin wine_N = mkN "红酒" ;
lin woman_N = mkN "女人" "个"; 
lin yellow_A = mkA "黄色的" ;
lin young_A = mkA "年轻的" ;

---------------------------
-- Paradigms part ---------
---------------------------
oper

  mkV2 = overload {
    mkV2 : Str -> V2 
      = \s -> case s of {
         v + "+" + p => lin V2 (regVerb v ** {c2 = emptyPrep ; hasPrep = False ; part = p}) ;
         v + "*" + p => lin V2 (regVerb v ** 
                                {c2 = mkPreposition p [] (getAdvType p) ; hasPrep = True ; part = []}) ;
         _ => lin V2 (regVerb s ** {c2 = emptyPrep ; hasPrep = False ; part = []})
         } ;
    mkV2 : V -> V2 
      = \v -> lin V2 (v ** {c2 = emptyPrep ; hasPrep = False ; part = []}) ;
    mkV2 : V -> Prep -> V2 
      = \v,p -> lin V2 (v ** {c2 = p ; hasPrep = True ; part = []}) ;
  } ;


  mkPron = overload {
    mkPron : (john : Str) -> Pron
      = \s -> lin Pron {s = s} ; -- normal name, in Chinese characters
  };

}
