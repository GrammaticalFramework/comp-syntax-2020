--# -path=.:../abstract

--THINGS TO FIX IN THE FUTURE:
--1 SER/ESTAR
--PredVPS (UsePron they_Pron) (UseComp (CompAP (PositA ready_A)))
--ellos son preparados

--2 some words dont have plural or indef. form
--PredVPS (DetCN a_Det (UseN sea_N)) (ComplV2 eat_V2 (UsePron she_Pron))
--un mar lo come
--PredVPS (DetCN aPl_Det (AdjCN (PositA green_A) (UseN milk_N))) (ComplV2 see_V2 (DetCN the_Det (UseN bread_N)))
--unas leches verdes ven el pan

--3 gender: I added the gender to the lexicon manually, but it could be made more efficient. 
--Depending on the edding of the word, gender is assigned (in Resource grammar).


concrete MicroLangSpa of MicroLang = open MicroResSpa, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
  --Common
    Utt = {s : Str} ;                                      -- sentence, question, word...         e.g. "be quiet"
    
  --Categories
    S  = {s : Str} ;                                       -- declarative sentence                e.g. "she lives here"

    N = Noun ;                                             -- common noun                         e.g. "house"
    CN = Noun ;
    Pron = {s : Case => Str ; g : Gender ; p : Person ; n : Number} ;   -- personal pronoun                    e.g. "she"
    NP = {s : Case => Str ; n : Number; p : Person ; g : Gender; isPron : Bool};
--    NP = {s : Case => Str ; det : Str ; n : Number; p : Person ; g : Gender; isPron : Bool};

    Det = {s : Gender => Str ; n : Number} ;               -- determiner phrase                   e.g. "those"
    Prep  = {s : Str} ;                                    -- preposition, or just case           e.g. "in", dative   
    A = Adjective;                                         -- one-place adjective                 e.g. "warm"
    AP = Adjective ;                                       -- adjectival phrase                   e.g. "warm"
    Adv = {s : Str} ;                                      -- adverbial phrase                    e.g. "in the house"

    V = {s : Number => Person => Str} ;
    V2 = Verb2 ;                                           -- two-place verb                      e.g. "love"
    VP = {verb : Verb ; compl : Gender => Number => Str; isPron : Bool; adv: Str} ; -- verb phrase                         e.g. "lives here"
    Comp = Adjective ;                 -- complement of copula                e.g. "warm"


  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;




   PredVPS np vp = { 
     s = case vp.isPron of {
       True => np.s ! Nom ++ vp.compl ! np.g ! np.n ++ vp.verb.s ! np.n ! np.p ++ vp.adv; -- "los rompre" - instead of "rompe los"
       False => np.s ! Nom ++ vp.verb.s ! np.n ! np.p ++ vp.compl ! np.g ! np.n ++ vp.adv
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
      verb = be_Verb ;     -- the verb is the copula "be"
      compl = \\g,n => comp.s ! g ! n ;
      isPron = False ;
      adv = []
      } ;
    
  
    CompAP  ap = ap ;

    AdvVP vp adv =
      vp ** {adv = adv.s} ;

    DetCN det cn = {
      s = \\c => det.s ! cn.g ++ cn.s ! det.n ;
      n = det.n ;
      g = cn.g ;
      p = P3 ;
      isPron = False
      } ;

    UsePron p = p ** {isPron = True} ;     
           
    a_Det = {
     s = table {Masc => "un"; Fem => "una"};
     n = Sg;
     } ; 
    aPl_Det = {
     s = table {Masc => "unos"; Fem => "unas"}; 
     n = Pl;
     } ;
    the_Det = {
     s = table {Masc => "el"; Fem => "la"}; 
     n = Sg;
     } ;
    thePl_Det = {
     s = table {Masc => "los"; Fem => "las"}; 
     n = Pl;
     } ;
    
    UseN n = n ** {isPron = False} ;    
    

    AdjCN ap cn = {s = \\n => cn.s ! n ++ ap.s ! cn.g ! n ; g = cn.g };

    PositA a = a ;

    PrepNP prep np = {
      s = prep.s ++ np.s ! Nom; -- con ella: nominative
      isPron = False } ;

    in_Prep = {s = "en"} ;
    on_Prep = {s = "en"} ;
    with_Prep = {s = "con"} ;

    he_Pron = {
      s = table {Nom => "él" ; Acc => "lo"} ;
      g = Masc ;
      p = P3 ; 
      n = Sg ;
      } ;
    she_Pron = {
      s = table {Nom => "ella" ; Acc => "la"} ;
      g = Fem ;
      p = P3 ;
      n = Sg ;
      } ;
    they_Pron = {
      s = table {Nom => "ellos" ; Acc => "los"} ;
      g = Masc ;
      p = P3 ;
      n = Pl ;
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "ya" ;
lin animal_N = mkN "animal" Masc ;
lin apple_N = mkN "manzana" Fem;
lin baby_N = mkN "bebé" Masc;
lin bad_A = mkA "malo" ;
lin beer_N = mkN "cerveza" Fem;
lin big_A = mkA "grande" ;
lin bike_N = mkN "bicicleta" Fem;
lin bird_N = mkN "pájaro" Masc;
lin black_A = mkA "negro" ;
lin blood_N = mkN "sangre" Fem;
lin blue_A = mkA "azul" ;
lin boat_N = mkN "barco" Masc;
lin book_N = mkN "libro" Masc;
lin boy_N = mkN "chico" Masc;
lin bread_N = mkN "pan" Masc;
lin break_V2 = mkV2 (mkV "romper") ;
lin buy_V2 = mkV2 (mkV "comprar") ;
lin car_N = mkN "coche" Masc;
lin cat_N = mkN "gato" Masc;
lin child_N = mkN "niño" Masc;
lin city_N = mkN "ciudad" Fem;
lin clean_A = mkA "limpio" ;
lin clever_A = mkA "inteligente" ;
lin cloud_N = mkN "nube" Fem;
lin cold_A = mkA "frio";
lin come_V = mkV "venir";
lin computer_N = mkN "ordenador" Masc;
lin cow_N = mkN "vaca" Fem;
lin dirty_A = mkA "sucio" ;
lin dog_N = mkN "perro" Masc;
lin drink_V2 = mkV2 (mkV "beber") ;
lin eat_V2 = mkV2 (mkV "comer" );
lin find_V2 = mkV2 (mkV "encontrar") ;
lin fire_N = mkN "fuego" Masc;
lin fish_N = mkN "pez" Masc;
lin flower_N = mkN "flor" Fem;
lin friend_N = mkN "amigo" Masc;
lin girl_N = mkN "chica" Fem;
lin good_A = mkA "bueno" ;
lin go_V = mkV "ir" "voy" "vas" "va" "vamos" "vais" "van" ;
lin grammar_N = mkN "gramática" Fem;
lin green_A = mkA "verde" ;
lin heavy_A = mkA "pesado" ;
lin horse_N = mkN "caballo" Masc;
lin hot_A = mkA "caliente" ;
lin house_N = mkN "casa" Fem;
lin jump_V = mkV "saltar" ;
lin kill_V2 = mkV2 (mkV "matar" );
lin language_N = mkN "idioma" Masc;
lin live_V = mkV "vivir" ;
lin love_V2 = mkV2 (mkV "querer" "quiero" "quieres" "quiere" "queremos" "queréis" "quieren") ;
lin man_N = mkN "hombre" Masc;
lin milk_N = mkN "leche" Fem;
lin music_N = mkN "musica" Fem;
lin new_A = mkA "nuevo" ;
lin now_Adv = mkAdv "ahora" ;
lin old_A = mkA "viejo" ;
lin play_V = mkV "jugar" ;
lin read_V2 = mkV2 (mkV "leer") ;
lin ready_A = mkA "preparado" ;
lin red_A = mkA "rojo" ;
lin river_N = mkN "río" Masc;
lin run_V = mkV "correr" ;
lin sea_N = mkN "mar" Masc;
lin see_V2 = mkV2 (mkV "ver" "veo" "ves" "ve" "vemos" "veis" "ven") ;
lin ship_N = mkN "barco" Masc;
lin sleep_V = mkV "dormir" ;
lin small_A = mkA "pequeño" ;
lin star_N = mkN "estrella" Fem;
lin swim_V = mkV "nadar" ;
lin teach_V2 = mkV2 (mkV "enseñar") ;
lin train_N = mkN "tren" Masc;
lin travel_V = mkV "viajar" ;
lin tree_N = mkN "árbol" Masc;
lin understand_V2 = mkV2 (mkV "entender") ;
lin wait_V2 = mkV2 (mkV "esperar");
lin walk_V = mkV "pasear" ;
lin warm_A = mkA "tibio" ;
lin water_N = mkN "agua" Masc;
lin white_A = mkA "blanco" ;
lin wine_N = mkN "vino" Masc;
lin woman_N = mkN "mujer" Fem;
lin yellow_A = mkA "amarillo" ;
lin young_A = mkA "joven" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN : Str -> Gender -> Noun 
      = \n,g -> lin N (smartNoun n g) ;

  mkA : Str -> A = \a -> lin A (smartAdj a) ; 


  mkV = overload {
    mkV  : (Inf: Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    mkV : (Inf,compro,compras,compra,compramos,comprais,compran : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \Inf,compro,compras,compra,compramos,comprais,compran -> lin V (mkVerb Inf compro compras compra compramos comprais compran) ;
    } ;


  mkV2 = overload {
  mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
      = \v   -> lin V2 (smartVerb v ** {c = \\g,n => []}) ;
   mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = \\g,n => p}) ;
   mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
     = \v   -> lin V2 (v ** {c =  \\g,n => []}) ;
   mkV2 : V -> Str -> V2     -- any verb with preposition
     = \v,p -> lin V2 (v ** {c = \\g,n => p}) ;
   } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}