--# -path=.:../abstract
concrete MicroLangSpaAndrea of MicroLang = open MicroResSpaAndrea, Prelude in {


-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
  
-- Common
    Utt = {s : Str} ;

-- Cat
    S  = {s : Str} ;      -- declarative sentence                e.g. "she lives here"
    VP = {verb : Verb ; compl : Gender => Number => Str ; isPron : Bool ; adv: Str} ; 
    Comp = Adjective ;    -- complement of copula                e.g. "warm"
    AP = Adjective ;      -- adjectival phrase                   e.g. "warm"
    CN = Noun ;           -- common noun (without determiner)    e.g. "red house"
    NP = {s : Case => Str ; p : Person ; g : Gender ; n : Number ; isPron : Bool} ;    -- noun phrase (subject or object)     e.g. "the red house"
    Det = {s : Gender => Str ; n : Number} ;    -- determiner phrase                   e.g. "those"
   

    Prep = {s : Str} ;    -- preposition, or just case           e.g. "in", dative
    V = Verb ;            -- one-place verb                      e.g. "sleep" 
    V2 = Verb2 ;          -- two-place verb                      e.g. "love"
    A = Adjective ;       -- one-place adjective                 e.g. "warm"
    N = Noun ;            -- common noun                         e.g. "house"
    Pron = {s : Case => Str ; p : Person ; g : Gender ; n : Number} ;   -- personal pronoun         e.g. "she"
    Adv = {s : Str} ;     -- adverbial phrase                    e.g. "in the house"




  lin
-- Phrase
    UttS s = s ;                           -- he walks
    UttNP np = {s = np.s ! Acc } ;         -- he

-- Sentence
    PredVPS np vp = { 
      s = case vp.isPron of {
        True => np.s ! Nom ++ vp.compl ! np.g ! np.n ++ vp.verb.s ! agr_vform np.n ++ vp.adv; -- "los rompre" - instead of "rompe los"
        False => np.s ! Nom ++ vp.verb.s ! agr_vform np.n ++ vp.compl ! np.g ! np.n ++ vp.adv
        }
      } ;

    
-- Verb
    UseV v = {              -- sleep
      verb = v ;
      compl = \\g,n => [] ;
      isPron = False ;
      adv = []
      } ;            

    ComplV2 v2 np = {       -- love it
      verb = v2 ;
      compl = \\g,n => v2.c ! g ! n ++ np.s ! Acc ;
      isPron = np.isPron ;
      adv = []
      } ;

    UseComp comp = {        -- be small
      verb = be_Verb ;      -- the verb is the copula "be" ("ser")
      compl = \\g,n => comp.s ! g ! n ;
      isPron = False ;
      adv = []
      } ;    
    
    CompAP ap = ap ;           -- small

    
    AdvVP vp adv =
      vp ** {adv = adv.s} ; 
   

-- Noun
    DetCN det cn = {      -- the man
     s = \\c => det.s ! cn.g ++ cn.s ! det.n ;
     n = det.n ; 
     g = cn.g ; 
     p = P3 ; 
     isPron = False      
     } ;

    a_Det = {                       -- indefinite singular ---s
      s = table {Masc => "un"; Fem => "una"};
      n = Sg;
      } ; 

    aPl_Det = {                     -- indefinite plural   ---s
      s = table {Masc => "unos"; Fem => "unas"}; 
      n = Pl;
      } ;

    the_Det = {                     -- definite singular   ---s
      s = table {Masc => "el"; Fem => "la"}; 
      n = Sg;
      } ;
    
    thePl_Det = {                   -- definite plural     ---s
      s = table {Masc => "los"; Fem => "las"}; 
      n = Pl;
      } ;
    
    UseN n = n ;      -- house 
    
    AdjCN ap cn = {
      s = \\n => cn.s ! n ++ ap.s ! cn.g ! n ; 
      g = cn.g
      };

-- Adjective
    PositA a = a ;      -- warm

-- Adverb
    PrepNP prep np = {
      s = prep.s ++ np.s ! Nom; 
      isPron = False 
      } ;
-- EN: with them (prep + np(acc))
-- SP: con ellos (prep + np(nom))

-- Structural
    in_Prep = {s = "en"} ;
    on_Prep = {s = "en"} ;
    with_Prep = {s = "con"} ;

    he_Pron = {
      s = table {Nom => "él" ; Acc => "lo"} ;
      p = P3 ;
      g = Masc ; 
      n = Sg ;
      } ;
  
    she_Pron = {
      s = table {Nom => "ella" ; Acc => "la"} ;
      p = P3 ;
      g = Fem ;
      n = Sg ;
      } ;
    
    they_Pron = { -- only using third person plural FEMININE
      s = table {Nom => "ellas" ; Acc => "las"} ;
      p = P3 ;
      g = Fem ;
      n = Pl ;
      } ;



-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "ya" ;
lin animal_N = mkN "animal" ;
lin apple_N = mkN "manzana" ;
lin baby_N = mkN "bebé" ;
lin bad_A = mkA "malo" ;
lin beer_N = mkN "cerveza" ;
lin big_A = mkA "grande" ;
lin bike_N = mkN "bicicleta" ;
lin bird_N = mkN "pájaro" ;
lin black_A = mkA "negro" ;
lin blood_N = mkN "sangre" ;
lin blue_A = mkA "azul" ;
lin boat_N = mkN "bote" ;
lin book_N = mkN "libro" ;
lin boy_N = mkN "chico" ;
lin bread_N = mkN "pan" ;
lin break_V2 = mkV2 (mkV "romper") ;
lin buy_V2 = mkV2 (mkV "comprar") ;
lin car_N = mkN "coche" ;
lin cat_N = mkN "gato" ;
lin child_N = mkN "niño" ;
lin city_N = mkN "ciudad" ;
lin clean_A = mkA "limpio" ;
lin clever_A = mkA "inteligente" ;
lin cloud_N = mkN "nube" ;
lin cold_A = mkA "frío" ;
lin come_V = mkV "venir";
lin computer_N = mkN "ordenador" ;
lin cow_N = mkN "vaca" ;
lin dirty_A = mkA "sucio" ;
lin dog_N = mkN "perro" ;
lin drink_V2 = mkV2 (mkV "beber") ;
lin eat_V2 = mkV2 (mkV "comer") ;
-- lin find_V2 = mkV2 (mkV "encontrar") ;
lin fire_N = mkN "fuego" ;
lin fish_N = mkN "pez";
lin flower_N = mkN "flor" ;
lin friend_N = mkN "amigo" ;
lin girl_N = mkN ("chica" | "niña");
lin good_A = mkA "bueno"  ;
lin go_V = mkV "ir" "voy" "vas" "va" "vamos" "vais" "van" ;
lin grammar_N = mkN "gramática" ;
lin green_A = mkA "verde" ;
lin heavy_A = mkA "pesado" ;
lin horse_N = mkN "caballo" ;
lin hot_A = mkA "caliente" ;
lin house_N = mkN "casa" ;
-- lin john_PN = mkPN "Juan" ;
lin jump_V = mkV "saltar" ;
lin kill_V2 = mkV2 (mkV "matar") ;
-- lin know_VS = mkV2 (mkV "conocer") ;
lin language_N = mkN ("idioma" | "lenguaje");
lin live_V = mkV "vivir" ;
lin love_V2 = mkV2 (mkV "amar") ;
lin man_N = mkN "hombre" ;
lin milk_N = mkN "leche" ;
lin music_N = mkN "música" ;
lin new_A = mkA "nuevo" ;
lin now_Adv = mkAdv "ahora" ;
lin old_A = mkA ("viejo" | "antiguo") ;
-- lin paris_PN = mkPN "París" ;
lin play_V = mkV "jugar" ;
lin read_V2 = mkV2 (mkV "leer") ;
lin ready_A = mkA ("listo" | "preparado") ;
lin red_A = mkA "rojo" ;
lin river_N = mkN "río" ;
lin run_V = mkV "correr" ;
lin sea_N = mkN "mar" ;
lin see_V2 = mkV2 (mkV "ver") ;
lin ship_N = mkN "barco" ;
lin sleep_V = mkV "dormir" ;
lin small_A = mkA "pequeño" ;
lin star_N = mkN "estrella" ;
lin swim_V = mkV "nadar" ;
lin teach_V2 = mkV2 (mkV "enseñar") ;
lin train_N = mkN "tren" ;
lin travel_V = mkV "viajar" ;
lin tree_N = mkN "árbol" ;
lin understand_V2 = mkV2 (mkV "entender") ;
lin wait_V2 = mkV2 "esperar" ;
lin walk_V = mkV ("caminar" | "pasear") ;
lin warm_A = mkA ("cálido" | "templado") ;
lin water_N = mkN "agua" ;
lin white_A = mkA "blanco" ;
lin wine_N = mkN "vino" ;
lin woman_N = mkN "mujer" ;
lin yellow_A = mkA "amarillo" ;
lin young_A = mkA "joven" ;



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
  mkA : Str -> A = 
  \a -> lin A (smartAdj a) ;
  } ;


mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    mkV : (inf,P1sg,P2sg,P3sg,P1pl,P2pl,P3pl : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \inf,P1sg,P2sg,P3sg,P1pl,P2pl,P3pl -> lin V (mkVerb inf P1sg P2sg P3sg P1pl P2pl P3pl) ;
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


--THINGS TO IMPROVE IN THE FUTURE:
-- Adding adjective comparative forms:
  -- bueno - mejor

-- Adding proper nouns

-- Negation: "el niño no rompe el coche"

-- Make a specific case for nouns that are feminine but start with 'a'.
-- That is: agua (water) / alma (soul)...
-- There is a grammatical rule that states that, feminine singular nouns starting with an 'a' 
-- should be precedeed by a masculine singular definine article ('el'). 
-- The reason for this rule is merely phonological, ie. it sounds odd having the same two vowels 
-- pronounced one after the other: 
  -- Ex.: *la agua / el agua
-- However, the gender of the noun doesn't not change even though the 
-- definite article is masculine. 
-- For the plural form of the words, the feminine form is used:
 -- Ex: las almas (the souls)

