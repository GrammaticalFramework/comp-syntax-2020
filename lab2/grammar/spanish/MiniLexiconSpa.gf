concrete MiniLexiconSpa of MiniLexicon = MiniGrammarSpa **
  open
    MiniParadigmsSpa,
    Prelude, 
    MiniResSpa
  in {
    {-
    NOTE: sometimes it is hard to distinguish between V2s with direct and 
    indirect object because some require a preposition when the object is a
    person and don't when it's something inanimate, e.g.
      "encontrar algo" (="to find something") but "encontrar a alguien" (="to
      find someone").
    I don't know how systematically this happens, so in such cases (when there
    is this problem AND the object can be both animate and inanimate), I
    arbitrarily decided to treat them as verbs that take a direct object.
    -}
    lin already_Adv = mkAdv "ya" False;
    lin animal_N = mkN "animal" M ;
    lin apple_N = mkN "manzana" ;
    lin baby_N = mkN "bebé" M ;
    lin bad_A = mkA "malo" ;
    lin beer_N = mkN "cerveza" ;
    lin big_A = mkA "grande" ;
    lin bike_N = mkN "bicicleta" ;
    lin bird_N = mkN "pájaro" ;
    lin black_A = mkA "negro" ;
    lin blood_N = mkN "sangre" F ;
    lin blue_A = mkA "azul" ;
    lin boat_N = mkN "barco" ;
    lin book_N = mkN "libro" ;
    lin boy_N = mkN "chico" ;
    lin bread_N = mkN "pan" M ;
    lin break_V2 = mkV2 (mkV "romper") ;
    lin buy_V2 = mkV2 (mkV "comprar") ;
    lin car_N = mkN "coche" M ;
    lin cat_N = mkN "gato" ;
    lin child_N = mkN "niño" ;
    lin city_N = mkN "ciudad" F ;
    lin clean_A = mkA "limpio" ;
    lin clever_A = mkA "inteligente" ;
    lin cloud_N = mkN "nube" F ;
    lin cold_A = mkA "frío" ;
    lin come_V = mkV "venir" "venido" "vengo" "vienes" "viene" "venimos" "venís" "vienen" "ven" "vengamos" "venid" "vengas" "vengamos" "vengáis" ;
    lin computer_N = mkN "ordenador" M ;
    lin cow_N = mkN "vaca" ;
    lin dirty_A = mkA "sucio" ;
    lin dog_N = mkN "perro" ;
    lin drink_V2 = mkV2 (mkV "beber") ;
    lin eat_V2 = mkV2 (mkV "comer") ;
    lin find_V2 = mkV2 (mkV "encontrar" "encontrado" "encuentro" "encuetras" "encuentra" "encontramos" "encontráis" "encuentran" "encuentra" "encontremos" "encontrad" "encuentres" "encontremos" "encontréis") ;
    lin fire_N = mkN "fuego" ;
    lin fish_N = mkN "pez" M ;
    lin flower_N = mkN "flor" F ;
    lin friend_N = mkN "amigo" ;
    lin girl_N = mkN "chica" ;
    lin good_A = mkA "bueno" ;
    lin go_V = mkV "ir" "ido" "voy" "vas" "va" "vamos" "vais" "van" "ve" "vamos" "id" "vayas" "vayamos" "vayáis" ;
    lin grammar_N = mkN "gramática" ;
    lin green_A = mkA "verde" ;
    lin heavy_A = mkA "pesado" ;
    lin horse_N = mkN "caballo" ;
    lin hot_A = mkA "caliente" ;
    lin house_N = mkN "casa" ;
    lin john_PN = mkPN "Juan" M ;
    lin jump_V = mkV "saltar" ;
    lin kill_V2 = mkV2 "matar" "a";
    lin know_VS = mkVS (mkV "saber" "sabido" "sé" "sabes" "sabe" "sabemos" "sabéis" "saben" "sabe" "sepamos" "sabed" "sepas" "sepamos" "sepáis") ; 
    lin language_N = mkN "idioma" M ;
    lin live_V = mkV "vivir" ;
    lin love_V2 = mkV2 (mkV "querer" "querido" "quiero" "quieres" "quiere" "queremos" "queréis" "quieren" "quiere" "queramos" "quered" "quieras" "queramos" "queráis") ;
    lin man_N = mkN "hombre" M ;
    lin milk_N = mkN "leche" F ;
    lin music_N = mkN "música" ;
    lin new_A = mkA "nuevo" ;
    lin now_Adv = mkAdv "ahora" True ;
    lin old_A = mkA "viejo" ;
    lin paris_PN = mkPN "París" F ;
    lin play_V = mkV "jugar" ;
    lin read_V2 = mkV2 (mkV "leer") ;
    lin ready_A = mkA "listo" ;
    lin red_A = mkA "rojo" ;
    lin river_N = mkN "río" ;
    lin run_V = mkV "correr" ;
    lin sea_N = mkN "mar" F ;
    lin see_V2 = mkV2 (mkV "ver" "visto" "veo" "ves" "ve" "vemos" "veis" "ven" "ve" "veamos" "ved" "veas" "veamos" "veáis") ; 
    lin ship_N = mkN "nave" F ;
    lin sleep_V = mkV "dormir" "dormido" "duermo" "duermes" "duerme" "dormimos" "dormís" "duermen" "duerme" "durmamos" "dormid" "duermas" "durmamos" "durmáis" ;
    lin small_A = mkA "pequeño" ;
    lin star_N = mkN "estrella" ;
    lin swim_V = mkV "nadar" ;
    lin teach_V2 = mkV2 (mkV "enseñar") ;
    lin train_N = mkN "treno" ;
    lin travel_V = mkV "viajar" ;
    lin tree_N = mkN "árbol" M ;
    lin understand_V2 = mkV2 (mkV "entender" "entendido" "entiendo" "entiendes" "entiende" "entendemos" "entendéis" "entienden" "entiende" "entendamos" "entended" "entiendas" "entendamos" "entendáis") ;
    lin wait_V2 = mkV2 "esperar" ;
    lin walk_V = mkV "caminar" ;
    lin warm_A = mkA "cálido" ;
    lin water_N = mkN "agua" ;
    lin white_A = mkA "blanco" ;
    lin wine_N = mkN "vino" ;
    lin woman_N = mkN "mujer" F ;
    lin yellow_A = mkA "amarillo" ;
    lin young_A = mkA "joven" ;
}
