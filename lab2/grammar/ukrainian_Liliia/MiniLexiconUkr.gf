--# -path=.:../abstract
--# -coding=utf8

concrete MiniLexiconUkr of MiniLexicon = MiniGrammarUkr ** open MiniResUkr in {
    flags coding=utf8;

    -- Nouns
    lin animal_N = mkN "тварина";
    lin apple_N = mkN "яблуко";
    lin baby_N = mkN "немовля";
    lin beer_N = mkN "пиво";
    lin bike_N = mkN "велосипед";
    lin bird_N = mkN "птах";
    lin blood_N = mkN "кров";
    lin boat_N = mkN "човен";
    lin book_N = mkN "книга";
    lin boy_N = mkN "хлопець";
    lin bread_N = mkN "хліб";
    lin car_N = mkN "машина";
    lin cat_N = mkN "кіт";
    lin child_N = mkN "дитина";
    lin city_N = mkN "місто";
    lin cloud_N = mkN "хмара";
    lin computer_N = mkN "комп'ютер";
    lin cow_N = mkN "корова";
    lin dog_N = mkN "собака";
    lin fire_N = mkN "вогонь";
    lin fish_N = mkN "риба";
    lin flower_N = mkN "квітка";
    lin friend_N = mkN "друг";
    lin girl_N = mkN "дівчина";
    lin grammar_N = mkN "граматика";
    lin horse_N = mkN "кінь";
    lin house_N = mkN "дім";
    lin language_N = mkN "мова";
    lin man_N = mkN "чоловік";
    lin milk_N = mkN "молоко";
    lin music_N = mkN "музика";
    lin river_N = mkN "річка";
    lin sea_N = mkN "море";
    lin ship_N = mkN "корабель";
    lin star_N = mkN "зірка";
    lin train_N = mkN "поїзд";
    lin tree_N = mkN "дерево";
    lin water_N = mkN "вода";
    lin wine_N = mkN "вино";
    lin woman_N = mkN "жінка";

    --
    lin john_PN = mkPN "Джон" Masc Sg;
    lin paris_PN = mkPN "Париж" Masc Sg;

    -- adj
    lin bad_A = mkA "поганий";
    lin big_A = mkA "великий";
    lin black_A = mkA "чорний";
    lin blue_A = mkA "синій";
    lin clean_A = mkA "чистий";
    lin clever_A = mkA "розумний";
    lin cold_A = mkA "холодний";
    lin dirty_A = mkA "брудний";
    lin good_A = mkA "гарний";
    lin green_A = mkA "зелений";
    lin heavy_A = mkA "важкий";
    lin hot_A = mkA "гарячий";
    lin new_A = mkA "новий";
    lin old_A = mkA "старий";
    lin ready_A = mkA "готовий";
    lin red_A = mkA "червоний";
    lin small_A = mkA "малий";
    lin warm_A = mkA "теплий";
    lin white_A = mkA "білий";
    lin yellow_A = mkA "жовтий";
    lin young_A = mkA "молодий";

    -- v1
    lin come_V = mkV Perfective "прийти" "прийди" "приходжу" "приходиш" "приходить" "приходимо" "приходите" "приходять" "прийшов";
    lin go_V = mkV Imperfective "піти" "піди" "іду" "ідеш" "іде" "ідемо" "ідете" "ідуть" "пішов";
    lin jump_V = mkV Imperfective "стрибати" "стрибай" "стрибаю" "стрибаєш" "стрибає" "стрибаємо" "стрибаєте" "стрибають" "стрибав";
    lin live_V = mkV Imperfective "жити" "живи" "живу" "живеш" "живе" "живемо" "живете" "живуть" "жив";
    lin play_V = mkV Imperfective "грати" "грай" "граю" "граєш" "грає" "граємо" "граєте" "грають" "грав";
    lin run_V = mkV Imperfective "бігти" "біжи" "біжу" "біжиш" "біжить" "біжемо" "біжете" "біжать" "біг";
    lin sleep_V = mkV Imperfective "спати" "спи" "сплю" "спиш" "спить" "спимо" "спите" "сплять" "спав";
    lin swim_V = mkV Imperfective "плавати" "пливи" "пливу" "пливеш" "пливе" "пливемо" "пливете" "пливуть" "плавав";
    lin travel_V = mkV Imperfective "подорожувати" "подорожуй" "подорожую" "подорожєш" "подорожує" "подорожуємо" "подоржуєте" "подорожують" "подорожував";
    lin walk_V = mkV Imperfective "ходити" "ходи" "ходжу" "ходиш" "ходить" "ходимо" "ходите" "ходять" "ходив";

    -- v2
    lin break_V2 = mkV2 (mkV Imperfective "ламати" "ламай" "ламаю" "ламаєш" "ламає" "ламаємо" "ламаєте" "ламають" "ламав") Acc;
    lin buy_V2 = mkV2 (mkV Perfective "купити" "купи" "купую" "купуєш" "купує" "купуємо" "купуєте" "купують" "купив") Acc;
    lin drink_V2 = mkV2 (mkV Imperfective "пити" "пий" "п'ю" "п'єш" "п'є" "п'ємо" "п'єте" "п'ють" "пив") Acc;
    lin eat_V2 = mkV2 (mkV Imperfective "їсти" "їж" "їм" "їси" "їсть" "їмо" "їсте" "їдять" "їв") Acc;
    lin find_V2 = mkV2 (mkV Perfective "знайти" "знайди" "знаходжу" "знаходиш" "знаходить" "знаходимо" "знаходите" "знаходять" "знайшов") Acc;
    lin kill_V2 = mkV2 (mkV Imperfective "вбивати" "вбий" "вбиваю" "вбиваєш" "вбиває" "вбиваємо" "вбиваєте" "вбивають" "вбив") Acc;
    lin love_V2 = mkV2 (mkV Imperfective "любити" "люби" "люблю" "любиш" "любить" "любимо" "любите" "люблять" "любив") Acc;
    lin read_V2 = mkV2 (mkV Imperfective "читати" "читай" "читаю" "читаєш" "читає" "читаємо" "читаєте" "читають" "читав") Acc;
    lin see_V2 = mkV2 (mkV Imperfective "бачити" "бач" "бачу" "бачиш" "бачить" "бачимо" "бачите" "бачять" "бачив") Acc;
    lin teach_V2 = mkV2 (mkV Imperfective "вчити" "вчи" "вчу" "вчиш" "вчить" "вчимо" "вчите" "вчять" "вчив") Acc;
    lin understand_V2 = mkV2 (mkV Imperfective "розуміти" "розумій" "розумію" "розумієш" "розуміє" "розуміємо" "розумієте" "розуміють" "розумів") Acc;
    lin wait_V2 = mkV2 (mkV Imperfective "чекати" "чекай" "чекаю" "чекаєш" "чекає" "чекаємо" "чекаєте" "чекають" "чекав") "на" Acc;

    -- vs
    lin know_VS = mkV Imperfective "знати" "знай" "знаю" "знаєш" "знає" "знаємо" "знаєте" "знають" "знав";

    -- adv
    lin already_Adv = { s = "вже" };
    lin now_Adv = { s = "зараз" };
}