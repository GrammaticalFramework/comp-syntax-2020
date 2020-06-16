--# -path=.:../abstract
--# -coding=utf8

concrete MiniGrammarUkr of MiniGrammar = open MiniResUkr, Prelude in {
    flags coding=utf8;

    -- Common
    lincat
        Utt = {s : Str};    -- sentence, question, word...         e.g. "be quiet"
        Pol = UkrPolarity ; -- polarity                            e.g. positive, negative
        Temp = Temporality; -- temporal features                   e.g. present, anterior

    -- Cat
    lincat
        Imp = Imperative;         -- imperative                          e.g. "walk", "don't walk"
        S = Sentence;             -- declarative sentence                e.g. "she lives here"
        QS = QSentence;           -- question sentence                   e.g. "does she live here"
        Cl = Clause;              -- declarative clause, with all tenses e.g. "she looks at this"
        QCl = Clause;             -- question clause                     e.g. "does she look at this"
        VP = VerbPhrase;          -- verb phrase                         e.g. "lives here"
        Comp = CopCompl;          -- complement of copula                e.g. "in trouble"
        AP = Adjective;           -- adjectival phrase                   e.g. "very warm"
        CN = CommonNoun;          -- common noun (without determiner)    e.g. "red house"
        NP = NounPhrase;          -- noun phrase (subject or object)     e.g. "the red house"
        IP = InterrogativePhrase; -- interrogative phrase                e.g. "who"
        Pron = Pronoun;           -- personal pronoun                    e.g. "she"
        Det = Determiner;         -- determiner phrase                   e.g. "those"
        Conj = { s : Str };       -- conjunction                         e.g. "and"
        Prep = { s : Str };       -- preposition, or just case           e.g. "in", dative
        V = Verb;                 -- one-place verb                      e.g. "sleep"
        V2 = Verb2;               -- two-place verb                      e.g. "love"
        VS = Verb;                -- sentence-complement verb            e.g. "know"
        VV = Verb;                -- verb-phrase-complement verb         e.g. "want"
        A = Adjective;            -- one-place adjective                 e.g. "warm"
        N = Noun;                 -- common noun                         e.g. "house"
        PN = PropNoun;            -- proper name                         e.g. "Paris"
        Adv = Adverb;             -- adverbial phrase                    e.g. "in the house"
        IAdv = Adverb;            -- interrogative adverbial             e.g. "where"

    -- Phrase
    lin
        UttS s = s;                                    -- John walks
        UttQS s = s;                                   -- does John walk
        UttNP np = {s = np.s ! PF Nom NonPoss};        -- John
        UttAdv adv = adv;                              -- in the house
        UttIAdv iadv = iadv;                           -- why
        UttImpSg pol imp = { s = imp.s ! pol.isTrue }; -- (do not) walk

    -- Sentence
    lin
        UseCl = UseClause;            -- John has not walked
        UseQCl = UseClause;           -- has John walked
        PredVP = UsePredVP;           -- John walks / John does not walk
        QuestCl cl = cl;              -- does John (not) walk
        QuestVP = QuestionVerbPhrase; -- who does (not) walk
        ImpVP = ImpVerbPhrase;        -- walk / do not walk

    -- Verb
    lin
        UseV = UseVerb;           -- sleep
        ComplV2 = UseComplV2;     -- love it
        ComplVS = UseComplVS;     -- know that it is good
        ComplVV = UseComplVV;     -- want to be good
        UseComp =  UseComplement; -- be small
        CompAP = UseCompAP;       -- small
        CompNP = UseCompNP;       -- a man
        CompAdv = UseCompAdv;     -- in the house
        AdvVP = UseAdvVP;         -- sleep here

    -- Noun
    lin
        DetCN = DeterminerCN;         -- the man
        UsePN = UsePropNoun;          -- John
        UsePron p = p;                -- he
        MassNP = MassNounPhrase;      -- milk
        a_Det = a_Determiner;         -- indefinite singular ---s
        aPl_Det = aPl_Determiner;     -- indefinite plural   ---s
        the_Det = the_Determiner;     -- definite singular   ---s
        thePl_Det = thePl_Determiner; -- definite plural     ---s
        UseN = UseNoun;               -- house
        AdjCN = AdjCommonNoun;        -- big house

    -- Adjective
    lin
        PositA a = a; -- warm

    -- Adverb
    lin
        PrepNP prep np = { s = prep.s ++ np.s ! PF Prepos NonPoss }; -- in the house

    -- Conjunction
    lin
        CoordS conj s1 s2 = {
            s = s1.s ++ conj.s ++ s2.s
        }; -- he walks and she runs ---s

    -- Tense
    lin
        PPos = { s = [] ; isTrue = True };  -- I sleep  [positive polarity]
        PNeg = { s = [] ; isTrue = False }; -- I do not sleep [negative polarity]
        TSim = { s = [] ; isPres = True };  -- simultanous: she sleeps ---s
        TAnt = { s = [] ; isPres = False }; -- anterior: she has slept ---s

    -- Structural
    lin
        and_Conj = { s = "та" };
        or_Conj  = { s = "чи" };

        every_Det = every_Determiner;

        in_Prep = { s = "в" };
        on_Prep = { s = "на" };
        with_Prep = { s = "з" };

        i_Pron = {
            s = table {
                PF Nom _    => "я";
                PF Gen _    => "мене";
                PF Dat _    => "мені";
                PF Acc _    => "мене";
                PF Inst _   => "мною";
                PF Prepos _ => "мені";
                PF Vocat _  => "я"
            };
            n = Sg;
            p = P1;
            g = PNoGen;
            pron = True
        };

        youSg_Pron = {
            s = table {
                PF Nom _    => "ти";
                PF Gen _    => "тебе";
                PF Dat _    => "тобі";
                PF Acc _    => "тебе";
                PF Inst _   => "тобою";
                PF Prepos _ => "тобі";
                PF Vocat _  => "ти"
            };
            n = Sg;
            p = P2;
            g = PNoGen;
            pron = True
        };

        he_Pron = {
            s = table {
                PF Nom _    => "він";
                PF Gen _    => "його";
                PF Dat _    => "йому";
                PF Acc _    => "його";
                PF Inst _   => "ним";
                PF Prepos _ => "ньому";
                PF Vocat _  => "він"
            };
            n = Sg;
            p = P3;
            g = PGen Masc;
            pron = True
        };

        she_Pron = {
            s = table {
                PF Nom _    => "вона";
                PF Gen _    => "її";
                PF Dat _    => "їй";
                PF Acc _    => "її";
                PF Inst _   => "нею";
                PF Prepos _ => "ній";
                PF Vocat _  => "вона"
            };
            n = Sg;
            p = P3;
            g = PGen Fem;
            pron = True
        };

        we_Pron = {
            s = table {
                PF Nom _    => "ми";
                PF Gen _    => "нас";
                PF Dat _    => "нам";
                PF Acc _    => "нас";
                PF Inst _   => "нами";
                PF Prepos _ => "нас";
                PF Vocat _  => "ми"
            };
            n = Pl;
            p = P1;
            g = PNoGen;
            pron = True
        };

        youPl_Pron = {
            s = table {
                PF Nom _    => "ви";
                PF Gen _    => "вас";
                PF Dat _    => "вам";
                PF Acc _    => "вас";
                PF Inst _   => "вами";
                PF Prepos _ => "вас";
                PF Vocat _  => "ви"
            };
            n = Pl;
            p = P2;
            g = PNoGen;
            pron = True
        };
        
        they_Pron = {
            s = table {
                PF Nom _    => "вони";
                PF Gen _    => "їх";
                PF Dat _    => "їм";
                PF Acc _    => "їх";
                PF Inst _   => "ними";
                PF Prepos _ => "них";
                PF Vocat _  => "вони"
            };
            n = Pl;
            p = P3;
            g = PNoGen;
            pron = True
        };

        whoSg_IP = {
            s = table {
                Nom  => "хто";
                Gen  => "кого";
                Acc  => "кого";
                Dat  => "кому";
                Inst => "ким";
                _    => "хто"
            };
            a = Agr Sg P3
        };

        where_IAdv = { s = "де" };
        why_IAdv   = { s = "чому" };

        have_V2 = mkV2 (mkV Imperfective "мати" "май" "маю" "маєш" "має" "маємо" "маєте" "мають" "мав") Acc;
        want_VV = mkV Imperfective "хотіти" "хоч" "хочу" "хочеш" "хоче" "хочемо" "хочете" "хочуть" "хотів";
}
