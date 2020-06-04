--# -coding=utf8

resource MiniResUkr = ParamX ** open Prelude in {
  flags  coding=utf8;

  param
    Gender = Masc | Fem | Neut;
    Case   = Nom | Gen | Dat | Acc | Inst | Prepos | Vocat;

  param
    NForm = NF Number Case;

  oper
    Noun: Type = {s : NForm => Str; g : Gender};

  oper
    CommonNoun = { noun : NForm => Str; g : Gender; };

  oper
    mkN = overload {
      mkN : (word : Str) -> Noun = mk1N;
      mkN : (nomSg, genSg, datSg, accSg, instSg, preposSg, vocSg, nomPl, genPl, datPl, accPl, instPl, preposPl, vocPl : Str) -> Gender -> Noun = mkWorstN
    };

  oper mk1N : Str -> Noun = \word ->
    case word of {
      stem + "а"         => mkFemN stem;
      stem + ("о" | "е") => mkNeutN word;
      _                  => mkMascN word };

  oper mkMascN : Str -> Noun = \stem ->
      { s = table {
          NF Sg Nom         => stem;
          NF Sg Gen         => stem + "а";
          NF Sg Dat         => stem + "у";
          NF Sg Acc         => stem + "i";
          NF Sg Inst        => stem + "oм";
          NF Sg Prepos      => stem + "i";
          NF Sg Vocat       => stem + "e";
          NF Pl Nom         => stem + "и";
          NF Pl Gen         => stem + "ів" ;
          NF Pl Dat         => stem + "ам";
          NF Pl Acc         => stem + "iв";
          NF Pl Inst        => stem + "ами";
          NF Pl Prepos      => stem + "ах";
          NF PL Vocat       => stem + "и" };
      g = Masc };

  oper mkNeutN : Str -> Noun = \word ->
    { s = table {
            NF Sg Nom         => word;
            NF Sg Gen         => case word of {
                                    stem + "е" => stem + "я";
                                    _          => Predef.tk 1 word + "а"
                                };
            NF Sg Dat         => case word of {
                                    stem + "е" => stem + "ю";
                                    _          => Predef.tk 1 word + "у"
                                };
            NF Sg Acc         => word;
            NF Sg Inst        => word + "м";
            NF Sg Prepos      => Predef.tk 1 word + "i";
            NF Sg Vocat       => word;
            NF Pl Nom         => case word of {
                                    stem + "е" => stem + "я";
                                    _          => Predef.tk 1 word + "а"
                                };
            NF Pl Gen         => case word of {
                                    stem + "е" => stem + "ів";
                                    _          => word
                                };
            NF Pl Dat         => case word of {
                                    stem + "е" => stem + "ям";
                                    _          => Predef.tk 1 word + "ам"
                                };
            NF Pl Acc         => case word of {
                                    stem + "е" => stem + "я";
                                    _          => Predef.tk 1 word + "а"
                                };
            NF Pl Inst        => case word of {
                                    stem + "е" => stem + "ями";
                                    _          => Predef.tk 1 word + "ами"
                                };
            NF Pl Prepos      => case word of {
                                    stem + "е" => stem + "ях";
                                    _          => Predef.tk 1 word + "ах"
                                };
            NF Pl Vocat       => case word of {
                                    stem + "е" => stem + "я";
                                    _          => Predef.tk 1 word + "а"
                                } 
        };
      g = Neut };

  oper mkFemN : Str -> Noun = \stem ->
    { s = table {
            NF Sg Nom         => stem + "а";
            NF Sg Gen         => stem + "и";
            NF Sg Dat         => stem + "і";
            NF Sg Acc         => stem + "у";
            NF Sg Inst        => stem + "ою";
            NF Sg Prepos      => stem + "і";
            NF Sg Vocat       => stem + "о";
            NF Pl Nom         => stem + "и";
            NF Pl Gen         => stem + "ок";
            NF Pl Dat         => stem + "ам";
            NF Pl Acc         => stem + "ок";
            NF Pl Inst        => stem + "ами";
            NF Pl Prepos      => stem + "ах";
            NF Pl Vocat       => stem + "и" };
      g = Fem };

  oper mkWorstN  : (nomSg, genSg, datSg, accSg, instSg, preposSg, vocSg,
          nomPl, genPl, datPl, accPl, instPl, preposPl, vocPl : Str) -> Gender -> Noun
    =  \nomSg, genSg, datSg, accSg, instSg, preposSg, vocSg,
          nomPl, genPl, datPl, accPl, instPl, preposPl, vocPl, g ->
   {
     s = table {
           NF Sg Nom     => nomSg;
           NF Sg Gen     => genSg;
           NF Sg Dat     => datSg;
           NF Sg Acc     => accSg;
           NF Sg Inst    => instSg;
           NF Sg Prepos  => preposSg;
           NF Sg Vocat   => vocSg;
           NF Pl Nom     => nomPl;
           NF Pl Gen     => genPl;
           NF Pl Dat     => datPl;
           NF Pl Acc     => accPl;
           NF Pl Inst    => instPl;
           NF Pl Prepos  => preposPl;
           NF Pl Voc     => vocPl };
     g = g };

  oper
    NounPhrase : Type = {
      s : PronForm => Str;
      n : Number;
      p : Person;
      g: PronGen;
      pron: Bool
    };

  oper
    MassNounPhrase : CommonNoun -> NounPhrase = \noun -> {
      s = \\c => noun.noun ! NF Sg (getPronounCase c);
      n = Sg;
      p = P3;
      g = PGen noun.g;
      pron = False
    };

  oper
    UseNoun : Noun -> CommonNoun = \noun -> {
      noun = \\nf => noun.s ! nf ;
      g = noun.g
    };

  -- Proper nouns
  oper PropNoun = { s :  Case => Str; g : Gender };

  oper
    UsePropNoun: PropNoun -> NounPhrase = \name -> {
      s = \\c => name.s ! (getPronounCase c);
      p = P3;
      g = PGen name.g;
      n = Sg;
      nComp = Sg;
      pron = False
    };

  oper
    mkPN  : Str -> Gender -> Number -> PropNoun = \name, g, n ->
      case n of {
        Sg => case g of {
          Masc => mkProperNounMasc name;
          Fem  => mkProperNounFem name;
          _    => mkProperNounMasc name
        };
        Pl => mkProperNounPl name
      };

  oper
    mkProperNounMasc : Str -> PropNoun = \taras ->
      { s =  table {  Nom    => taras;
                      Gen    => taras + "а";
                      Dat    => taras + "у";
                      Acc    => taras + "а";
                      Inst   => taras + "ом";
                      Prepos => taras + "і";
                      Vocat  => taras + "е" } ;
        g = Masc };

  oper
    mkProperNounFem : Str -> PropNoun = \mariia ->
      { s =  table {  Nom    => mariia;
                      Gen    => mariia + "ї";
                      Dat    => mariia + "ї";
                      Acc    => mariia + "ю";
                      Inst   => mariia + "єю";
                      Prepos => mariia + "ї";
                      Vocat  => mariia + "є" } ;
        g = Fem };

  oper
    mkProperNounNeut : Str -> PropNoun = \oslo ->
      { s =  table {  _    => oslo } ;
        g = Neut };

  oper
    mkProperNounPl : Str -> PropNoun = \nederlandy ->
        { s = table { Nom    => nederlandy;
                      Gen    => nederlandy + "ів";
                      Dat    => nederlandy + "ам";
                      Acc    => nederlandy;
                      Inst   => nederlandy + "ами";
                      Prepos => nederlandy + "ах";
                      Vocat  => nederlandy };
            g = Neut };

  -- Adjectives
  oper
    Adjective : Type = {s : AdjForm => Str};

  param
    GenNum = GSg Gender | GPl;
    AdjForm = AF Case GenNum;

  oper
    gennum : Gender -> Number -> GenNum = \g,n ->
      case n of {
        Sg => GSg g;
        Pl => GPl
      };

  oper
    numGenNum : GenNum -> Number = \gn ->
      case gn of {
        GSg _  => Sg;
        GPl    => Pl
      };

  mkA = overload {
    mkA : (word : Str) -> Adjective = mk1A; };

  oper
    mk1A : Str -> Adjective = \word ->
    {
      s = table {
        AF Nom (GSg Masc)    => word;                   -- гарний -> гарний
        AF Nom (GSg Fem)     => Predef.tk 2 word + "а"; -- гарний -> гарнa
        AF Nom (GSg Neut)    => Predef.tk 2 word + "е"; -- гарний -> гарне
        AF Nom GPl           => Predef.tk 2 word + "і"; -- гарний -> гарні

        AF Gen (GSg Masc)    => Predef.tk 2 word + "ого"; -- гарний -> гарного
        AF Gen (GSg Fem)     => Predef.tk 2 word + "ої";  -- гарний -> гарної
        AF Gen (GSg Neut)    => Predef.tk 2 word + "ого"; -- гарний -> гарного
        AF Gen GPl           => Predef.tk 2 word + "их";  -- гарний -> гарних

        AF Dat (GSg Masc)    => Predef.tk 2 word + "ому"; -- гарний -> гарному
        AF Dat (GSg Fem)     => Predef.tk 2 word + "ій";  -- гарний -> гарній
        AF Dat (GSg Neut)    => Predef.tk 2 word + "ому"; -- гарний -> гарному
        AF Dat GPl           => Predef.tk 2 word + "им";  -- гарний -> гарним

        AF Acc (GSg Masc)    => Predef.tk 2 word + "ого"; -- гарний -> гарного
        AF Acc (GSg Fem)     => Predef.tk 2 word + "у";   -- гарний -> гарну
        AF Acc (GSg Neut)    => Predef.tk 2 word + "е";   -- гарний -> гарне
        AF Acc GPl           => Predef.tk 2 word + "их";  -- гарний -> гарних

        AF Inst (GSg Masc)   => Predef.tk 2 word + "им";  -- гарний -> гарним
        AF Inst (GSg Fem)    => Predef.tk 2 word + "ою";  -- гарний -> гарною
        AF Inst (GSg Neut)   => Predef.tk 2 word + "им";  -- гарний -> гарним
        AF Inst GPl          => Predef.tk 2 word + "ими"; -- гарний -> гарними

        AF Prepos (GSg Masc) => Predef.tk 2 word + "ому"; -- гарний -> гарному
        AF Prepos (GSg Fem)  => Predef.tk 2 word + "ій";  -- гарний -> гарній
        AF Prepos (GSg Neut) => Predef.tk 2 word + "ому"; -- гарний -> гарному
        AF Prepos GPl        => Predef.tk 2 word + "их";  -- гарний -> гарних

        AF Vocat (GSg Masc)  => word;                   -- гарний -> гарний
        AF Vocat (GSg Fem)   => Predef.tk 2 word + "а"; -- гарний -> гарнa
        AF Vocat (GSg Neut)  => Predef.tk 2 word + "е"; -- гарний -> гарне
        AF Vocat GPl         => Predef.tk 2 word + "і"  -- гарний -> гарні
      }
    };
    -- let stem = Predef.tk 2 positive in mk2A positive (stem+"ее") ;

  oper
    AdjCommonNoun : Adjective -> CommonNoun -> CommonNoun = \adj, noun -> {
      noun = \\nf => adj.s ! case nf of {
        NF Sg Gen => AF Nom GPl;
        NF n c => AF c (gennum noun.g n)
      } ++ noun.noun ! nf;
      g = noun.g
    };

  -- Pronouns
  param Possessive = NonPoss | Poss GenNum;
  param PronGen = PGen Gender | PNoGen;
  param PronForm = PF Case Possessive;

  oper
    prongen2gennum : PronGen -> Number -> GenNum = \pg, n ->
      case <pg, n> of {
        <PGen g, Sg> => GSg g;
        <PGen g, Pl> => GPl;
        <PNoGen, Sg> => GSg Masc;
        <PNoGen, Pl> => GPl
      };

  oper
    getPronounCase: PronForm -> Case = \pf -> case pf of { PF c _ => c } ;

  oper Pronoun = { s : PronForm => Str; n : Number; p : Person;
          g: PronGen;  pron: Bool};

  oper
    pgen2gen : PronGen -> Gender = \p -> case p of {
      PGen g => g;
      PNoGen => Masc
    };

  -- Verbs and Sentences
  param Aspect      = Imperfective | Perfective;
  param UkrTense    = UkrPast GenNum | Present GenNum Person | Future GenNum Person;
  param VerbForm    = VIndic UkrTense | VImper Number | VInf;

  oper UkrPolarity : Type = {s: Str ; isTrue : Bool};
  oper Temporality : Type = {s : Str ; isPres : Bool};

  oper
    Verb : Type = {
      s : VerbForm => Str;
      asp : Aspect };

  oper Verb2 : Type = Verb ** { complement : Str; cas: Case };

  oper
    VerbPhrase : Type = { verb: Verb; complement: Str };

  oper
    UseVerb : Verb -> VerbPhrase = \verb -> {
        verb = verb;
        complement = []
    };

  oper mkV : Aspect -> (inf, imperSg, presSgP1, presSgP2, presSgP3, presPlP1, presPlP2, presPlP3, pastSgMasc: Str) -> Verb =
    \aspect, inf, imperSg, presSgP1, presSgP2, presSgP3, presPlP1, presPlP2, presPlP3, pastSgMasc ->
      {
        s = table {
          VInf                        => inf;
          VIndic (UkrPast (GSg Masc)) => pastSgMasc;
          VIndic (UkrPast (GSg Fem))  => case Predef.tk 1 pastSgMasc of {
                                          s + "о" => Predef.tk 1 s + "ла";
                                          s => s + "ла" };
          VIndic (UkrPast (GSg Neut)) => Predef.tk 1 pastSgMasc + "ло";
          VIndic (UkrPast GPl)        => Predef.tk 1 pastSgMasc + "ли";
          VIndic (Present (GSg _) P1) => presSgP1;
          VIndic (Present (GSg _) P2) => presSgP2;
          VIndic (Present (GSg _) P3) => presSgP3;
          VIndic (Present GPl P1)     => presPlP1;
          VIndic (Present GPl P2)     => presPlP2;
          VIndic (Present GPl P3)     => presPlP3;
          VIndic (Future (GSg _) P1)  => Predef.tk 1 pastSgMasc + "тиму";
          VIndic (Future (GSg _) P2)  => Predef.tk 1 pastSgMasc + "тимеш";
          VIndic (Future (GSg _) P3)  => Predef.tk 1 pastSgMasc + "тиме";
          VIndic (Future GPl P1)      => Predef.tk 1 pastSgMasc + "тимемо";
          VIndic (Future GPl P2)      => Predef.tk 1 pastSgMasc + "тимете";
          VIndic (Future GPl P3)      => Predef.tk 1 pastSgMasc + "тимуть";
          VImper (Sg)                 => imperSg;
          VImper (Pl)                 => Predef.tk 1 imperSg + "іть"
        };
        asp = aspect };

    oper mkV2 = overload {
      mkV2 : Verb -> Str -> Case -> Verb2 = \verb, compl, cas ->  verb ** { complement = compl; cas = cas };
      mkV2 : Verb -> Case -> Verb2 = \verb, cas -> verb ** { complement = []; cas = cas };
    };

    oper Clause : Type = {
      subj : Str ;
      verb : Bool => Str;
      complement : Str
    };

    oper negation : Bool -> Str = \b -> case b of {True => [] ; False => "не"} ;

    oper Sentence : Type = {s : Str};

    oper UseClause : Temporality -> UkrPolarity -> Clause -> Sentence = \temp, pol, cl -> {
        s = pol.s ++ temp.s ++
	        cl.subj ++
	        negation pol.isTrue ++
	        cl.verb ! pol.isTrue ++
	        cl.complement
    };

    oper UsePredVP : NounPhrase -> VerbPhrase -> Clause = \np, vp -> {
      subj = np.s ! PF Nom NonPoss;
      verb = table {
        _ => vp.verb.s ! VIndic (Present (prongen2gennum np.g np.n) np.p)
      };
      complement = vp.complement
    };

    oper Imperative : Type = {s : Bool => Str};

    oper ImpVerbPhrase : VerbPhrase -> Imperative = \vp -> {
      s = table {
        True  => "не" ++ vp.verb.s ! VImper Sg;
        False => vp.verb.s ! VImper Sg
      }
    };

    param Agreement = Agr Number Person ;
    oper InterrogativePhrase : Type = {s : Case => Str ; a : Agreement};

    oper agr2gennum : Agreement -> GenNum = \agr -> 
      case agr of {
        Agr Sg _ => GSg Masc;
        Agr Pl _ => GPl
      };

    oper agr2person : Agreement -> Person = \agr -> 
      case agr of {
        Agr _ p => p
      };

    oper QSentence = {s : Str};

    oper QuestionVerbPhrase : InterrogativePhrase -> VerbPhrase -> Clause = \ip, vp -> {
      subj = ip.s ! Nom;
      verb = table {
        _ => vp.verb.s ! VIndic (Present (agr2gennum ip.a) (agr2person ip.a))
      };
      complement = vp.complement
    };

    oper CopCompl : Type = { s : Str };

    oper UseComplV2 : Verb2 -> NounPhrase -> VerbPhrase = \v2, np -> {
      verb = {
        s = v2.s;
        asp = v2.asp
      };
      complement = np.s ! PF Acc NonPoss
    };

    oper UseComplement : CopCompl -> VerbPhrase = \copcompl -> {
      verb = {
        s = table {
          _ => ""
        };
        asp = Imperfective
      };
      complement = copcompl.s
    };

    oper UseCompAP : Adjective -> CopCompl = \adj -> {
      s = adj.s ! AF Nom (GSg Masc)
    };

    oper UseCompNP : NounPhrase -> CopCompl = \np -> {
      s = np.s ! PF Nom NonPoss
    };

    oper UseComplVV : Verb -> VerbPhrase -> VerbPhrase = \v, vp -> {
      verb = v;
      complement = vp.verb.s ! VInf ++ vp.complement
    };

    oper UseComplVS : Verb -> Sentence -> VerbPhrase = \v, s -> {
      verb = v;
      complement = s.s
    };

    oper Adverb = { s : Str };

    oper UseCompAdv : Adverb -> CopCompl = \adv -> adv;

    oper UseAdvVP : VerbPhrase -> Adverb -> VerbPhrase = \vp, adv -> {
      verb = vp.verb;
      complement = adv.s
    };

    -- Determiners
    oper Determiner : Type = {
      s : DetForm => Str;
      n : Number;
      g : PronGen };

    param DetForm = DF Case Gender;

    oper DeterminerCN : Determiner -> CommonNoun -> NounPhrase = \det, cn -> {
      s = \\p => det.s ! DF (getPronounCase p) cn.g ++ cn.noun ! (NF det.n (getPronounCase p));
      n = det.n;
      p = P3;
      g = case det.g of {
        PNoGen => (PGen cn.g);
        _      => det.g };
      pron = False };

    oper a_Determiner : Determiner = {
      s = table {
        _ => ""
      };
      n = Sg;
      g = PNoGen };

    oper aPl_Determiner : Determiner = {
      s = table {
        _ => ""
      };
      n = Pl;
      g = PNoGen };

    oper the_Determiner : Determiner = {
      s = table {
        _ => ""
      };
      n = Sg;
      g = PNoGen };

    oper thePl_Determiner : Determiner = {
      s = table {
        _ => ""
      };
      n = Pl;
      g = PNoGen };

    oper every_Determiner : Determiner = {
      s = table {
        DF (Nom | Vocat) Masc => "кожний";
        DF (Nom | Vocat) Fem  => "кожна";
        DF (Nom | Vocat) Neut => "кожне";
        DF Gen Fem            => "кожної";
        DF Gen _              => "кожного";
        DF (Dat | Prepos) Fem => "кожній";
        DF (Dat | Prepos) _   => "кожному";
        DF Acc Masc           => "кожен";
        DF Acc Fem            => "кожну";
        DF Acc Neut           => "кожне";
        DF Inst Fem           => "кожною";
        DF Inst _             => "кожним"
      };
      n = Sg;
      g = PNoGen };
}