--# -path=.:../abstract
concrete MiniGrammarSpa of MiniGrammar = open MiniResSpa, Prelude in {

  lincat
    Utt = {s : Str} ;
    Pol  = {s : Str ; isPos : Bool} ; 
    Temp = {s : Str ; isPres : Bool} ; 

    S  = {s : Str} ;
    QS = {s : Str} ;
    Cl = {   -- word order is fixed in S and QS
      subj : Str ; -- subject (should be optional!)
      verb : Bool => Bool => Str ; -- depends on Pol and Temp
      compl : { s : Str ; isPron : Bool } ; -- after verb: complement, adverbs
      adv : Adv
    } ;
    QCl = Cl ;
    Imp = {s : Bool => Str} ; -- imperative (depends on Pol)
    VP = {verb : Verb ; compl : NGAgreement => Str ; isPron : Bool ; adv : Adv } ;
    Comp = {s : NGAgreement => Str; isPron : Bool} ;  -- copula complement
    AP = Adjective ;
    CN = Noun ; -- common noun
    NP = {s : Case => Str ; a : NPAgreement; g : Gender ; isPron : Bool } ;
    IP = {s : Case => Str ; a : NPAgreement; g : Gender ; isPron : Bool } ;
    Pron = {
      s : PronForm => Str ; 
      a : NPAgreement ;
      g : Gender
    } ;
    Det = { -- would have been nicer with NGAgr actually
      s : Gender => Str ; 
      n : Number 
    } ;
    Conj = {s : Str} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    VS = Verb ;
    VV = Verb ; ---- only VV to VP
    A = Adjective ;
    N = Noun ;
    PN = Noun ; -- proper name
    Adv = {s : Str ; isFinal : Bool} ;
    IAdv = {s : Str} ; -- interrogative

  lin
    UttS s = s ;
    UttQS s = s ;
    UttNP np = {s = np.s ! Acc} ;
    UttAdv adv = adv ;
    UttIAdv iadv = iadv ;
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.isPos} ;

    UseCl temp pol cl = 
      let 
        vf = cl.verb ! pol.isPos ! temp.isPres ;
        comp = cl.compl
      in {
        s = pol.s ++ temp.s ++ cl.subj ++ case <cl.adv.isFinal, comp.isPron> of {
          <False,False> => cl.adv.s ++ negation pol.isPos ++ vf ++ comp.s ;
          <False,True> => cl.adv.s ++ negation pol.isPos ++ comp.s ++ vf ;
          <True,False> => negation pol.isPos ++ vf ++ comp.s ++ cl.adv.s ;
          <True,True> => negation pol.isPos ++ comp.s ++ vf ++ cl.adv.s
        }
      } ;

    -- added ¿? just to visually distinguish questions
    UseQCl temp pol qcl = 
      {s = "¿" ++ (UseCl temp pol qcl).s ++ "?"} ;

    -- NOTE: there is a warning but everything seems to work
    QuestCl cl = cl ;

    PredVP np vp = 
      let 
        n = extractNumber np.a ;
        g = np.g 
      in {
        subj = np.s ! Nom ;
        compl = {
          s = vp.compl ! (NGAgr n g) ;
          isPron = vp.isPron
        } ;
        verb = \\_,isPres => case isPres of {
          True => vp.verb.s ! (VPres np.a) ;
          False => ((smartVerb "haber").s ! (VPres np.a)) ++ (vp.verb.s ! VPartPast)
        } ;
        adv = vp.adv
      }  ;

    QuestVP ip vp = PredVP ip vp ; 

    ImpVP vp = {
      -- NOTE: agreement is hardcoded (twice) because the only sentences we can 
      -- form seem to be singular and, I assume, second person, while gender is 
      -- irrelevant here
      s = \\pol => 
        negation pol ++
        -- TODO: remove space between the two tokens whenever vp.isPron
        -- (problem: unsupported token gluing) 
        (vp.verb.s ! (VImp (NPAgr Sg P2) (polarity pol))) ++ 
        vp.compl ! (NGAgr Sg M)
    } ;

    UseV v = {
      verb = v ;
      compl = \\_ => [] ;
      isPron = False ;
      adv = lin Adv {
        s = [] ;
        isFinal = False
      }
    } ;

    ComplV2 v2 np = let pron = np.isPron in {
      verb = v2 ;
      compl = \\_ => case pron of {
        {-
        NOTE: I think this is valid in general, but at least it is for sure for
        the only V2 with indirect object we have in the vocabulary, "matar"
        -}
        True => np.s ! Acc ;
        False => v2.c ++ np.s ! Acc 
      } ;
      isPron = pron ;
      adv = lin Adv {
        s = [] ;
        isFinal = False
      }
    } ;

    ComplVS vs s = {
      verb = vs ;
      compl = \\_ => "que" ++ s.s ;
      isPron = False ;
      adv = lin Adv {
        s = [] ;
        isFinal = False
      }
    } ;

    ComplVV vv vp = {
      verb = vv ;
      -- TODO: remove space between the two tokens whenever vp.isPron
      -- (problem: unsupported token gluing)
      compl = \\agr => vp.verb.s ! VInf ++ vp.compl ! agr ;
      isPron = False ;
      adv = lin Adv {
        s = [] ;
        isFinal = False
      }
    } ;
    
    UseComp comp = {
      verb = ser "s" ;
      compl = comp.s ;
      isPron = False ;
      adv = lin Adv {
        s = [] ;
        isFinal = False
      }
    } ;   

    CompAP ap = {
      s = ap.s ;
      isPron = False
    } ;

    CompNP np = {
      s = \\_ => np.s ! Nom ;
      isPron = np.isPron
    } ;

    CompAdv adv = { 
      s = \\_ => adv.s ;
      isPron = False
    } ;

    AdvVP vp adv = vp ** {
      adv = lin Adv {
        s = adv.s ;
        isFinal = adv.isFinal
      }} ;

    -- common noun with det
    DetCN det cn = {
      s = table {c => det.s ! cn.g ++ cn.s ! det.n} ;
      a = NPAgr det.n P3 ;
      g = cn.g ;
      isPron = False
    } ;

    -- proper noun
    UsePN pn = {
      s = \\_ => pn.s ! Sg ;
      a = NPAgr Sg P3 ;
      g = pn.g ;
      isPron = False
    } ;

    UsePron p = {
      s = table {
        c => (p.s) ! (PForm c (NGAgr Sg M)) -- NGAgr is arbitrary (only important for genitive)
      } ;
      a = p.a ;
      g = p.g ;
      isPron = True ;
    } ;
   
    MassNP cn = {
      s = \\_ => cn.s ! Sg ;
      a = NPAgr Sg P3 ;
      g = cn.g ;
      isPron = False
    } ;

    a_Det = {
      s = table {
        M => "un" ;
        F => "una" 
      } ;
      n = Sg
    } ;

    aPl_Det = {
      s = table {
        M => "unos" ;
        F => "unas" 
      } ;
      n = Pl
    } ;

    the_Det = {
      s = table {
        M => "el" ;
        F => "la" -- even though that's another story for "el agua" y "el aguila"
      } ;
      n = Sg
    } ;

    thePl_Det = {
      s = table {
        M => "los" ;
        F => "las"
      } ;
      n = Pl
    } ;

    UseN n = n ;

    {- 
    NOTE: adjectives in Spanish are usually, but not always, after the noun 
    they modify, but their position depends more on the intended meaning
    than on the type of adjective, cf. "Posición" in
    https://es.wikipedia.org/wiki/Gram%C3%A1tica_del_espa%C3%B1ol#Adjetivo
    -}
    AdjCN ap cn = {
      s = \\n => (cn.s ! n) ++ (ap.s ! (NGAgr n (cn.g))) ;
      g = cn.g
    } ;

    PositA a = a;

    PrepNP prep np = {
      s = prep.s ++ np.s ! Acc ;
      isFinal = True } ;

    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;

    PPos  = {s = [] ; isPos = True} ;
    PNeg  = {s = [] ; isPos = False} ;

    TSim  = {s = [] ; isPres = True} ;
    TAnt  = {s = [] ; isPres = False} ;

    and_Conj = {s = "y"} ;
    or_Conj = {s = "o"} ;

    every_Det = {
      s = table {
        _ => "cada"
      } ; 
      n = Sg
    } ;

    in_Prep = {s = "en"} ;
    on_Prep = {s = "sobre"} ;
    with_Prep = {s = "con"} ;

    i_Pron = {
      s = table {
        PForm Nom _ => "yo" ; 
        PForm Acc _ => "me" ;
        PForm Dat _ => "me" ;
        PForm Gen (NGAgr Sg M) => "mìo" ;
        PForm Gen (NGAgr Sg F) => "mìa" ;
        PForm Gen (NGAgr Pl M) => "mìos" ;
        PForm Gen (NGAgr Pl F) => "mìas" ;
        PForm Pre _ => "mì"
      } ;
      a = NPAgr Sg P1 ;
      g = M  -- well not really but there is no way to know ?
      } ;
    
    youSg_Pron = {
      s = table {
        PForm Nom _ => "tù" ; 
        PForm Acc _ => "te" ;
        PForm Dat _ => "te" ;
        PForm Gen (NGAgr Sg M) => "tuyo" ;
        PForm Gen (NGAgr Sg F) => "tuya" ;
        PForm Gen (NGAgr Pl M) => "tuyos" ;
        PForm Gen (NGAgr Pl F) => "tuyas" ;
        PForm Pre _ => "ti"
      } ;
      a = NPAgr Sg P2 ;
      g = M -- well not really but there is no way to know ?
    } ;

    he_Pron = {
      s = table {
        PForm Nom _ => "él" ; 
        PForm Acc _ => "lo" ;
        PForm Dat _ => "le" ;
        PForm Gen (NGAgr Sg M) => "suyo" ;
        PForm Gen (NGAgr Sg F) => "suya" ;
        PForm Gen (NGAgr Pl M) => "suyos" ;
        PForm Gen (NGAgr Pl F) => "suyas" ;
        PForm Pre _ => "se"
      } ;
      a = NPAgr Sg P3 ;
      g = M 
    } ;

    she_Pron = {
      s = table {
        PForm Nom _ => "ella" ; 
        PForm Acc _ => "la" ;
        PForm Dat _ => "le" ;
        PForm Gen (NGAgr Sg M) => "suyo" ;
        PForm Gen (NGAgr Sg F) => "suya" ;
        PForm Gen (NGAgr Pl M) => "suyos" ;
        PForm Gen (NGAgr Pl F) => "suyas" ; 
        PForm Pre _ => "se"
      } ;
      a = NPAgr Sg P3 ;
      g = F
    } ;

      -- it_Pron missing in AST

    we_Pron = {
      s = table {
        PForm Nom _ => "nosotros" ;
        PForm Acc _ => "nos" ;
        PForm Dat _ => "nos" ;
        PForm Gen (NGAgr Sg M) => "nuestro" ;
        PForm Gen (NGAgr Sg F) => "nuestra" ;
        PForm Gen (NGAgr Pl M) => "nuestros" ;
        PForm Gen (NGAgr Pl F) => "nuestras" ;
        PForm Pre _ => "nosotros"
      } ;
      a = NPAgr Pl P1 ;
      g = M  -- well not really but there is no way to know ?
    } ;

    youPl_Pron = {
      s = table {
        PForm Nom _ => "vosotros" ;
        PForm Acc _ => "vos" ;
        PForm Dat _ => "vos" ;
        PForm Gen (NGAgr Sg M) => "vuestro" ;
        PForm Gen (NGAgr Sg F) => "vuestra" ;
        PForm Gen (NGAgr Pl M) => "vuestros" ;
        PForm Gen (NGAgr Pl F) => "vuestras" ; 
        PForm Pre _ => "vosotros"
      } ;
      a = NPAgr Pl P2 ;
      g = M  -- well not really but there is no way to know ?
    } ;

    they_Pron = {
      s = table {
        PForm Nom _ => "ellos" ;
        PForm Acc _ => "los" ;
        PForm Dat _ => "les" ;
        PForm Gen (NGAgr Sg M) => "suyo" ;
        PForm Gen (NGAgr Sg F) => "suya" ;
        PForm Gen (NGAgr Pl M) => "suyos" ;
        PForm Gen (NGAgr Pl F) => "suyas" ;
        PForm Pre _ => "se"
      } ;
      a = NPAgr Pl P3 ;
      g = M  -- well not really but there is no way to know ?
    } ;

    whoSg_IP = { 
      s = table { _ => "quién"} ; -- case does not matter
      a = NPAgr Sg P3 ;
      g = M ; -- again completely arbitrary
      isPron = True -- well technically
      } ;

    -- no plural, for some reason

    where_IAdv = {s = "dònde"} ;
    why_IAdv = {s = "por qué"} ;

    have_V2 = smartVerb "haber" ** {c = []} ;

    want_VV = mkVerb "querer" "querido" "quiero" "quieres" "quiere" "queremos" "queréis" "quieren" "quiere" "queramos" "quered" "quieras" "queramos" "queràis" ;
}