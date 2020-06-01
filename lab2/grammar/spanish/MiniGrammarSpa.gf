--# -path=.:../abstract
concrete MiniGrammarSpa of MiniGrammar = open MiniResSpa, Prelude in {

  lincat
    Utt = {s : Str} ;
    Pol  = {s : Str ; p : Bool} ; -- Polarity. s is empty, but needed for parsing
    Temp = {s : Str ; t : TenseForm} ;

    S  = {s : Str} ;
    QS = {s : Str} ;
    -- TODO: adjust to Spanish, does not make any sense
    Cl = {   -- word order is fixed in S and QS
      subj : Str ; -- subject (TODO: should be optional)
      verb : Bool => TenseForm => Str ; -- dep. on Pol,Temp, e.g. "does","sleep"
      compl : Str -- after verb: complement, adverbs
    } ;
    QCl = Cl ;
    Imp = {s : Bool => Str} ; -- imperative (negative or positive)
    VP = {verb : Verb ; compl : Str} ; -- I don't think I need GVerbs
    Comp = {s : Str} ;  -- copula complement
    AP = Adjective ;
    CN = Noun ; -- common noun
    NP = {s : Case => Str ; a : NPAgreement} ; -- TODO: is case relevant here?
    IP = {s : Str ; a : NPAgreement} ;
    Pron = {
      s : PronForm => Str ; 
      a : NPAgreement 
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
    PN = {s : Str} ; -- proper name
    Adv = {s : Str} ;
    IAdv = {s : Str} ; -- interrogative

  lin
    UttS s = s ;
    UttQS s = s ;
    UttNP np = {s = np.s ! Acc} ;
    UttAdv adv = adv ;
    UttIAdv iadv = iadv ;
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.p} ;

    UseCl temp pol cl = 
      let vf = cl.verb ! pol.p ! temp.t in {
      s = pol.s ++ temp.s ++ -- GF hack, they are empty!
	        cl.subj ++ -- ella
          negation pol.p ++ -- no
          vf ++ -- bebe
	        cl.compl -- cerveza
    } ;

    UseQCl temp pol qcl =
      let vf = qcl.verb ! pol.p ! temp.t in {
        s = pol.s ++ temp.s ++ -- hack again
            qcl.subj ++ -- quién/ella
	          negation pol.p ++ -- no
	          vf ++ -- bebe
	          qcl.compl -- cerveza
      } ;

    QuestCl cl = cl ;

    {-
    PredVP np vp = {
      subj = np.s ! Nom ;
      compl = vp.compl ;
      verb = vp.verb.s ;
    } ;
    -}

    -- QuestVP ip vp = PredVP ip vp ; 
    {-
    ImpVP vp = {
      s = table {
        True  => vp.verb.s ! VForm Inf ++ vp.compl ;    -- in Eng, imperative = infinitive
        False => "do not" ++ vp.verb.s ! VF Inf ++ vp.compl
      }
    } ;
    -}

    UseV v = {
      verb = v ;
      compl = []
    } ;

    ComplV2 v2 np = {
      verb = v2 ;
      compl = v2.c ++ np.s ! Acc
    } ;

    ComplVS vs s = {
      verb = vs ;
      compl = "que" ++ s.s ;
    } ;

    ComplVV vv vp = {
      verb = vv ;
      compl = vp.verb.s ! VFImp VInf ++ vp.compl ;
      } ;

    UseComp comp = {
      verb = ser "s" ; -- copula TODO: change implementation of ser
      compl = comp.s
    } ;    

    -- CompAP ap = ap ; -- TODO: agreement as a lambda...?

    CompNP np = {
      s = np.s ! Nom
    } ;

    CompAdv adv = adv ;

    AdvVP vp adv = vp ** {compl = vp.compl ++ adv.s} ;

    -- common noun with det
    DetCN det cn = {
      s = table {c => det.s ! cn.g ++ cn.s ! det.n} ;
      a = NPAgr det.n P3
    } ;

    -- proper noun
    UsePN pn = {
      s = \\_ => pn.s ;
      a = NPAgr Sg P3
    } ;

    --UsePron p = { s = p.s ! (PForm Nom (NGAgr Sg M)) } ;

    MassNP cn = {
      s = \\_ => cn.s ! Sg ;
      a = NPAgr Sg P3
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
    AdjCN ap cn = {
      s = table {n => cn.s ! n ++ ap.s ! (NGAgr Sg M) }
    } ;
    -}

    PositA a = a;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;

    PPos  = {s = [] ; p = True} ;
    PNeg  = {s = [] ; p = False} ;

    TSim  = {s = []    ; t = Simple} ;
    TPPref  = {s = []    ; t = PretPerf} ;
    TPPlus  = {s = []    ; t = PretPlus} ;
    TPAnt  = {s = []    ; t = PretAnt} ;
    TFComp  = {s = []    ; t = FutComp} ;
    TGer  = {s = []    ; t = Ger} ;

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

    -- TODO: ? gender of we you etc.
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
      a = NPAgr Sg P3
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
      a = NPAgr Sg P3
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
      a = NPAgr Pl P1
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
      a = NPAgr Pl P2
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
      a = NPAgr Pl P3
    } ;

    whoSg_IP = { 
      s = "quién" ;
      a = NPAgr Sg P3
      } ;

    -- no plural, for some reason

    where_IAdv = {s = "dònde"} ;
    why_IAdv = {s = "por qué"} ;

    have_V2 = smartVerb "haber" ** {c = []} ;

    want_VV = mkVerb "querer" "queriente" "querido" "queriendo" "quiero" "quieres" "quiere" "queremos" "queréis" "quieren" "querìa" "querìas" "querìamos" "querìais" "querìan" "quise" "quisiste" "quiso" "quisimos" "quisisteis" "quisieron" "querré" "querràs" "querrà" "querremos" "querréis" "querràn" "quiera" "quieras" "queremos" "queràis" "quieran" "quisiera" "quisieras" "quisiéramos" "quisierais" "quisieran" "quisiere" "quisieres" "quisiéremos" "quisiereis" "quisieren" "quiere" "queramos" "quered" "quieras" "queramos" "queràis" "querrìa" "querrìas" "querrìamos" "querrìais" "querrìan" ;
}