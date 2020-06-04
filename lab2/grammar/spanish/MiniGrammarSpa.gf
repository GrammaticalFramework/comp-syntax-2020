--# -path=.:../abstract
concrete MiniGrammarSpa of MiniGrammar = open MiniResSpa, Prelude in {

  lincat
    Utt = {s : Str} ;
    Pol  = {s : Str ; isPos : Bool} ; 
    Temp = {s : Str ; isPres : Bool} ; 

    S  = {s : Str} ;
    QS = {s : Str} ;
    Cl = { -- word order is set in S and QS
      {- 
      NOTE: subject should be optional! One way I can think of to make it so
      is to add Expl = {s : Str ; isExpl : Bool} to the AS and add use it as 
      Pol and Temp to know whether the subject is implicit or explicit.  
      -}
      subj : Str ; 
      verb : Bool => Bool => Str ; -- depends on Pol and Temp
      compl : { s : Str ; isPron : Bool } ;
      adv : Adv
    } ;
    QCl = Cl ;
    Imp = {s : Bool => Str} ; -- depends on Pol
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
    Det = { 
      -- should be NGAgr but there was some reason for it not to be
      s : Gender => Str ; 
      n : Number 
    } ;
    Conj = {s : Str} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    VS = Verb ;
    VV = Verb ; 
    A = Adjective ;
    N = Noun ;
    PN = Noun ; 
    -- NOTE: isFinal is due to the fact that different adverbs occupy different
    -- positions. Reality is more complicated than this anyway
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
        pos = pol.isPos ;
        vf = cl.verb ! pos ! temp.isPres ;
        comp = cl.compl ;
        adv = cl.adv
      in {
        s = pol.s ++ temp.s ++ cl.subj ++ case <adv.isFinal, comp.isPron> of {
          <False,False> => adv.s ++ negation pos ++ vf ++ comp.s ;
          <False,True> => adv.s ++ negation pos ++ comp.s ++ vf ;
          <True,False> => negation pos ++ vf ++ comp.s ++ adv.s ;
          <True,True> => negation pos ++ comp.s ++ vf ++ adv.s
        }
      } ;

    UseQCl temp pol qcl = UseCl temp pol qcl ;

    -- there is a warning but everything seems to work
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
          False => (haber.s ! (VPres np.a)) ++ (vp.verb.s ! VPartPast)
        } ;
        adv = vp.adv
      }  ;

    QuestVP ip vp = PredVP ip vp ; 

    ImpVP vp = {
      {-
      NOTE: agreement is hardcoded (twice) because 
      - the only sentences we can form seem to be singular and, I assume, second person
      - gender is irrelevant here 
      -}
      s = \\pol => let 
        verb = vp.verb.s ! (VImp (NPAgr Sg P2) (polarity pol)) ;
        compl = vp.compl ! (NGAgr Sg M) ;
        most = negation pol ++ case pol of {
          True => 
            -- TODO: remove space between the two tokens whenever vp.isPron
            -- (problem: unsupported token gluing) 
            verb ++ compl ;
          False => case vp.isPron of {
            True => compl ++ verb ;
            False => verb ++ compl
          }
        } in case vp.adv.isFinal of {
          True => most ++ vp.adv.s ;
          False => vp.adv.s ++ most
        }
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
        the only V2 with indirect object we have in the vocabulary, "matar".
        Example:
        - "yo mato a Juan" = "I kill John"
        - "yo lo mato" = "I kill him"
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
        s = vp.adv.s ;
        isFinal = False
      }
    } ;
    
    UseComp comp = {
      verb = ser ;
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

    DetCN det cn = {
      s = table {c => det.s ! cn.g ++ cn.s ! det.n} ;
      a = NPAgr det.n P3 ;
      g = cn.g ;
      isPron = False
    } ;

    UsePN pn = {
      s = \\_ => pn.s ! Sg ;
      a = NPAgr Sg P3 ;
      g = pn.g ;
      isPron = False
    } ;

    UsePron p = {
      s = table {
        -- NGAgr is arbitrary (only important for genitive... whatever!)
        c => (p.s) ! (PForm c (NGAgr Sg M)) 
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
        F => "la" -- even though that's another story for "el agua" y "el aguila" (their gender is feminine but their article is masculine)
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

    {-
    NOTE: one way this grammar is overgenerating is that it accepts/generates
    sentences like "Yo veo gato", while a determiner would be necessary when 
    the noun is singular. As this also happens in the English, I will not fix it.
    -}
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
        PForm Gen (NGAgr Sg M) => "mío" ;
        PForm Gen (NGAgr Sg F) => "mía" ;
        PForm Gen (NGAgr Pl M) => "míos" ;
        PForm Gen (NGAgr Pl F) => "mías" ;
        PForm Pre _ => "mí"
      } ;
      a = NPAgr Sg P1 ;
      g = M  -- not really, but there is no way to know
      } ;
    
    youSg_Pron = {
      s = table {
        PForm Nom _ => "tú" ; 
        PForm Acc _ => "te" ;
        PForm Dat _ => "te" ;
        PForm Gen (NGAgr Sg M) => "tuyo" ;
        PForm Gen (NGAgr Sg F) => "tuya" ;
        PForm Gen (NGAgr Pl M) => "tuyos" ;
        PForm Gen (NGAgr Pl F) => "tuyas" ;
        PForm Pre _ => "ti"
      } ;
      a = NPAgr Sg P2 ;
      g = M -- not really, but there is no way to know
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
      g = M -- not really, but there is no way to know
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
      g = F -- not really, but there is no way to know
    } ;

    -- it_Pron missing in AST :(

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
      g = M  -- missing "nosotras"
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
      g = M  -- missing "vosotras"
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
      g = M  -- missing "ellas"
    } ;

    whoSg_IP = { 
      s = table { _ => "quién"} ; -- case does not matter
      a = NPAgr Sg P3 ;
      g = M ; -- not really, but there is no way to know
      isPron = True 
      } ;

    -- no plural, for some reason

    where_IAdv = {s = "dónde"} ;
    why_IAdv = {s = "por qué"} ;

    have_V2 = smartVerb "haber" ** {c = []} ;

    want_VV = mkVerb "querer" "querido" "quiero" "quieres" "quiere" "queremos" "queréis" "quieren" "quiere" "queramos" "quered" "quieras" "queramos" "queráis" ;
}