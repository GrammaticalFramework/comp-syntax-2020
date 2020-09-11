resource MicroResAraMhmd = open Prelude in {

param
  -- ignore tashkeel
  Number  = Sg    | Dl    | Pl    ;
  Gender  = Masc  | Fem           ;
  Case    = Nom   | Acc           ;     -- ignoring gen
  Species = NoHum | Hum           ;
  State   = Def   | Indef         ;
  Tense   = Perf  | Impf          ;     -- Shouldnt br present, past futurev?!
  Person  = Per1  | Per2  | Per3  ;

  -- ignore mood and voice
  VPer =    Vp3     Number  Gender
          | Vp2Sg           Gender
          | Vp2Dl
          | Vp2Pl           Gender
          | Vp1Sg
          | Vp1Pl ;

  -- Adjective
  ANum =    Asg Gender
          | Adl Gender  Case
          | Apl Gender  Case  Species;


oper
  -- ********** Define types **********
  Root      : Type = {F,C,L : Str} ;
  Pattern   : Type = {F,FC,CL,L : Str} ;
  Verb      : Type = {s : Tense => VPer => Str} ;
  Verb2     : Type = Verb ** {c : Str} ;
  Noun      : Type = {s : State => Number => Case => Str ; g : Gender ; sp : Species} ;
  Adjective : Type = {s : State => ANum => Str} ;

  -- ********** operations on roots and patterns **********
  -- source: GF Tutorial: https://www.grammaticalframework.org/lib/doc/rgl-tutorial/index.html
  appPattern : Root -> Pattern -> Str = \r,p ->
    p.F + r.F + p.FC + r.C + p.CL + r.L + p.L ;

  getRoot : Str -> Root = \s -> case s of {
    F@? + C@? + L => {F = F ; C = C ; L = L} ;
    _ => Predef.error ("cannot get root from" ++ s)
    } ;

  getPattern : Str -> Pattern = \s -> case s of {
    F + "F" + FC + "C" + CL + "L" + L => {F = F ; FC = FC ; CL = CL ; L = L} ;
    _ => Predef.error ("cannot get pattern from" ++ s)
  } ;

  -- **********     Nouns     **********
  nounTable : ( SgDef,   DlDefNom,   DlDefAcc,   PlDefNom,   PlDefAcc,
                SgIndef, DlIndefNom, DlIndefAcc, PlIndefNom, PlIndefAcc : Str) -> Gender -> Species -> Noun

    = \SgDef,   DlDefNom,   DlDefAcc,   PlDefNom,   PlDefAcc,
       SgIndef, DlIndefNom, DlIndefAcc, PlIndefNom, PlIndefAcc, gender, species -> {s = table {
          Def => \\n,c   => case  <n,   c  > of {
                                  <Sg,  _  >  =>  SgDef    ;    -- ignore tashkeel / grammatical case for Singular
                                  <Dl,  Nom>  =>  DlDefNom ;
                                  <Pl,  Nom>  =>  PlDefNom ;
                                  <Dl,  Acc>  =>  DlDefAcc;
                                  <Pl,  Acc>  =>  PlDefAcc } ;

          Indef => \\n,c => case  <n,   c  > of {
                                  <Sg,  _  >  =>  SgIndef     ; -- ignore tashkeel / grammatical case for Singular
                                  <Dl,  Nom>  =>  DlIndefNom  ;
                                  <Pl,  Nom>  =>  PlIndefNom  ;
                                  <Dl,  Acc>  =>  DlIndefAcc;
                                  <Pl,  Acc>  =>  PlIndefAcc  }  }; g = gender; sp = species
  } ;

  -- **********   Adjectives   **********
  smartAdj :  Str -> Str -> Str -> Adjective
    = \p,r,a -> let root = getRoot r in {s = table {
        Def     =>  table { Asg Masc        => ("ال" + a)  ;
                            Asg Fem         => case p of {
                                                "2aFoCaL" =>  ("ال" + appPattern root (getPattern "FCLاء"));
                                                "_"       =>  ("ال" + a + "ة") } ;
                            Adl Masc  Nom   => ("ال" + a + "ان") ;
                            Adl Fem   Nom   => case p of {
                                                "2aFoCaL" =>  ("ال" + appPattern root (getPattern "FCLاوتان"));
                                                "_"       =>  ("ال" + a + "تان") } ;
                            Adl Masc  Acc   => ("ال" + a + "ين") ;
                            Adl Fem   Acc   => case p of {
                                                "2aFoCaL" =>  ("ال" + appPattern root (getPattern "FCLاوتين"));
                                                "_"       =>  ("ال" + a + "تين") } ;

                            Apl Masc  Nom Hum   =>  case p of {
                                                      "2aFoCaL" =>  ("ال" + appPattern root (getPattern "FCL"));
                                                      "_"       =>  ("ال" + a + "ون") } ;
                            Apl Masc  Acc Hum   =>  case p of {
                                                      "2aFoCaL" =>  ("ال" + appPattern root (getPattern "FCL"));
                                                      "_"       =>  ("ال" + a + "ين") } ;
                            Apl Fem   _   Hum   =>  case p of {
                                                      "2aFoCaL" =>  ("ال" + appPattern root (getPattern "FCLاوات"));
                                                      "_"       =>  ("ال" + a + "ات") } ;
                            Apl _     _   NoHum =>  case p of {
                                                      "2aFoCaL" =>  ("ال" + appPattern root (getPattern "FCLاء"));
                                                      "_"       =>  ("ال" + a + "ة") }
                            } ;

        Indef   =>  table {Asg Masc        => a ;
                            Asg Fem         => case p of {
                                                "2aFoCaL" =>  (appPattern root (getPattern "FCLاء"));
                                                "_"       =>  (a + "ة") } ;
                            Adl Masc  Nom   => (a + "ان") ;
                            Adl Fem   Nom   => case p of {
                                                "2aFoCaL" =>  (appPattern root (getPattern "FCLاوتان"));
                                                "_"       =>  (a + "تان") } ;
                            Adl Masc  Acc   => (a + "ين") ;
                            Adl Fem   Acc   => case p of {
                                                "2aFoCaL" =>  (appPattern root (getPattern "FCLاوتين"));
                                                "_"       =>  (a + "تين") } ;

                            Apl Masc  Nom Hum   =>  case p of {
                                                      "2aFoCaL" =>  (appPattern root (getPattern "FCL"));
                                                      "_"       =>  (a + "ون") } ;
                            Apl Masc  Acc Hum   =>  case p of {
                                                      "2aFoCaL" =>  (appPattern root (getPattern "FCL"));
                                                      "_"       =>  (a + "ين") } ;
                            Apl Fem   _   Hum   =>  case p of {
                                                      "2aFoCaL" =>  (appPattern root (getPattern "FCLاوات"));
                                                      "_"       =>  (a + "ات") } ;
                            Apl _     _   NoHum =>  case p of {
                                                      "2aFoCaL" =>  (appPattern root (getPattern "FCLاء"));
                                                      "_"       =>  (a + "ة") }
        }
      }
  } ;

  -- **********     Verbs      **********
  patternVerb : Str -> Verb
    = \r -> let root = getRoot r in {s = table {
      Perf => table {
        Vp3 Sg Masc => case r of { x+"ي"  => appPattern (getRoot (x+"ى")) (getPattern "FCL");
                                   _      => appPattern root (getPattern "FCL")} ;
        Vp3 Sg Fem  => appPattern root (getPattern "FCLت") ;
        Vp3 Dl Masc => appPattern root (getPattern "FCLا") ;
        Vp3 Dl Fem  => appPattern root (getPattern "FCLتا") ;
        Vp3 Pl Masc => case r of { x+"ي"  => appPattern (getRoot x) (getPattern "FCLوا");
                                   _      => appPattern root (getPattern "FCLوا")} ;
        Vp3 Pl Fem  => appPattern root (getPattern "FCLن") ;

        Vp2Sg  Masc => appPattern root (getPattern "FCLت") ;
        Vp2Sg  Fem  => appPattern root (getPattern "FCLت") ;
        Vp2Dl       => appPattern root (getPattern "FCLتما") ;
        Vp2Pl  Masc => appPattern root (getPattern "FCLتم") ;
        Vp2Pl  Fem  => appPattern root (getPattern "FCLتن") ;

        Vp1Sg       => appPattern root (getPattern "FCLت") ;
        Vp1Pl       => appPattern root (getPattern "FCLنا")
      } ;

      Impf => table {
        Vp3 Sg Masc => appPattern root (getPattern "يFCL") ;
        Vp3 Sg Fem  => appPattern root (getPattern "تFCL") ;
        Vp3 Dl Masc => appPattern root (getPattern "يFCLان") ;
        Vp3 Dl Fem  => appPattern root (getPattern "تFCLان") ;
        Vp3 Pl Masc => case r of { x+"ي"  => appPattern (getRoot x) (getPattern "يFCLون") ;
                                   _      => appPattern root (getPattern "يFCLون")} ;
        Vp3 Pl Fem  => appPattern root (getPattern "يFCLن") ;

        Vp2Sg  Masc => appPattern root (getPattern "تَFCL") ;
        Vp2Sg  Fem  => appPattern root (getPattern "تFCLين") ;
        Vp2Dl       => appPattern root (getPattern "تFCLان") ;
        Vp2Pl  Masc => case r of { x+"ي"  => appPattern (getRoot x) (getPattern "تFCLون");
                                   _      => appPattern root (getPattern "تFCLون")} ;
        Vp2Pl  Fem  => appPattern root (getPattern "تFCLن") ;

        Vp1Sg       => appPattern root (getPattern "أFCL") ;
        Vp1Pl       => appPattern root (getPattern "نFCL")
      }
    }
  } ;

  ------------------------ smart paradigm ----------------------------
  -- ********** Nouns **********
  -- Feminame nouns that have Plural of 'STEM + ـات'
  femNoun : Str -> Species -> Gender -> Noun
    = \n,sp,g -> case <n,sp,g> of {
    <x+"ة",_,_> => nounTable	("ال" + n)
			                        ("ال" + x + "تان")
                      	      ("ال" + x + "تين")
                      	      ("ال" + x + "ات")
                              ("ال" + x + "ات")
                              n
                              (x + "تان")
                              (x + "تين")
                              (x + "ات")
                              (x + "ات")
                              g sp;

    <_,NoHum,Fem> => nounTable	("ال" + n)
			                          ("ال" + n + "ان")
                      	        ("ال" + n + "ين")
                      	        ("ال" + n + "ات")
                                ("ال" + n + "ات")
                                n
                                (n + "ان")
                                (n + "ين")
                                (n + "ات")
                                (n + "ات")
                                g sp
    } ;

  -- Takseer plural from root
  mkTakseer :  Str -> Str -> Str -> Species -> Gender -> Noun
    = \n,r,p,sp,g -> let root = getRoot r in case <p,g,n> of {
      <("FaCaAL"|"FeCaAL"),Fem,_>  => nounTable ("ال" + n)
                                                ("ال" + n + "تان")
                                                ("ال" + n + "تين")
                                                ("ال" + appPattern root (getPattern "FCL"))
                                                ("ال" + appPattern root (getPattern "FCL"))
                                                n
                                                (n + "تان")
                                                (n + "تين")
                                                (appPattern root (getPattern "FCL"))
                                                (appPattern root (getPattern "FCL"))
                                                g sp ;

      <("FaCaAL"|"FeCaAL"),Masc,_> => nounTable ("ال" + n)
                                                ("ال" + n + "ان")
                                                ("ال" + n + "ين")
                                                ("ال" + appPattern root (getPattern "FCL"))
                                                ("ال" + appPattern root (getPattern "FCL"))
                                                n
                                                (n + "ان")
                                                (n + "ين")
                                                (appPattern root (getPattern "FCL"))
                                                (appPattern root (getPattern "FCL"))
                                                g sp;

      <("FaCeeLah"),_,x+"ة">       => nounTable ("ال" + n)
                                                ("ال" + x + "تان")
                                                ("ال" + x + "تين")
                                                ("ال" + appPattern root (getPattern "FCL"))
                                                ("ال" + appPattern root (getPattern "FCL"))
                                                n
                                                (x + "تان")
                                                (x + "تين")
                                                (appPattern root (getPattern "FCL"))
                                                (appPattern root (getPattern "FCL"))
                                                g sp;

      <("FaCaL"|"FeCol"),_,_>      => nounTable ("ال" + n)
                                                ("ال" + n + "ان")
                                                ("ال" + n + "ين")
                                                ("ال" + appPattern root (getPattern "أFCاL"))
                                                ("ال" + appPattern root (getPattern "أFCاL"))
                                                n
                                                (n + "ان")
                                                (n + "ين")
                                                (appPattern root (getPattern "أFCاL"))
                                                (appPattern root (getPattern "أFCاL"))
                                                g sp
    } ;

  -- irregular plural nouns
  worestNoun : Str -> Str -> Species -> Gender -> Noun
    = \sg,pl,sp,g -> case sg of {
      x+"ة" => nounTable	("ال" + sg)
			                    ("ال" + x + "تان")
                      	  ("ال" + x + "تين")
                      	  ("ال" + pl)
                          ("ال" + pl)
                          sg
                          (x + "تان")
                          (x + "تين")
                          pl	pl
                          g  sp ;

      _     => nounTable	("ال" + sg)
			                    ("ال" + sg + "ان")
                      	  ("ال" + sg + "ين")
                      	  ("ال" + pl)
                          ("ال" + pl)
                          sg
                          (sg + "ان")
                          (sg + "ين")
                          pl pl
                          g  sp
  } ;

  -- ********** Verbs **********
  smartVerb : Str -> Verb = \r -> case r of {
    x + "ى" => patternVerb (x + "ي") ; -- verbs ends with ي
    _ => patternVerb r
  } ;

-- - a simplified verb agreement function
  agr2vform : Gender -> Number -> Person -> VPer
    = \g,n,p -> case <n,g,p> of {
      <Sg,       Masc,  Per3>  => Vp3   Sg Masc ;
      <Sg,       Fem,   Per3>  => Vp3   Sg Fem  ;
      <Dl,       Masc,  Per3>  => Vp3   Dl Masc ;
      <Dl,       Fem,   Per3>  => Vp3   Dl Fem  ;
      <Pl,       Masc,  Per3>  => Vp3   Pl Masc ;
      <Pl,       Fem,   Per3>  => Vp3   Pl Fem  ;
      <Sg,       Masc,  Per2>  => Vp2Sg Masc    ;
      <Sg,       Fem,   Per2>  => Vp2Sg Fem     ;
      <Dl,       _ ,    Per2>  => Vp2Dl         ;
      <Pl,       Masc,  Per2>  => Vp2Pl Masc    ;
      <Pl,       Fem,   Per2>  => Vp2Pl Fem     ;
      <Sg,       _,     Per1>  => Vp1Sg         ;
      <(Dl|Pl),  _,     Per1>  => Vp1Pl

  } ;

-- old
  -- Some old parm, types, and opers
      -- Noun      : Type = {s : State => NNum => Str ; g : Gender; sp : Species} ;

    -- nounTable : ( SgIndefNom, SgIndefAcc, SgDefNom, SgDefAcc,
    --               DlIndefNom, DlIndefAcc, DlDefNom, DlDefAcc,
    --               PlIndefNom, PlIndefAcc, PlDefNom, PlDefAcc : Str) -> Noun
    --   = \SgIndefNom, SgIndefAcc, SgDefNom, SgDefAcc,
    --     DlIndefNom, DlIndefAcc, DlDefNom, DlDefAcc,
    --     PlIndefNom, PlIndefAcc, PlDefNom, PlDefAcc -> {s = table {
    --       Sg => table { Indef => table { Nom => SgIndefNom ; Acc => SgIndefAcc} ;
    --                     Def => table { Nom => SgDefNom ; Acc => SgDefAcc } } ;
    --       Dl => table { Indef => table { Nom => DlIndefNom ; Acc => DlIndefAcc} ;
    --                       Def => table { Nom => DlDefNom ; Acc => DlDefAcc } } ;
    --       Pl => table { Indef => table { Nom => PlIndefNom ; Acc => PlIndefAcc} ;
    --                       Def => table { Nom => PlDefNom ; Acc => PlDefAcc } } };
    --       g = Masc } ;

    -- NNum =    Nsg
    --         | Ndlnom
    --         | Naccmore2   -- noun double and singular in gen and acc are equal
    --         | Nplnom ;
    -- nounTable : ( NsgDef,     NsgIndef,
    --               NdlnomDef,  NdlnomIndef,
    --               Naccmore2Def,  Naccmore2Indef,
    --               NplnomDef,     NplnomIndef  : Str ) -> Gender -> Species -> Noun
    --   = \NsgDef, NsgIndef, NdlnomDef,  NdlnomIndef, Naccmore2Def,  Naccmore2Indef,
    --       NplnomDef, NplnomIndef, gender, species -> {s = table {
    --         Def   =>  table { Nsg => NsgDef ;
    --                           Ndlnom => NdlnomDef ;
    --                           Naccmore2 => Naccmore2Def ;
    --                           Nplnom => NplnomDef };
    --         Indef =>  table { Nsg => NsgIndef ;
    --                           Ndlnom => NdlnomIndef ;
    --                           Naccmore2 => Naccmore2Indef ;
    --                           Nplnom => NplnomIndef } }; g = gender; sp = species } ;



}
