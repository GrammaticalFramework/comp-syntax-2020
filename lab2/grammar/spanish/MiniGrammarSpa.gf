--# -path=.:../abstract
concrete MiniGrammarSpa of MiniGrammar = open MiniResSpa, Prelude in {


  lincat
    V = Verb ;
    V2 = Verb ** {c : Str} ;
    VS = Verb ;
    VV = Verb ; ---- only VV to VP
    A = Adjective ;
    N = Noun ;
    Pron = {
      s : Case => Str ; 
      a : NPAgreement ; -- NOTE: it's number-gender
    } ;
    Prep = {s : Str} ;
    Adv = {s : Str} ;
    IAdv = {s : Str} ; -- interrogative

  lin
    -- | PRONOUNS
    UsePron p = { s = (p.s) ! Nom } ; -- as NP, TODO: check if correct, Eng is UsePron p = p  
    -- TODO: ? gender of we you etc.
    -- TODO: ? gender and number of genitive
    i_Pron = {
      s = table {
        Nom => "yo" ; 
        Acc => "me" ;
        Dat => "me" ;
        Gen => "mìo" ;
        Pre => "mì"
      } ;
      a = NPAgr Sg P1 ;
      } ;

    youSg_Pron = {
      s = table {
        Nom => "tù" ; 
        Acc => "te" ;
        Dat => "te" ;
        Gen => "tuyo" ;
        Pre => "ti"
      } ;
      a = NPAgr Sg P2 ;
    } ;

    he_Pron = {
      s = table {
        Nom => "él" ; 
        Acc => "lo" ;
        Dat => "le" ;
        Gen => "suyo" ; 
        Pre => "se"
      } ;
      a = NPAgr Sg P3
    } ;

    she_Pron = {
      s = table {
        Nom => "ella" ; 
        Acc => "la" ;
        Dat => "le" ;
        Gen => "suyo" ; 
        Pre => "se"
      } ;
      a = NPAgr Sg P3
    } ;

      -- it_Pron missing in AST

    we_Pron = {
      s = table {
        Nom => "nosotros" ;
        Acc => "nos" ;
        Dat => "nos" ;
        Gen => "nuestro" ;
        Pre => "nosotros"
      } ;
      a = NPAgr Pl P1
    } ;

    youPl_Pron = {
      s = table {
        Nom => "vosotros" ;
        Acc => "vos" ;
        Dat => "vos" ;
        Gen => "vuestro" ; 
        Pre => "vosotros"
      } ;
      a = NPAgr Pl P2
    } ;

    they_Pron = {
      s = table {
        Nom => "ellos" ;
        Acc => "los" ;
        Dat => "les" ;
        Gen => "suyo" ; 
        Pre => "se"
      } ;
      a = NPAgr Pl P3
    } ;

    -- | PREPOSITIONS
    -- TODO: PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    in_Prep = {s = "en"} ;
    on_Prep = {s = "sobre"} ;
    with_Prep = {s = "con"} ;

    -- | ADVERBS
    UttAdv adv = adv ;
    UttIAdv iadv = iadv ;
    CompAdv adv = adv ;
    AdvVP vp adv = vp ** {compl = vp.compl ++ adv.s} ;

    where_IAdv = {s = "dònde"} ;
    why_IAdv = {s = "por qué"} ;
}
