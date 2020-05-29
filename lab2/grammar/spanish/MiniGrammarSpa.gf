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

  lin
    --UsePron p = p ; TODO: ?
    -- TODO: ? gender of we you etc.
    -- TODO: ? gender and number of genitive
    i_Pron = {
      s = table {
        Nom => "yo" ; 
        Acc => "me" ;
        Dat => "me" ;
        Gen => "mìo" ;
        Prep => "mì"
      } ;
      a = NPAgr Sg P1 ;
      } ;

      youSg_Pron = {
      s = table {
        Nom => "tù" ; 
        Acc => "te" ;
        Dat => "te" ;
        Gen => "tuyo" ;
        Prep => "ti"
      } ;
      a = NPAgr Sg P2 ;
      } ;

      he_Pron = {
      s = table {
        Nom => "él" ; 
        Acc => "lo" ;
        Dat => "le" ;
        Gen => "suyo" ; 
        Prep => "se"
      } ;
      a = NPAgr Sg P3
      } ;

      she_Pron = {
      s = table {
        Nom => "ella" ; 
        Acc => "la" ;
        Dat => "le" ;
        Gen => "suyo" ; 
        Prep => "se"
      } ;
      a = NPAgr Sg P3
      } ;

      -- it_Pron

      we_Pron = {
      s = table {
        Nom => "nosotros" ;
        Acc => "nos" ;
        Dat => "nos" ;
        Gen => "nuestro" ;
        Prep => "nosotros"
      } ;
      a = NPAgr Pl P1
      } ;

      youPl_Pron = {
      s = table {
        Nom => "vosotros" ;
        Acc => "vos" ;
        Dat => "vos" ;
        Gen => "vuestro" ; 
        Prep => "vosotros"
      } ;
      a = NPAgr Pl P2
      } ;

      they_Pron = {
      s = table {
        Nom => "ellos" ; -- TODO: gender 
        Acc => "los" ;
        Dat => "les" ;
        Gen => "suyo" ; 
        Prep => "se"
      } ;
      a = NPAgr Pl P3
      } ;
}
