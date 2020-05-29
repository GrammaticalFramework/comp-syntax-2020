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
      a : Agreement ; -- NOTE: it's number-gender
      p : Person ;
    } ;

  lin
    --UsePron p = p ; TODO: ?
    i_Pron = {
      s = table {
        Nom => "yo" ; 
        Acc => "me" ;
        Dat => "me" ;
        Gen => "mìo" ;
        Prep => "mì"
      } ;
      a = Agr Sg M ; -- TODO: ? (genitive sucks)
      p = P1
      } ;

      youSg_Pron = {
      s = table {
        Nom => "tù" ; 
        Acc => "te" ;
        Dat => "te" ;
        Gen => "tuyo" ;
        Prep => "ti"
      } ;
      a = Agr Sg M ; -- TODO: ? (genitive sucks)
      p = P2
      } ;

      he_Pron = {
      s = table {
        Nom => "él" ; 
        Acc => "lo" ;
        Dat => "le" ;
        Gen => "suyo" ; 
        Prep => "se"
      } ;
      a = Agr Sg M ; -- TODO: ? (genitive sucks)
      p = P3
      } ;

      she_Pron = {
      s = table {
        Nom => "ella" ; 
        Acc => "la" ;
        Dat => "le" ;
        Gen => "suyo" ; 
        Prep => "se"
      } ;
      a = Agr Sg M ; -- TODO: ? (genitive sucks)
      p = P3
      } ;

      -- it_Pron

      we_Pron = {
      s = table {
        Nom => "nosotros" ; -- TODO: gender 
        Acc => "nos" ;
        Dat => "nos" ;
        Gen => "nuestro" ;
        Prep => "nosotros"
      } ;
      a = Agr Pl M ; -- TODO: ? (genitive sucks)
      p = P1
      } ;

      youPl_Pron = {
      s = table {
        Nom => "vosotros" ; -- TODO: gender 
        Acc => "vos" ;
        Dat => "vos" ;
        Gen => "vuestro" ; 
        Prep => "vosotros"
      } ;
      a = Agr Pl M ; -- TODO: ? (genitive sucks)
      p = P2
      } ;

      they_Pron = {
      s = table {
        Nom => "ellos" ; -- TODO: gender 
        Acc => "los" ;
        Dat => "les" ;
        Gen => "suyo" ; 
        Prep => "se"
      } ;
      a = Agr Pl M ; -- TODO: ? (genitive sucks)
      p = P3
      } ;
}
