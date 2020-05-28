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
      g = Agr Sg M ; -- TODO: ?
      p = P1
      } ;
}
