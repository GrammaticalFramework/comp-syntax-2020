resource MicroResChiFang = open Prelude in {

param
  AdvType = ATPlace Bool | ATTime | ATManner | ATPoss ; -- ATPlace True = has zai_s already
  Number = Sg | Pl ;
  DetType = DTFull Number | DTNum | DTPoss ;  -- this | these, five, our
  NumType = NTFull | NTVoid Number ;          -- five | sg, pl

oper

  ge_s = "一个" ;
  neg_s = "不" ;
  xie_s = "些" ;
  de_s = "的" ;

-- Determiner

  Determiner = {s : Str ; detType : DetType} ;

  mkDet = overload {
    mkDet : Str ->            Determiner = \s   -> {s = s ; detType = DTFull Sg} ;
    mkDet : Str -> Number  -> Determiner = \s,n -> {s = s ; detType = DTFull n} ;
    mkDet : Str -> DetType -> Determiner = \s,d -> {s = s ; detType = d} ;
  } ;


-- Nouns

  Noun : Type = {s : Str ; c : Str};

  regNoun : Str -> Str -> Noun = \s,c -> {s = s ; c = c};

  mkN = overload {
    mkN : (man : Str) -> Noun 
      = \n -> lin N (regNoun n ge_s) ;  
    mkN : (man : Str) -> Str -> Noun 
      = \n,c -> lin N (regNoun n c)
  } ; 


-- Verbs

  Verb : Type = {s,sn : Str ; pp,ds,dp,ep : Str ; neg : Str} ;

  regVerb : (walk : Str) -> Verb = \v -> mkVerb v "了" "着" "在" "过" "不" ;

  noVerb : Verb = regVerb [] ;

  mkVerb : (v : Str) -> (pp,ds,dp,ep,neg : Str) -> Verb = \v,pp,ds,dp,ep,neg -> 
    {s,sn = v ; pp = pp ; ds = ds ; dp = dp ; ep = ep ; neg = neg} ;   

  mkV = overload {      
    mkV : (walk : Str) -> Verb 
      = \walk -> case walk of {
          v + "+" + p => lin V (regVerb (v + p)) ;
          _ => lin V (regVerb walk)
          } ;
    mkV : (walk,out : Str) -> Verb 
      = \v,p -> lin V (regVerb (v + p)) ; ----
    mkV : (arrive : Str) -> Str -> Str -> Str -> Str -> Verb
      = \arrive,pp,ds,dp,ep -> lin V (mkVerb arrive pp ds dp ep neg_s) ;
    mkV : (arrive : Str) -> Str -> Str -> Str -> Str -> Str -> Verb
      = \arrive,pp,ds,dp,ep,neg -> lin V (mkVerb arrive pp ds dp ep neg) ;
  } ;


-- Adjective
  
  Adjective : Type = {s : Str ; monoSyl: Bool} ;
  
  mkAdj : Str -> Bool -> Adjective = \s,b -> {s = s ; monoSyl = b};

  simpleAdj : Str -> Adjective = \s -> case s of {
    ? => mkAdj s True ; -- monosyllabic
    _ => mkAdj s False
  } ;

  mkA = overload {
    mkA : (small : Str) -> Adjective 
      = \a -> lin Adjective (simpleAdj a) ;
    mkA : (small : Str) -> Bool -> Adjective 
      = \a,b -> lin Adjective (mkAdj a b) ;
  } ;

-- Adverb
  
  Adverb : Type = {s : Str} ;
  mkAdv : Str -> {s : Str}  ;
  mkAdv str = {s = str} ;

  getAdvType : Str -> AdvType = \s -> case s of {
    "的"     => ATPoss ;
    "在" + _ => ATPlace True ; -- certain that True
    _ => ATPlace False         -- uncertain whether ATPlace
  } ;

  advTypeHasDe : AdvType -> Bool = \at -> case at of {
      ATPoss => True ;
      _ => False
  } ;


-- Preposition
  
  Preposition = {prepPre : Str ; prepPost : Str ; advType : AdvType ; hasDe : Bool} ;

  mkPreposition : Str -> Str -> AdvType -> Preposition = \s1,s2,at -> {
    prepPre  = s1 ;
    prepPost = s2 ;
    advType  = at ;
    hasDe = advTypeHasDe at ;
  } ;

  emptyPrep : Preposition = mkPrep [] ;

  mkPrep = overload { -- first pre part, then optional post part
    mkPrep : Str -> Preposition 
     = \s -> lin Preposition (mkPreposition s [] (getAdvType s)) ;
    mkPrep : Str -> Str -> Preposition 
     = \s,t -> lin Preposition (mkPreposition s t (getAdvType s)) ;
    mkPrep : Str -> Str -> AdvType -> Preposition 
     = \s,t,a -> lin Preposition (mkPreposition s t a) ;
  } ;

}
