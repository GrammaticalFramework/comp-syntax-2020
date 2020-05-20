concrete QueryEng of Query =

open
  SyntaxEng, ParadigmsEng, SymbolicEng
in {

lincat
  Query = Utt ;
  Kind = CN ;
  Property = AP ;
  Term = NP ;
  Element = NP ;

lin
  QWhich kind property = mkUtt (mkQS (mkQCl (mkIP whichPl_IDet kind) property)) ;
  QWhether term property = mkUtt (mkQS (mkQCl (mkCl term property))) ;
  QWhat element = mkUtt (mkQS (mkQCl what_IP element)) ;

  TAll kind = mkNP all_Predet (mkNP aPl_Det kind) ;
  TAny kind = mkNP someSg_Det kind ;
  TElement element = element ;
  
  PAnd p q = mkAP and_Conj p q ;
  POr p q = mkAP or_Conj p q ;
  PNot p = mkAP (lin AdA {s = "not"}) p ; ---

  KProperty property kind = mkCN property kind ;

-- lexicon

  KNumber = mkCN (mkN "number") ;
  EInteger i = symb i ;
  PEven = mkAP (mkA "even") ;
  POdd = mkAP (mkA "odd") ;
  PPrime = mkAP (mkA "prime") ;
  PDivisible term = mkAP (mkA2 (mkA "divisible") by8means_Prep) term ;
  PSmaller term = mkAP (mkA "small") term ;
  PGreater term = mkAP (mkA "great") term ;
  PBetween x y = mkAP (mkA2 (mkA "between") (mkPrep "")) (mkNP and_Conj x y) ; ---

  ESum x y = mkNP the_Det (mkCN (mkN2 (mkN "sum")) (mkNP and_Conj x y)) ;
  EProduct x y = mkNP the_Det (mkCN (mkN2 (mkN "product")) (mkNP and_Conj x y)) ;
  EFactorial x = mkNP the_Det (mkCN (mkN2 (mkN "factorial")) x) ;

}