concrete DoctorDanJonas of Doctor =
  open
    SyntaxDan,
    ParadigmsDan,
    Prelude
  in {

-- application using standard RGL

lincat
  Phrase = Utt ;
  Fact = Cl ;
  Action = VP ;
  Property = VP ;
  Profession = CN ;
  Person = NP ;
  Place = {at,to : Adv} ;
  Substance = NP ;
  Illness = NP ;

lin
  presPosPhrase fact = mkUtt (mkS fact) ;
  presNegPhrase fact = mkUtt (mkS negativePol fact) ;
  pastPosPhrase fact = mkUtt (mkS anteriorAnt fact) ;
  pastNegPhrase fact = mkUtt (mkS anteriorAnt negativePol fact) ;
  --presQuestionPhrase fact = mkUtt (mkQS (mkQCl fact)) ;
  --pastQuestionPhrase fact = mkUtt (mkQS anteriorAnt (mkQCl fact)) ;
  presQuestionPhrase fact = let p : Utt = mkUtt (mkQS (mkQCl fact)) in p ** {s = p.s ++ SOFT_BIND ++ "?"} ;
  pastQuestionPhrase fact = let p : Utt = mkUtt (mkQS anteriorAnt (mkQCl fact)) in p ** {s = p.s ++ SOFT_BIND ++ "?"} ;


  impPosPhrase action = mkUtt (mkImp action) ;
  impNegPhrase action = mkUtt negativePol (mkImp action) ;

  actionFact person action = mkCl person action ;
  propertyFact person property = mkCl person property ;

  isProfessionProperty profession = mkVP (mkNP a_Det profession) ;
  needProfessionProperty profession = mkVP need_V2 (mkNP a_Det profession) ;
  isAtPlaceProperty place = mkVP place.at ;
  haveIllnessProperty illness = mkVP have_V2 illness ;

  theProfessionPerson profession = mkNP the_Det profession ;

  iMascPerson = i_NP ;
  iFemPerson = i_NP ;
  youMascPerson = you_NP ;
  youFemPerson = you_NP ;
  hePerson = he_NP ;
  shePerson = she_NP ;

  goToAction place = mkVP (mkVP go_V) place.to ;
  stayAtAction place = mkVP (mkVP stay_V) place.at ;
  vaccinateAction person = mkVP vaccinate_V2 person ;
  examineAction person = mkVP examine_V2 person ;
  takeSubstanceAction substance = mkVP take_V2 substance ;

-- end of what could be a functor
--------------------------------

  coughAction = mkVP (mkV "hoste") ;
  breatheAction = mkVP (mkV "trækker vej") ;
  vomitAction = mkVP (mkV "kaste" "op") ;
  sleepAction = mkVP (mkV "sove" "sov" "sovet") ;
  undressAction = mkVP (mkVP take_V2 (mkNP (mkN "tøjet"))) (pAdv "af") ;
  dressAction = mkVP (mkVP put_V2 (mkNP (mkN "tøjet"))) (pAdv "på") ;
  eatAction = mkVP (mkV "spise" "spiste" "spist") ;
  drinkAction = mkVP (mkV "drikke" "drak" "drukket") ;
  smokeAction = mkVP (mkV "ryge" "røg" "røget") ;
  measureTemperatureAction = mkVP (mkV2 (mkV "tage")) (mkNP the_Det (mkN "kropstemperatur")) ;
  measureBloodPressureAction = mkVP (mkV2 (mkV "tage")) (mkNP the_Det (mkN "blodtrykk")) ;

  hospitalPlace = {at = pAdv "på hospitalet" ; to = pAdv "til hospitalet"} ;
  homePlace = {at = pAdv "hjem" ; to = pAdv "hjem"} ;
  schoolPlace = {at = pAdv "i skole" ; to = pAdv "til skole"} ;
  workPlace = {at = pAdv "på arbejde" ; to = pAdv "til arbejde"} ;

  doctorProfession = mkCN (mkN "doktor") ;
  nurseProfession = mkCN (mkN "sygeplejerske") ;
  interpreterProfession = mkCN (mkN "tolk") ;

  bePregnantProperty = mkVP (mkA "gravid") ;
  beIllProperty = mkVP (mkA "syg") ;
  beWellProperty = mkVP (mkA "rask") ;
  beDeadProperty = mkVP (mkA "død") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "allergi")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "smerte")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP a_Det (mkN "barn" "børn")) ;

  feverIllness = mkNP a_Det (mkN "feber") ;
  fluIllness = mkNP a_Det (mkN "influenza") ;
  headacheIllness = mkNP a_Det (mkN "hovedpine") ;
  diarrheaIllness = mkNP a_Det (mkN "diarre") ;
  heartDiseaseIllness = mkNP a_Det (mkN "hjertesygdom") ;
  lungDiseaseIllness = mkNP a_Det (mkN "lungesygdom") ;
  hypertensionIllness = mkNP (mkN "hypertension") ;

  alcoholSubstance = mkNP (mkN "alkohol") ;
  medicineSubstance = mkNP (mkN "medicin") ;
  drugsSubstance = mkNP aPl_Det (mkN "stoffer") ;

oper
  pAdv : Str -> Adv = ParadigmsDan.mkAdv ;

  go_V = mkV "gå" "gik" "gået" ;
  stay_V = mkV "blive" ;
  need_V2 = mkV2 (mkV "mangle") ;
  take_V2 = mkV2 (mkV "tage" "tog" "taget") ;
  put_V2 = mkV2 (mkV "tage" "tog" "taget") ;
  vaccinate_V2 = mkV2 (mkV "vaccinere") ;
  examine_V2 = mkV2 (mkV "undersøge") ;
  havde_v = mkV "har" "havde" "haft" ;

}
