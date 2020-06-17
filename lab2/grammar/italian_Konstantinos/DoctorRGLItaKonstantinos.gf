concrete DoctorRGLItaKonstantinos of Doctor =
  open
    SyntaxIta,
    ParadigmsIta,
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
  -- presQuestionPhrase fact = mkUtt (mkQS (mkQCl fact)) ;
  -- pastQuestionPhrase fact = mkUtt (mkQS anteriorAnt (mkQCl fact)) ;
  presQuestionPhrase fact = let p : Utt = mkUtt (mkQS (mkQCl fact)) in p ** {s = p.s ++ SOFT_BIND ++ "?"} ;
  pastQuestionPhrase fact = let p : Utt = mkUtt (mkQS anteriorAnt (mkQCl fact)) in p ** {s = p.s ++ SOFT_BIND ++ "?"} ;


  impPosPhrase action = mkUtt politeImpForm (mkImp action) ;
  impNegPhrase action = mkUtt politeImpForm negativePol (mkImp action) ;

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

  coughAction = mkVP (mkV "tossire") ;
  breatheAction = mkVP (mkV "respirare") ;
  vomitAction = mkVP (mkV "vomitare") ;
  sleepAction = mkVP (mkV "dormire") ;
  undressAction = mkVP (mkV "spogliare") ;
  dressAction = mkVP (mkV "vestire") ;
  eatAction = mkVP (mkV "mangiare") ; --mangiare
  drinkAction = mkVP (mkV "bere") ;
  --drinkAction = mkVP (mkV "bere" "bevo" "beviamo" "bevi" "bevete" "beve" "bevono") ; -- Unfortunately, adding all the forms of the irregular verbs doesn't work with my grammar. 
  smokeAction = mkVP (mkV "fumare") ;
  measureTemperatureAction = mkVP (mkV2 (mkV "misurare")) (mkNP the_Det (mkN "temperatura corporea")) ;
  measureBloodPressureAction = mkVP (mkV2 (mkV "misurare")) (mkNP the_Det (mkN "pressione sanguigna")) ;

  hospitalPlace = {at = pAdv "all'ospedale" ; to = pAdv "all'ospedale"} ;
  homePlace = {at = pAdv "a casa" ; to = pAdv "a casa"} ;
  schoolPlace = {at = pAdv "a scuola" ; to = pAdv "a scuola"} ;
  workPlace = {at = pAdv "al lavoro" ; to = pAdv "al lavoro"} ;

  doctorProfession = mkCN (mkN "medico") ;
  nurseProfession = mkCN (mkN "infermiera") ;
  interpreterProfession = mkCN (mkN "interprete") ;

  bePregnantProperty = mkVP (mkA "incinta") ;
  beIllProperty = mkVP (mkA "malato") ;
  beWellProperty = mkVP (mkA "bene") ;
  beDeadProperty = mkVP (mkA "morto") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "allergia")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "dolore")) ; --dolore masc
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "bambino")) ;

  feverIllness = mkNP a_Det (mkN "febbre") ; --fem
  fluIllness = mkNP a_Det (mkN "influenza") ;
  headacheIllness = mkNP a_Det (mkN "mal di testa") ;
  diarrheaIllness = mkNP a_Det (mkN "diarrea") ;
  heartDiseaseIllness = mkNP a_Det (mkN "cardiopatia") ;
  lungDiseaseIllness = mkNP a_Det (mkN "malattia polmonare") ;
  hypertensionIllness = mkNP (mkN "ipertensione") ; --fem

  alcoholSubstance = mkNP (mkN "alcool") ;
  medicineSubstance = mkNP a_Det (mkN "farmaco") ;
  drugsSubstance = mkNP aPl_Det (mkN "droga") ;

oper
  pAdv : Str -> Adv = ParadigmsIta.mkAdv ;

  go_V = mkV "partire" ;
  stay_V = mkV "rimanere" ;
  need_V2 = mkV2 (mkV "richiedere") ;
  take_V2 = mkV2 (mkV "prendere") ;
  put_V2 = mkV2 (mkV "mettere") ;
  vaccinate_V2 = mkV2 (mkV "vaccinare") ;
  examine_V2 = mkV2 (mkV "esaminare") ;

}