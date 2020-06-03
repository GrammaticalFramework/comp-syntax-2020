concrete DoctorRGLFreJulia of Doctor =
  open
    SyntaxFre,
    ParadigmsFre,
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

  coughAction = mkVP (mkV "tousser") ;
  breatheAction = mkVP (mkV "respirer") ;
  vomitAction = mkVP (mkV "vomir") ;
  sleepAction = mkVP (mkV "dormir" "dors" "dormons" "dorment" "dormit" "dormira" "dormi") ;
  undressAction = mkVP take_off_V2 (mkNP thePl_Det (mkN "vêtement")) ;
  dressAction = mkVP put_V2 (mkNP thePl_Det (mkN "vêtement")) ;
  eatAction = mkVP (mkV "manger") ;
  drinkAction = mkVP (mkV "boire" "bois" "buvons" "boivent" "but" "boira" "bu") ;
  smokeAction = mkVP (mkV "fumer") ;
  measureTemperatureAction = mkVP (mkV2 (mkV "mesurer")) (mkNP the_Det (mkN "température du corps" feminine)) ;
  measureBloodPressureAction = mkVP (mkV2 (mkV "mesurer")) (mkNP the_Det (mkN "tension artérielle")) ;

  hospitalPlace = {at = pAdv "à l'hôpital" ; to = pAdv "à l'hôpital"} ;
  homePlace = {at = pAdv "à la maison" ; to = pAdv "à la maison"} ;
  schoolPlace = {at = pAdv "à l'école" ; to = pAdv "à l'école"} ;
  workPlace = {at = pAdv "au travail" ; to = pAdv "au travail"} ;

  doctorProfession = mkCN (mkN "médecin") ;  
  nurseProfession = mkCN (mkN "infirmière") ; 
  interpreterProfession = mkCN (mkN "interprète") ; 

  bePregnantProperty = mkVP (mkA "enceint") ;
  beIllProperty = mkVP (mkA "malade") ;
  beWellProperty = mkVP (mkA "en bonne santé") ;
  beDeadProperty = mkVP (mkA "mort") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "allergie")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "douleur")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "enfant")) ;

  feverIllness = mkNP (mkN "fièvre") ;
  fluIllness = mkNP a_Det (mkN "grippe") ;
  headacheIllness = mkNP a_Det (mkN "mal à la tête") ; 
  diarrheaIllness = mkNP (mkN "diarrhée") ;
  heartDiseaseIllness = mkNP a_Det (mkN "maladie cardiaque") ;
  lungDiseaseIllness = mkNP a_Det (mkN "maladie pulmonaire") ;
  hypertensionIllness = mkNP (mkN "hypertension") ;

  alcoholSubstance = mkNP (mkN "alcool") ;
  medicineSubstance = mkNP a_Det (mkN "médicament") ;
  drugsSubstance = mkNP aPl_Det (mkN "drogue") ;

oper
  pAdv : Str -> Adv = ParadigmsFre.mkAdv ;

  go_V = etreV (mkV "aller" "vais" "allons" "vont" "alla" "ira" "allé") ;
  stay_V = etreV (mkV "rester") ;
  need_V2 = mkV2 (mkV "nécessiter") ;
  take_V2 = mkV2 (mkV "prendre" "prends" "prenons" "prennent" "prit" "prendra" "pris") ;
  take_off_V2 = mkV2 (mkV "retirer") ;
  put_V2 = mkV2 (mkV "mettre" "mets" "mettons" "mettent" "mit" "mettra" "mis") ;
  vaccinate_V2 = mkV2 (mkV "vacciner") ;
  examine_V2 = mkV2 (mkV "examiner") ;

}
