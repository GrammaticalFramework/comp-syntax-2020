concrete DoctorChiFang of Doctor =
  open
    SyntaxChi,
    ParadigmsChi,
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

  coughAction = mkVP (mkV "咳嗽") ;
  breatheAction = mkVP (mkV "呼吸") ;
  vomitAction = mkVP (mkV "呕吐") ;
  sleepAction = mkVP (mkV "睡觉") ;
  undressAction = mkVP (mkVP take_V2 (mkNP thePl_Det (mkN "衣服"))) (pAdv "脱") ;
  dressAction = mkVP (mkVP put_V2 (mkNP thePl_Det (mkN "衣服"))) (pAdv "穿") ;
  eatAction = mkVP (mkV "吃") ;
  drinkAction = mkVP (mkV "喝") ;
  smokeAction = mkVP (mkV "抽烟") ;
  measureTemperatureAction = mkVP (mkV2 (mkV "量")) (mkNP the_Det (mkN "体温")) ;
  measureBloodPressureAction = mkVP (mkV2 (mkV "量")) (mkNP the_Det (mkN "血压")) ;

  hospitalPlace = {at = pAdv "在医院" ; to = pAdv "去医院"} ;
  homePlace = {at = pAdv "在家" ; to = pAdv "回家"} ;
  schoolPlace = {at = pAdv "在学校" ; to = pAdv "去学校"} ;
  workPlace = {at = pAdv "在公司" ; to = pAdv "去工作"} ;

  doctorProfession = mkCN (mkN "医生") ;
  nurseProfession = mkCN (mkN "护士") ;
  interpreterProfession = mkCN (mkN "翻译人员") ;

  bePregnantProperty = mkVP (mkA "怀孕") ;
  beIllProperty = mkVP (mkA "生病") ;
  beWellProperty = mkVP (mkA "状况良好") ;
  beDeadProperty = mkVP (mkA "死亡") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "过敏")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "头痛")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "孩子")) ;

  feverIllness = mkNP a_Det (mkN "发烧") ;
  fluIllness = mkNP a_Det (mkN "流感") ;
  headacheIllness = mkNP a_Det (mkN "头痛") ;
  diarrheaIllness = mkNP a_Det (mkN "腹泻") ;
  heartDiseaseIllness = mkNP a_Det (mkN "心脏病") ;
  lungDiseaseIllness = mkNP a_Det (mkN "肺病") ;
  hypertensionIllness = mkNP (mkN "高血压") ;

  alcoholSubstance = mkNP (mkN "酒精") ;
  medicineSubstance = mkNP a_Det (mkN "药") ;
  drugsSubstance = mkNP aPl_Det (mkN "药") ;

oper
  pAdv : Str -> Adv = ParadigmsChi.mkAdv ;

  go_V = mkV "去" ;
  stay_V = mkV "留" ;
  need_V2 = mkV2 "需要" ;
  take_V2 = mkV2 "消耗" ;
  put_V2 = mkV2 "用" ;
  vaccinate_V2 = mkV2 "接种疫苗" ;
  examine_V2 = mkV2 "检查" ;

}
