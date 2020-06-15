concrete DoctorSpa of Doctor =
  open
    SyntaxSpa,
    ParadigmsSpa,
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
  presQuestionPhrase fact = let p : Utt = mkUtt (mkQS (mkQCl fact)) in p ** {s = "¿" ++ SOFT_BIND ++ p.s ++ SOFT_BIND ++ "?"} ;
  pastQuestionPhrase fact = let p : Utt = mkUtt (mkQS anteriorAnt (mkQCl fact)) in p ** {s = "¿" ++ SOFT_BIND ++ p.s ++ SOFT_BIND ++ "?"} ;


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

  coughAction = mkVP (mkV "toser") ;
  breatheAction = mkVP (mkV "respirar") ;
  vomitAction = mkVP (mkV "vomitar") ;
  sleepAction = mkVP (mkV "dormir" "duermo") ;
  --undressAction = mkVP (mkVP take_V2 (mkNP thePl_Det (mkN "ropa"))) (pAdv "off") ;
  --dressAction = mkVP (mkVP put_V2 (mkNP thePl_Det (mkN "clothe"))) (pAdv "on") ;
  undressAction = mkVP take_V2 (mkNP theSg_Det (mkN "ropa")) ;
  dressAction = mkVP put_V2 (mkNP theSg_Det (mkN "ropa")) ;
  eatAction = mkVP (mkV "comer") ;
  drinkAction = mkVP (mkV "beber") ;
  smokeAction = mkVP (mkV "fumar") ;
  measureTemperatureAction = mkVP (mkV2 (mkV "medir" "mido")) (mkNP the_Det (mkN "temperatura corporal" feminine)) ; --fixed the gender of this NP which is feminine
  measureBloodPressureAction = mkVP (mkV2 (mkV "medir" "mido")) (mkNP the_Det (mkN "presión arterial" feminine)) ;

  hospitalPlace = {at = pAdv "en el hospital" ; to = pAdv "en el hospital"} ;
  homePlace = {at = pAdv "en casa" ; to = pAdv "a casa"} ;
  schoolPlace = {at = pAdv "en el colegio" ; to = pAdv "en el colegio"} ;
  workPlace = {at = pAdv "en el trabajo" ; to = pAdv "en el trabajo"} ;

  doctorProfession = mkCN (mkN "médico") ;
  nurseProfession = mkCN (mkN "enfermera") ;
  interpreterProfession = mkCN (mkN "interprete") ;

  bePregnantProperty = mkVP (be_VA) (mkAP (mkA "embarazado")) ;  
  beIllProperty = mkVP (be_VA) (mkAP (mkA "enfermo")) ;
  --beWellProperty =  mkVP (mkVA (mkV "estar")) (mkAP (mkA "bien")) ; -- more neat solution below, copula fixed
  beWellProperty =  mkVP (be_VA) (mkAP (mkA "bien")) ; --copula estar
  beDeadProperty = mkVP (be_VA) (mkAP (mkA "muerto")) ;  --copula estar
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "alergia")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "dolor")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "niño")) ;

  feverIllness = mkNP a_Det (mkN "fiebre") ;
  fluIllness = mkNP a_Det (mkN "gripe") ;
  headacheIllness = mkNP a_Det (mkN "jaqueca") ;
  diarrheaIllness = mkNP a_Det (mkN "diarrea") ;
  heartDiseaseIllness = mkNP a_Det (mkN "enfermedades cardíacas") ;
  lungDiseaseIllness = mkNP a_Det (mkN "enfermedad pulmonar") ;
  hypertensionIllness = mkNP (mkN "hipertensión") ;

  alcoholSubstance = mkNP (mkN "alcohol") ;
  medicineSubstance = mkNP a_Det (mkN "medicina") ;
  drugsSubstance = mkNP aPl_Det (mkN "droga") ;

oper
  pAdv : Str -> Adv = ParadigmsSpa.mkAdv ;

  go_V = mkV "ir" "fui";
  stay_V = reflV (mkV "quedar") ;  --reflexive verb fixed
  need_V2 = mkV2 (mkV "necesitar") ;
  take_V2 = mkV2 (reflV (mkV "quitar")) ; --reflexive verb fixed
  put_V2 = mkV2 (reflV (mkV "poner")) ; -- reflexive verb fixed
  be_VA = mkVA (mkV "estar") ;
  vaccinate_V2 = mkV2 ( reflV (mkV "vacunar")) ; --reflexive verb fixed
  examine_V2 = mkV2 (mkV "examinar") ;

}


--- WEIRD ERROR-----
--when it's an affirmative sentence for imperative, the declension is OK, but when its negative the declension is wrong:

--AFFIRMATIVE: quita &+ te la ropa (take the clothes off) CORRECT
--NEGATIVE: no quita &+ te la ropa (don't take the clothes off) INCORECT,   should be: no TE quitES la ropa 

-- Herbert: when he runs my file on his computer he gets the right results. My RGL is updated and everything is up to date.
-- l impPosPhrase undressAction
-- quita &+ te la ropa
-- l impNegPhrase undressAction
-- no te quites la ropa




--- TO RUN -------------------
-- i DoctorSpa.gf - LOAD FILE
-- gr -number=50 | l
-- i -retain DoctorSpa.gf  -- RELOAD


