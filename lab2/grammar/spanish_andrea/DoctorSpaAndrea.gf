concrete DoctorSpaAndrea of Doctor =
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
  
  -- Question marks before and after
  presQuestionPhrase fact = let p : Utt = mkUtt (mkQS (mkQCl fact)) in p ** {s = "¿" ++ SOFT_BIND ++ p.s ++ SOFT_BIND ++ "?"} ;
  pastQuestionPhrase fact = let p : Utt = mkUtt (mkQS anteriorAnt (mkQCl fact)) in p ** {s = "¿" ++ SOFT_BIND ++ p.s ++ SOFT_BIND ++ "?"} ;


  impPosPhrase action = mkUtt (mkImp action) ;
  impNegPhrase action = mkUtt negativePol (mkImp action) ;

  actionFact person action = mkCl person action ;
  propertyFact person property = mkCl person property ;

  isProfessionProperty profession = mkVP profession ;
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
  -- undressAction = mkVP quitarse_V2 la_ropa_NP ;
  -- dressAction = mkVP ponerse_V2 la_ropa_NP ;
  undressAction = mkVP desvestirse_V ;
  dressAction = mkVP vestirse_V ;
  eatAction = mkVP (mkV "comer") ;
  drinkAction = mkVP (mkV "beber") ;
  smokeAction = mkVP (mkV "fumar") ;
  -- measureTemperatureAction = mkVP (mkV2 (mkV "medir" "mido")) (mkNP the_Det (mkN "temperatura corporal" feminine)) ;
  -- measureBloodPressureAction = mkVP (mkV2 (mkV "medir" "mido")) (mkNP the_Det (mkN "presión arterial" feminine)) ;
  measureBloodPressureAction = mkVP (mkV2 (mkV "medir" "mido")) (mkNP the_Det (mkCN (mkA "arterial") (mkN "presión"))) ;
  measureTemperatureAction = mkVP (mkV2 (mkV "medir" "mido")) (mkNP the_Det (mkCN (mkA "corporal") (mkN "temperatura"))) ;

  hospitalPlace = {at = pAdv "en el hospital" ; to = pAdv "al hospital"} ;
  homePlace = {at = pAdv "en casa" ; to = pAdv "a casa"} ;
  schoolPlace = {at = pAdv "en el colegio" ; to = pAdv "al colegio"} ;
  workPlace = {at = pAdv "en el trabajo" ; to = pAdv "al trabajo"} ;

  doctorProfession = mkCN (mkN "doctor") ; -- 'doctor' (masc) / 'doctora' (fem)
  nurseProfession = mkCN (mkN "enfermera") ; -- 'enfermera' (fem) / 'enfermero' (masc)
  interpreterProfession = mkCN (mkN "intérprete") ;
  
  bePregnantProperty = mkVP (be_VA) (mkAP (mkA "embarazada")) ;
  beIllProperty = mkVP (be_VA) (mkAP (mkA "enfermo")) ; -- 'enfermo' (masc) / 'enferma' (fem)
  beWellProperty = mkVP (be_VA) (mkAP (mkA "bien")) ;
  beDeadProperty = mkVP (be_VA) (mkAP (mkA "muerto")) ; -- 'muerto' (masc) / 'muerta' (fem)
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "alergia")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "dolor")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "niño")) ; 
  -- The literal translation of "children" would be "niños" in Spanish. However, if I write the plural form (niños)
  -- then it thinks the word is in the singular form and it ends up giving me "niñoses" 
  -- as the plural form for "niño" because it's following the rules for pluralization of nouns.
  
  -- haveHeadacheProperty = mkVP have_V2 (mkNP (mkN "dolor de cabeza")) ; -- it would be a property instead of an illness

  feverIllness = mkNP (mkN "fiebre") ;
  fluIllness = mkNP a_Det (mkN "gripe" feminine) ;
  headacheIllness = mkNP a_Det (mkN "jaqueca") ; -- 'dolor de cabeza' is also possible, but it would probably be more common as a property instead (as shown above)
  diarrheaIllness = mkNP (mkN "diarrea") ;
  heartDiseaseIllness = mkNP a_Det (mkN "cardiopatía") ;
  lungDiseaseIllness = mkNP a_Det (mkCN (mkA "pulmonar") (mkN "enfermedad" feminine)) ;
  hypertensionIllness = mkNP (mkN "hipertensión") ;

  alcoholSubstance = mkNP (mkN "alcohol") ;
  medicineSubstance = mkNP a_Det (mkN "medicamento") ;
  drugsSubstance = mkNP aPl_Det (mkN "droga") ;

oper
  pAdv : Str -> Adv = ParadigmsSpa.mkAdv ;

  go_V = mkV "ir" ;
  stay_V = reflV (mkV "quedar") ;
  need_V2 = mkV2 (mkV "necesitar") ;
  take_V2 = mkV2 (mkV "tomar") ;
  put_V2 = mkV2 (mkV "poner") ;
  vaccinate_V2 = mkV2 (mkV "vacunar") ;
  examine_V2 = mkV2 (mkV "examinar") ;
  be_VA = mkVA (mkV "estar") ;
  vestirse_V = reflV (mkV "vestir" "visto") ;
  desvestirse_V = reflV (mkV "desvestir" "desvisto") ;
  -- quitarse_V2 = mkV2 (reflV (mkV "quitar")) ;
  -- ponerse_V2 = mkV2 (reflV (mkV "poner")) ;
  -- la_ropa_NP = mkNP the_Det (mkN "ropa") ;
}



-- DRESS AND UNDRESS:
-- Actions 'dress' and 'undress' might be translated into 'vestirse' and 'desvestirse' respectively, 
-- where they act as reflexive verbs. In addition, verbs 'dress' and 'undress' can also be translated as 
-- 'ponerse la ropa' and 'quitarse la ropa'. 

-- This last translation would demand the verbs to take the NP 'la ropa' ('the clothes'), resulting in:
  -- undressAction = mkVP quitarse_V2 la_ropa_NP
  -- dressAction = mkVP ponerse_V2 la_ropa_NP
-- To make this possible I've added 3 new opers: quitarse_V2, ponerse_V2 and la_ropa_NP


-- MEASURING:
-- Regarding functions "measureBloodPressureAction" and "measureTemperatureAction":
-- When parsing sentences such as "el doctor mide la presión arterial" or "el doctor mide la temperatura corporal",
-- both mkN "presión arterial" and "temperatura corporal", were considered to be masculine nouns.
-- An easy way to change the gender and temporarily solve the problem would be setting the gender to feminine, like below:
  -- measureBloodPressureAction = mkVP (mkV2 (mkV "medir" "mido")) (mkNP the_Det (mkN "presión arterial" feminine)) ;
  -- measureTemperatureAction = mkVP (mkV2 (mkV "medir" "mido")) (mkNP the_Det (mkN "temperatura corporal" feminine)) ;

-- However, this is not fixing the problem since "temperatura corporal" / "presión arterial" are actually 
-- a common noun without a determiner (mkCN) of the type:	A -> N -> CN
-- For that reason, we could change our original functors to:
  -- measureBloodPressureAction = mkVP (mkV2 (mkV "medir" "mido")) (mkNP the_Det (mkCN (mkA "arterial") (mkN "presión")))
  -- measureTemperatureAction = mkVP (mkV2 (mkV "medir" "mido")) (mkNP the_Det (mkCN (mkA "corporal") (mkN "temperatura")))


-- DEFINITE ARTICLES:
-- Definite articles in Spanish can be feminine or masculine and they also have
-- singular and plural forms in agreement with the noun they refer to:
    -- FemSing: la
    -- MascSing: el
    -- FemPl: las 
    -- MascPl: los

-- When the preposition 'a' ('to') comes before the definite article 'el' (the - MascSing), 
-- they combine to form the contraction 'al' (to the). The same rule applies for preposition 'de' 
-- ('of' / 'from') when it appears before 'el', resulting in 'del'.
  -- Ex.: to the hospital -> a + el hospital = al hospital



-- THINGS TO FIX / IMPROVE IN THE FUTURE:

-- In Spanish, we don't usually say "he is A doctor" but instead "he is doctor". 
-- *Él es un doctor vs. Él es doctor.
-- It isn't grammatically incorrect, but it sounds odd though.

-- Adding the feminine or masculine counterpart for every profession whenever possible:
-- Doctor (masc) / Doctora (fem)
-- Enfermero (masc) / Enfermera (fem)

-- Adding separate verbs 'ser' and 'estar', Spanish equivalent to verb 'to be' in English.
