resource MicroResIta = open Prelude, Predef in {

  param
    Gender = Fem | Masc ;
    Number = Sg | Pl ;
    Case = Nom | Acc ;

--   Agreement = Agr Number ; ---s Person to be added

    Person = P1 | P2 | P3 ;
--   -- all forms of normal Eng verbs, although not yet used in MiniGrammar
    VForm = Inf | Cond Number Person | Ind Tense Number Person | SubjPres Number Person | SubjImpf Number Person | Imp Number Person ; 
    Tense = Present | Imperfect | Past | Future ;
    Aux = Essere | Avere ;

    Article = Il | Lo | L' | I | Gli | La | Le ;
  oper

    consonant : Str = "b"|"c"|"d"|"f"|"g"|"h"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|"v"|"z";
    inpure_s : Str = "s" + consonant ;
      
    Noun : Type = {s : Number => Str ; g : Gender } ;

    mkNoun : (sg, pl : Str) -> Gender -> Noun = \sg, pl, gender -> {
     s = table { Sg => sg ; Pl => pl } ;
     g = gender
     } ;

    regNounFem : Str -> Noun =
      \sg -> case sg of { cas + "a" => mkNoun sg (cas + "e") Fem };
    regNounMasc : Str -> Noun =
      \sg -> case sg of { fratell + "o" => mkNoun sg (fratell + "i") Masc } ;
    
--   -- smart paradigm
    smartNoun : Str -> Noun = \sg -> case sg of {
      telegram + "ma" => mkNoun sg (telegram + "mi") Masc ;
      poe + "ta" => mkNoun sg (poe + "ti") Masc ;
      _ + "a" => regNounFem sg ;
      _ + "o" => regNounMasc sg ;
      pan + "e" => mkNoun sg (pan + "i") Masc ; -- Not sure here
      _ => error ("No smarts for nouns here: " ++ sg)
      } ;

    Adjective : Type = {s : Gender => Number => Str ; isPre : Bool } ;

    mkAdjective : (femSg, femPl, mascSg, mascPl : Str) -> Bool -> Adjective =
      \femSg, femPl, mascSg, mascPl, pos ->
      {
	s = table {
	  Fem => 
	    table {
	      Sg => femSg ;
	      Pl => femPl 
	    } ;
	  Masc =>
	    table {
	      Sg => mascSg ;
	      Pl => mascPl
	    }
	  } ;
	  isPre = pos
      } ;

    smartAdjective : Str -> Adjective =
      \sg ->
      case sg of {
	italian + "o" => mkAdjective (italian + "a") (italian + "e") sg (italian + "i") False ;
	grand + "e" => mkAdjective sg (grand + "i") sg sg False ;
	_ => error ("No smarts for adjectives here: " ++ sg)
      } ;
    
    Verb : Type = {s : VForm => Str ; aux : Aux } ;

    verb1are : (inf,stem : Str) -> Verb
      = \inf,stem -> {
	s = table {
	  -- amare
	  Inf => inf ;
	  -- amere + i
	  Cond n p => stem + suffix "erei" "eresti" "erebbe" "eremmo" "ereste" "erebbero" n p ;
	  -- am + o
	  Ind Present n p => stem + suffix "o" "i" "a" "iamo" "ate" "ano" n p ;
	  -- am + "avo"
	  Ind Imperfect n p => stem + suffix "avo" "avi" "ava"  "avamo" "avate" "avano" n p;
	  -- am + ai
	  Ind Past n p => stem + suffix "ai" "asti" "ò"  "ammo" "amaste" "arono" n p ;
	  -- am + erò
	  Ind Future n p => stem + suffix "erò" "erai" "erà" "eremo" "erete" "eranno" n p;
	  -- special cases
	  SubjPres Sg _ => stem + "i" ; -- am + i
	  SubjPres Pl P1 => stem + "iamo" ;
	  SubjPres Pl P2 => stem + "iate" ;
	  SubjPres Pl P3 => stem + "ino" ;
	  SubjImpf Sg P3 => stem + "asse" ;
	  SubjImpf Sg _  => stem + "assi" ;
	  SubjImpf Pl P1 => stem + "assimo" ;
	  SubjImpf Pl P2 => stem + "aste" ;
	  SubjImpf Pl p3 => stem + "assero" ;
	  Imp Sg P1 => nonExist ;
	  Imp Sg P2 => stem + "a" ; -- am + a
	  Imp Sg P3 => stem + "i" ;
	  Imp Pl P1 => stem + "iamo" ;
	  Imp Pl P2 => stem + "ate" ;
	  Imp Pl P3 => stem + "ino"
	  } ;
	aux = Avere
      } ;
    
    verb2ere : (inf,stem : Str) -> Verb
      = \inf,stem -> {
	s = table {
	  -- credere
	  Inf => inf ;
	  -- cred + erei
	  Cond n p => stem + suffix "erei" "eresti" "erebbe" "eremmo" "ereste" "erebbero" n p ;
	  -- cred + o
	  Ind Present n p => stem + suffix "o" "i" "e" "iamo" "ete" "ono" n p;
	  -- cred + evo
	  Ind Imperfect n p => stem + suffix "evo" "evi" "eva" "evamo" "evate" "evano" n p;
	  -- cred + ei
	  Ind Past n p => stem + suffix "ei" "esti" "è"  "emmo" "este" "erono" n p ; -- è/ette, erono/ettero
	  -- cred + erò
	  Ind Future n p => stem + suffix "erò" "erai" "erà" "eremo" "erete" "eranno" n p;
	  -- special cases
	  SubjPres Sg _ => stem + "a" ; -- cred + a
	  SubjPres Pl P1 => stem + "iamo" ;
	  SubjPres Pl P2 => stem + "iate" ;
	  SubjPres Pl P3 => stem + "ano" ;
	  SubjImpf Sg P3 => stem + "esse" ;
	  SubjImpf Sg _  => stem + "essi" ;
	  SubjImpf Pl P1 => stem + "essimo" ;
	  SubjImpf Pl P2 => stem + "este" ;
	  SubjImpf Pl p3 => stem + "essero" ;
	  Imp Sg P1 => nonExist ;
	  Imp Sg P2 => stem + "i" ; -- cred + i
	  Imp Sg P3 => stem + "a" ;
	  Imp Pl P1 => stem + "iamo" ;
	  Imp Pl P2 => stem + "ete" ;
	  Imp Pl P3 => stem + "ano"
	  } ;
	aux = Avere
      } ;

    verb3ire : (inf,stem,stem2 : Str) -> Verb
      = \inf,stem,stem2 -> {
	s = table {
	  -- dormire
	  Inf => inf ;
	  -- dorm + irei
	  Cond n p => stem + suffix "irei" "iresti" "irebbe" "iremmo" "ireste" "irebbero" n p ;
	  -- dorm + o / fin + isc + o
	  Ind Present Pl P1 => stem + "iamo" ;
	  Ind Present Pl P2 => stem + "ite" ;
	  Ind Present n p => stem2 + suffix "o" "i" "e" "iamo" "ite" "ono" n p;
	  -- dorm + ivo
	  Ind Imperfect n p => stem + suffix "ivo" "ivi" "iva"  "ivamo" "ivate" "ivano" n p;
	  -- dorm + ii
	  Ind Past n p => stem + suffix "ii" "isti" "i"  "immo" "iste" "irono" n p ;
	  -- dorm + irò
	  Ind Future n p => stem + suffix "irò" "irai" "irà" "iremo" "irete" "iranno" n p;
	  -- special cases
	  SubjPres Sg P1 => stem2 + "a" ; -- dorm + a / fin + isc + a
	  SubjPres Sg _ => stem + "a" ; -- dorm + a
	  SubjPres Pl P1 => stem + "iamo" ;
	  SubjPres Pl P2 => stem + "iate" ;
	  SubjPres Pl P3 => stem2 + "ano" ;
	  SubjImpf Sg P3 => stem + "isse" ;
	  SubjImpf Sg _  => stem + "issi" ;
	  SubjImpf Pl P1 => stem + "issimo" ;
	  SubjImpf Pl P2 => stem + "iste" ;
	  SubjImpf Pl p3 => stem + "issero" ;
	  Imp Sg P1 => nonExist ;
	  Imp Sg P2 => stem2 + "i" ; -- dorm + i / fin + isc + i
	  Imp Sg P3 => stem2 + "a" ; -- dorm + a / fin + isc + a
	  Imp Pl P1 => stem + "iamo" ;
	  Imp Pl P2 => stem + "ite" ;
	  Imp Pl P3 => stem2 + "ano"  -- dorm + ano / fin + isc + ano
	  } ;
	aux = Avere
      } ;

    
    suffix : (sgP1, sgP2, sgP3, plP1, plP2, plP3 : Str) -> Number -> Person ->
      Str =
      \sgP1, sgP2, sgP3, plP1, plP2, plP3,n,p ->
      case <n,p> of {
	<Sg,P1> => sgP1 ;
	<Sg,P2> => sgP2 ;
	<Sg,P3> => sgP3 ;
	<Pl,P1> => plP1 ;
	<Pl,P2> => plP2 ;
	<Pl,P3> => plP3
	} ;

--   -- regular verbs with predictable variations
    smartVerb : Str -> Verb = \inf -> case inf of {
      am + "are" => verb1are inf am ;
      cred + "ere" => verb2ere inf cred ;
      dorm + "ire" => verb3ire inf dorm dorm ;
      _ => error ("No smarts for verbs here: " ++ inf)
      } ;

   -- normal irregular verbs e.g. rompere; pr. rompo, rompi, rompe, rompiamo, rompete, rompono; ps.p. ho rotto ; impf. rompevo; ps.r. ruppi, rompesti; f. romperò; sg.pr. che rompa,  che rompiamo; sg.impf. che rompessi; imp. rompi!, rompa!,  rompiamo!, rompete!;  pt.pr. rompente;
    irregVerb : (inf,presp1sg,presp2sg,presp3sg,presp1pl,presp2pl,presp3pl,pastpart,impfp1sg,pastp1sg,pastp2sg,futp1sg,pressubjsg,pressubjp1pl,impfsubjp1sg,impp2sg,impp3sg,impp1pl,impp2pl,impp3pl,prespart : Str) -> Aux -> Verb =
      \inf,presp1sg,presp2sg,presp3sg,presp1pl,presp2pl,presp3pl,pastpart,impfp1sg,pastp1sg,pastp2sg,futp1sg,pressubjsg,pressubjp1pl,impfsubjp1sg,impp2sg,impp3sg,impp1pl,impp2pl,impp3pl,prespart,aux ->
      {
	s = table {
	  Inf => inf ;
	  Ind Present Sg P1 => presp1sg ;
	  Ind Present Sg P2 => presp2sg ;
	  Ind Present Sg P3 => presp3sg ;
	  Ind Present Pl P1 => presp1pl ;
	  Ind Present Pl P2 => presp2pl ;
	  Ind Present Pl P3 => presp3pl ;
	  Ind Imperfect n p =>
	    case impfp1sg of {
	      sta + "vo" => sta + suffix "vo" "vi" "va"  "vamo" "vate" "vano" n p;
	      "ero" => suffix "ero" "eri" "era" "eravamo" "eravate" "erano" n p
	    } ;
	  Ind Past Sg P1 => pastp1sg ;
	  Ind Past Sg P2 => pastp2sg ;
	  Ind Past Sg P3 => case pastp1sg of { ebb + "i" => ebb + "e" } ;
	  Ind Past Pl P1 => case pastp2sg of { ave + "sti" => ave + "mmo" } ;
	  Ind Past Pl P2 => case pastp2sg of { ave + "sti" => ave + "ste" } ;
	  Ind Past Pl P3 => case pastp1sg of { ebb + "i" => ebb + "ero" } ;
	  Ind Future n p => case futp1sg of { av + "rò" => av + suffix "rò" "rai" "rà"  "remo" "rete" "ranno" n p } ;
	  Cond n p => case futp1sg of { av + "rò" => av + suffix "rei" "resti" "rebbe"  "remmo" "reste" "rebbero" n p } ;
	  SubjPres Sg _ => pressubjsg ;
	  SubjPres Pl P1 => pressubjp1pl ;
	  SubjPres Pl P2 => case pressubjp1pl of { abbia + "mo" => abbia + "te" } ;
	  SubjPres Pl P3 => case pressubjp1pl of { abbia + "mo" => abbia + "no" } ;
	  SubjImpf Sg P1 => impfsubjp1sg ;
	  SubjImpf Sg P2 => impfsubjp1sg ;
	  SubjImpf Sg P3 => case impfsubjp1sg of { aves + "si" => aves + "se" } ;
	  SubjImpf Pl P1 => case impfsubjp1sg of { aves + "si" => aves + "simo" } ;
	  SubjImpf Pl P2 => case impfsubjp1sg of { aves + "si" => aves + "te" } ;
	  SubjImpf Pl P3 => case impfsubjp1sg of { aves + "si" => aves + "sero" } ;
	  Imp Sg P1 => nonExist ;
	  Imp Sg P2 => impp2sg ;
	  Imp Sg P3 => impp3sg ;
	  Imp Pl P1 => impp1pl ;
	  Imp Pl P2 => impp2pl ;
	  Imp Pl P3 => impp3pl 
--	    _ => nonExist 
	  } ;
	aux = aux ;
      } ;
--   -- two-place verb with "case" as preposition; for transitive verbs, c=[]
    Verb2 : Type = Verb ** {c : Preposition } ;

    be_Verb : Verb =
      let
	orig = irregVerb "essere" "sono" "sei" "è" "siamo" "siete" "sono" "stato" "ero" "fui" "fosti" "sarò" "sia" "siamo" "fossi" "sii" "sia" "siamo" "siate" "siano" "stante" Essere
      in
      orig ** {
	s = table {
	  Ind Past Sg P3 => "fu" ;
	  Ind Past Pl P1 => "fummo" ;
	  Ind Past Pl P3 => "furono" ;
	  v => orig.s ! v 
	  }
      } ;
    
    -- con is the contraction with the article
    Preposition : Type = {s : Str ; con : Gender => Number => Str} ;

    emptyPreposition : Preposition = { s = "" ; con = \\_,_ => "" } ;
    
    mkPreposition : Str -> Preposition =
      \str ->
      {
	s = str ;
	con = let stem : Str = case str of {
		    "in" => "ne" ;
		    co + "n" => co ;
		    _ => str
		    }
	  in
	  table {
	    Fem => table {
	      Sg => pre {
		   "a"|"e"|"i"|"o"|"u" => stem + "ll'" ++ BIND ;
		   _ => stem + "lla"
		} ;
	      Pl => stem + "lle" 
	      } ;
	    Masc => table {
	      Sg => pre {
		"a"|"e"|"i"|"o"|"u" => stem + "ll'" ++ BIND ;
		"sb"|"sc"|"sd"|"sf"|"sg"|"sh"|"sk"|"sl"|"sm"|"sn"|"sp"|"sq"|"sr"|"st"|"sz"|"gn"|"pn"|"ps"|"z" => "llo" ;
		_ => stem + "l"
		} ;
	      Pl => pre {
		"a"|"e"|"i"|"o"|"u"|"sb"|"sc"|"sd"|"sf"|"sg"|"sh"|"sk"|"sl"|"sm"|"sn"|"sp"|"sq"|"sr"|"st"|"sz"|"gn"|"pn"|"ps"|"z" => "gli" ;
		_ => stem + "i"
		}
	      }
	  }
      };

    Pronoun : Type = {s : Case => Str ; g : Gender ; n : Number } ;

    NounPhrase : Type = {s : Case => Str ; det : Str ; g : Gender ; n : Number ; isPron : Bool } ;

}