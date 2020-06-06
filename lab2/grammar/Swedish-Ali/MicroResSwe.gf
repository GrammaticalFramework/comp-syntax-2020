resource MicroResSwe =  {

param
  Number = Sg | Pl ;
  Gender = Ut | Neu ;
  Defenitivness = Defi | Indef ;
--  Case = Nom | Acc ;
--  Gender = Utr | Neutr

--  Agreement = Agr Number ; ---s Person to be added

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
--  VForm = Inf | PresSg3 | Past | PastPart | PresPart ;

oper

  Noun : Type = {s : Number => Defenitivness => Str ; g : Gender } ;


  --  mkN :(sgIndef,sgDefi,plindef,plDefi : Str)-> Gender -> Noun = smartN ;
--  } ;


 mkN : (sgIndef,sgDefi,plIndef,plDefi : Str) -> Gender -> Noun
    =\sgIndef, sgDefi, plIndef, plDefi, gender -> {s = table { Sg    => table { Indef => sgIndef ;
                                                                               Defi  => sgDefi } ;

                                                           Pl    => table { Indef => plIndef ;
                                                                            Defi  => plDefi }
                                         } ;
                                                                 g = gender }  ;
  --smartN : Str -> Gender -> Noun ;
--  smartN sgIndef gender sgDefi plIndef plDefi  = {s = table { Sg    => table { Indef => sgIndef ;
  --                                                                            Defi  => sgDefi } ;
---
  --                                                           Pl    => table { Indef => plIndef ;
    --                                                                      Defi  => plDefi }
    --                                   } ;
    --                  g = gender }  ;



  smartN : Str -> Gender -> Noun ;
  smartN sgIndef gender  = case <sgIndef, gender> of {
    <x + ("t"|"d"),Neu> => mkN sgIndef (sgIndef + "en") (sgIndef + "ar") (sgIndef + "arna") Neu ;
    <x + "e", Neu>       => mkN sgIndef (x + "en") (x + "ar") (x + "arna") Neu ;
    < x + "m", Neu >   => mkN sgIndef (x + "len") (x +"lar") (x + "larna")  Neu ;
  <  _ , Ut  >   => mkN sgIndef (sgIndef + "et") sgIndef (sgIndef + "ene")  Ut ;
  <   _, Neu >   => mkN sgIndef (sgIndef + "et") sgIndef (sgIndef + "ene") Neu
} ;


--  irregN : Str -> Str -> Gender -> Noun;
--  irregN sgIndef plIndef gender = case <gender,sgIndef,plIndef> of {
--  < Neu, x + ("l"|"k"|"d"), _ > => mkN sgIndef (sgIndef + "en") (plIndef) (plIndef + "erna") Neu ;
--  < Neu, "Öl", _ >              => mkN sgIndef (sgIndef + "en") (plIndef) (plIndef ) Neu ;
--  < Neu, "man", _ >             => mkN sgIndef (sgIndef + "en") (plIndef) (plIndef + "en" ) Neu ;
--  < Neu, _ , _  >             => mkN sgIndef (sgIndef + "en") (plIndef) (plIndef + "en" ) Neu ;
--  < Ut, _ ,_  >             => mkN sgIndef (sgIndef + "en") (plIndef) (plIndef + "en" ) Ut ;
--  < Ut, x + ("s"|"n"|"v"|"r"|"k"|"d"), plIndef > => mkN sgIndef (sgIndef + "et") (plIndef) (plIndef + "en") Ut
--};

  --  mkN : Str -> Str -> Noun = irregN ;
  --irregN : Str -> Str - > Noun
  --  } ;
  --  regN : Str  -> Gender -> Noun ;
  --  regN sgIndef gender  = {



  -- smart paradigm
  --smartN : Str -> Gender -> Noun ;
  --smartN sgIndf gender = case <gender,sgIndf> of {
  --   <Utrum, sky + ("a"|"i"|"o"|"u"|"y"|"æ"|"ø"|"å")> --en kopi en ko en sky en sø
  --     => worstN sgIndf (sky + "en") (sky + "er") (sky + "erne") Utrum ;
  --   <Utrum, tåg + "e" > -- en tåge
  --     => worstN sgIndf (sgIndf +"n") (sgIndf + "r") (sgIndf + "erne") Utrum ;
  --   <Utrum, ka + "t" > -- en kat
  --     => worstN sgIndf (sgIndf + "ten") (sgIndf + "te") (sgIndf + "tene") Utrum ; --   <Utrum, _ > -- en computer
  --     => worstN sgIndf (sgIndf + "en") (sgIndf + "er") (sgIndf + "erne") Utrum ;
  --   <Neutrum, hjert + "e"> -- et hjerte
  --     => worstN sgIndf (sgIndf + "t") (hjert + "er") (hjert + "erne") Neutrum ;
  --   <Neutrum, _ >
  --     => worstN sgIndf (sgIndf + "et") (sgIndf) (sgIndf + "ene") Neutrum
  --  } ;
  --```










  --   regN : Str -> Gender -> Noun ;
  --   irregN sgIndef  =
  --     let sgDefi, plDefi, plIndef  : Str = case sgIndef of {
  --      x + ("a"|"o")            => (x + "or") (sgIndef + "er") (sgIndef + "ar") ;
  --      x + ("t"|"r"|"n")  	     => (sgIndef + "er") (x + "or")(sgIndef + "ar") ;
  --      x + ("k"|"d"|"l"|"e")    => (sgIndef + "ar") (sgIndef + "ar") ;
  --      _                        => (sgIndef + "s") }; g = Ut ;

  --      in regN sgIndef sgDefi plDefi plIndef ;

  --      let plDefi : Str = case sgIndef of {
  --      x + ("a"|"o")            => x + "orna" ;
  --      x + ("t"|"r"|"n")  	    => sgIndef + "erna" ;
  --      x + ("k"|"d"|"l"|"e")    => sgIndef + "arna" ;
  --      _                        => sgIndef + "s" }  ;

  --      let sgDefi : Str = case sgdefi of {
  --      x + ("a"|"o")            => x + "et" ;
  --      x + ("t"|"r"|"n")  	    => sgIndef + "erna" ;
  --      x + ("k"|"d"|"l"|"e")    => sgIndef + "arna" ;
  --      _                        => sgIndef + "s" }  ;










   --mKN : Str -> Noun = indefSg ;
  --  mKN : Str -> Noun = smartN ;


--  mkN : (sgIndef,sgDefi,plDefi,plIndef: Str) -> Noun
  --  = \sgIndef, sgDefi, plDefi, plIndef -> {
    --              s = table { Sg    => table { Indef => sgIndef ;
      --                                         Defi  => sgDefi } ;

        --                      Pl    => table { Indef => plIndef ;
          --                                     Defi  => plDefi }
            --                  } ;
              --    g = Neu } ;  --table { Ut => ut ; Neu  =>  neu }


  --  smartNoun : Str -> Noun = \sg -> case sg of {
    --  x + ("a"|"o")            => mkN sg (x + "or") (x + "orna") (x + "en");
    --  x + ("t"|"r"|"n")  	     => mkN sg (x + "er") (x + "orna") (x + "en");
    --  x + ("k"|"d"|"l"|"e")    => mkN sg (x + "ar") (x + "orna") (x + "en") ;
    --  _                        => mkN sg (x + "s") (x + "orna") (x + "en")
  --  } ;

--  }
   -- smartAdj : Str -> Adjective = \sg -> case sg of {




--  smartN : Str -> Noun ;
--  defReg : Str -> Noun ;
--  defReg indef sg =
--     let defi sg : Str = case indefi sg of {
--      _     => sg + "en"  } ;
--      in irregN sg pl ;


  --regN : Str -> Noun ;
--  regN sgIndef  =
  --   let plIndef : Str = case sgIndef of {
  --   x + ("a"|"o")            => x + "or" ;
    -- x + ("t"|"r"|"n")  	    => sgIndef + "er" ;
    -- x + ("k"|"d"|"l"|"e")    => sgIndef + "ar" ;
    -- _                        => sgIndef + "s" } ;

    -- in irregN sgIndef sgDefi plDefi plIndef ;



--  regN : Str -> Noun ;
--  regN sgIndef  =

  --   let plDefi : Str = case sgIndef of {
  --   x + ("a"|"o")            => x + "orna" ;
  --   x + ("t"|"r"|"n")  	    => sgDefi + "erna" ;
  --   x + ("k"|"d"|"l"|"e")    => sgDefi + "arna" ;
  --   _                        => sgDefi + "s" }  ;

  --   in irregN sgIndef sgDefi plDefi plIndef ;

--  irregN : Str -> Str -> Noun ;
--  irregN sgIndef sgDefi plDefi plIndef = {
  --              s = table { Sg    => table { Indef => sgIndef ;
    --                                         Defi  => sgDefi } ;
--
  --                          Pl    => table { Indef => plIndef ;
    --                                         Defi  => plDefi }
      --                           } ;
        --             g = Ut }





  --regN : Str -> Noun ;



  -- Adjective : Type = {s : Det => Str}
  -- mkA : Str -> Adjective
  -- mkA sg = {}


  --  regN : (sgIndef : Str) -> Noun = \sg ->
  --  regN sg (sg + "e") (sg + "s") (sg + "es") ;

  --    irregN : Str  -> Str -> Noun ;
  --    irregN sgIndef   = {
  --                    s = table { Sg    => table { Indef => sgIndef ;
  --                                                 Defi  => sgDefi } ;
  --
  --                                Pl    => table { Indef => plIndef ;
  --                                                 Defi  => plDefi }
  --                                     } ;
  --                    g = Ut } ;



--  mkA : Str -> {s : Str} ;
--  mkA str = {s = str} ;

--  mkV : Str -> {s : Str} ;
--  mkV str = {s = str} ;

--  mkV2 : Str -> {s : Str} ;
--  mkV2 str = {s = str} ;

--  mkAdv : Str -> {s : Str} ;
--  mkAdv str = {s = str} ;

--  mkPrep : Str -> {s : Str} ;
--  mkPrep str = {s = str} ;

--  mkPron : Str -> {s : Str} ;
--  mkPron str = {s = str} ;

}
