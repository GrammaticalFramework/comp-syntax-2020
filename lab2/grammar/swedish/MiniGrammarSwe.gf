concrete MiniGrammarSwe of MiniGrammar =

  open (G = GrammarSwe), (S = SyntaxSwe) in {

lincat
-- Common
   Utt = G.Utt ;
   Pol = G.Pol ;
   Temp = G.Temp ;

-- Cat
   Imp = G.Imp ;
   S = G.S ;
   QS = G.QS ;
   Cl = G.Cl ;
   QCl = G.QCl ;
   VP = G.VP ;
   Comp = G.Comp ;
   AP = G.AP ;
   CN = G.CN ;
   NP = G.NP ;
   IP = G.IP ;
   Pron = G.Pron ;
   Det = G.Det ;
   Conj = G.Conj ;
   Prep = G.Prep ;
   V = G.V ;
   V2 = G.V2 ;
   VS = G.VS ;
   VV = G.VV ;
   A = G.A ;
   N = G.N ;
   PN = G.PN ;
   Adv = G.Adv ;
   IAdv = G.IAdv ;
    
  lin
-- Phrase
   UttS = G.UttS ;
   UttQS = G.UttQS ;
   UttNP = G.UttNP ;
   UttAdv = G.UttAdv ;
   UttIAdv = G.UttIAdv ;
   UttImpSg = G.UttImpSg ;

-- Sentence
   UseCl = S.mkS ;
   UseQCl = S.mkQS ;
   PredVP = G.PredVP ;
   QuestCl = G.QuestCl ;
   QuestVP = G.QuestVP ;
   ImpVP = G.ImpVP ;

-- Verb
   UseV = G.UseV ;
   ComplV2 = S.mkVP ;
   ComplVS = G.ComplVS ;
   ComplVV = G.ComplVV ;
   UseComp = G.UseComp ;
   CompAP = G.CompAP ;
   CompNP = G.CompNP ;
   CompAdv = G.CompAdv ;
   AdvVP = G.AdvVP ;

-- Noun
   DetCN = G.DetCN ;
   UsePN = G.UsePN ;
   UsePron = G.UsePron ;
   MassNP = G.MassNP ;
   a_Det = S.a_Det ;
   aPl_Det = S.aPl_Det ;
   the_Det = S.the_Det ;
   thePl_Det = S.thePl_Det ;
   UseN = G.UseN ;
   AdjCN = G.AdjCN ;

-- Adjective
   PositA = G.PositA ;

-- Adverb
   PrepNP = G.PrepNP ;

-- Conjunction
   CoordS = S.mkS ;

-- Tense
   PPos = G.PPos ;
   PNeg = G.PNeg ;
   TSim = S.mkTemp S.presentTense S.simultaneousAnt ;
   TAnt = S.mkTemp S.presentTense S.anteriorAnt ;

-- Structural
   and_Conj = G.and_Conj ;
   or_Conj = G.or_Conj ;
    
   every_Det = G.every_Det ;

   in_Prep = G.in_Prep ;
   on_Prep = G.on_Prep ;
   with_Prep = G.with_Prep ;

   i_Pron = G.i_Pron ;
   youSg_Pron = G.youSg_Pron ;
   he_Pron = G.he_Pron ;
   she_Pron = G.she_Pron ;
   we_Pron = G.we_Pron ;
   youPl_Pron = G.youPl_Pron ;
   they_Pron = G.they_Pron ;

   whoSg_IP = G.whoSg_IP ;
    
   where_IAdv = G.where_IAdv ;
   why_IAdv = G.why_IAdv ;

   have_V2 = G.have_V2 ;
   want_VV = G.want_VV ;
    
}