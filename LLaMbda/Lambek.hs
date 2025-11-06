{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes
import LExp 
import PySupport

-- every way a list cna be split in 2 parts 
splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (x:xs) = ([],x:xs) : [(x:ls,rs) | (ls,rs) <- splits xs]

-----------------------------------------------------------------------------------
-- PART 2
-----------------------------------------------------------------------------------

-- picks each element in the list as a “focus” along with its left and right context
focusSplits :: [a] -> [([a], a, [a])]
focusSplits xs = [(l, a, r) | (l, a:r) <- splits xs]

-- right-inverse derivation: builds lambda expressions
der_rinv :: [(String, LTp)] -> LTp -> [LExp]
der_rinv gamma goal =
  case goal of
    DivL b c ->
      -- R rule: lambda abstraction for left division (B\C)
      [ L x e
      | let x = fresh (map (V . fst) gamma),     -- Avoiding variable capture : Take the first component of each pair in gamma (that’s the variable name), then wrap it in a V constructor (to make it a variable expression) and then generate a variable that is not one of the ones we already have 
        e <- der_rinv ((x, b) : gamma) c         
      ]

    DivR c b ->
      -- R' rule: lambda abstraction for right division (C/B)
      [ L x e
      | let x = fresh (map (V . fst) gamma),     -- generate a fresh variable to Avoide variable capture 
        e <- der_rinv (gamma ++ [(x, b)]) c
      ]

    Atm p ->
      -- F rule : Focus on some atom in the context
      concat [ der_lfoc left at right p | (left, at, right) <- focusSplits gamma ] -- apply der_lfoc to each focus possible and then concat to remove a layer from this [[[LExp]], [[LExp]], [[LExp]]] to this [[LExp], [LExp], [LExp]]


-- left-focused derivation: builds expressions by filling the focused variable
der_lfoc :: [(String, LTp)] -> (String, LTp) -> [(String, LTp)] -> Atm -> [LExp]
der_lfoc gammaL (var, a) gammaR p =
  case a of
    -- L rule a = B \ C  (DivL b c)
    DivL b c ->
      -- find a split of gammaL = left ++ right
      concat
        [ do
            eLeft <- der_rinv left b                     -- expressions producing the left arg(s)
            eRest <- der_lfoc right (var, c) gammaR p    -- rest with a V var placeholder
            let u = foldl A (V var) [eLeft]              -- apply V var to left-arg (left-to-right)
            return $ subst (u, var) eRest                -- substitute placeholder by application 
        | (left, right) <- splits gammaL
        ]

    -- L' rule a = C / B  (DivR c b)
    DivR c b ->
      -- find a split of gammaR = left ++ right
      concat
        [ do
            eRight <- der_rinv right b                    -- expressions producing the right arg
            eRest  <- der_lfoc gammaL (var, c) left p     -- rest with placeholder
            let u = foldl A (V var) [eRight]              -- apply V var to right-arg
            return $ subst (u, var) eRest
        | (left, right) <- splits gammaR
        ]

    Atm q ->
      if null gammaL && null gammaR && q == p
        then [V var]
        else []

-----------------------------------------------------------------------------------
-- PART 3
-----------------------------------------------------------------------------------
-- from lexicon
-- input Awen likes Butor output : [([("Awen_1","'Awen'"),("Likes_2","..."),("Butor_3","'Butor'")], Likes_2 Awen_1 Butor_3)]

getAllDerivationsWithEnv :: Lexicon PythonCode -> [String] -> [([(Var, PythonCode)], LExp)]
getAllDerivationsWithEnv (Lexicon entries stype) ws =
  let combos = sequence
        [[ (word, t, sem) | (w,t,sem) <- entries, w == word ]
        | word <- ws ]
      numbered =
        [ zipWith (\(w,t,sem) i -> (w ++ "_" ++ show i, t, sem))
                  combo [1..]
        | combo <- combos ]
   in concatMap (\combo ->
        let types = [(w,t) | (w,t,_) <- combo]
            env   = [(w,sem) | (w,_,sem) <- combo]
            derivs = der_rinv types stype
        in [(env, d) | d <- derivs])
      numbered

-- convert lambda to Python 
interpretLExp :: [(Var, PythonCode)] -> LExp -> PythonCode -- input : env = [ ("Awen_1", PCode "'Awen'"), ("Likes_2", PCode "lambda x : lambda y : (x,y) in likes"), ("Butor_3", PCode "'Butor'")]  lexp = A (A (V "Likes_2") (V "Awen_1")) (V "Butor_3")output : ((lambda x : lambda y : (x,y) in likes)('Awen'))('Butor')
interpretLExp env (V x) =
  case lookup x env of
    Just c  -> c
    Nothing -> PCode x
interpretLExp env (A e1 e2) =
  let PCode f1 = interpretLExp env e1
      PCode f2 = interpretLExp env e2
  in PCode ("(" ++ f1 ++ ")(" ++ f2 ++ ")")
interpretLExp env (L v body) =
  let PCode b = interpretLExp env body
  in PCode ("(lambda " ++ v ++ " : " ++ b ++ ")")




-----------------------------------------------------------------------------------
-- PART 2
-----------------------------------------------------------------------------------

getAllDerivations :: Lexicon sem -> [String] -> [LExp]
getAllDerivations (Lexicon entries stype) ws =
  let typeCombinations = sequence [[(word, t) | (w, t, _) <- entries, w == word] | word <- ws]
      numberedCombinations = [zipWith (\(word, tp) i -> (word ++ "_" ++ show i, tp)) combo [1..] 
                            | combo <- typeCombinations]
  in concatMap (\combo -> der_rinv combo stype) numberedCombinations

-- check if a sentence is grammatical and return derivations
checkSentence :: Lexicon sem -> [String] -> (Bool, [LExp])
checkSentence lex ws =
  let derivations = getAllDerivations lex ws
      grammatical = not (null derivations)
  in (grammatical, derivations)

prettyPrintDerivation :: LExp -> String
prettyPrintDerivation = prettyLExp

-----------------------------------------------------------------------------------
-- PART 1
-----------------------------------------------------------------------------------

dec_rinv :: [LTp] -> LTp -> Bool
dec_rinv gamma goal =
  case goal of
    DivL b c -> dec_rinv ([b] ++ gamma) c
    DivR c b -> dec_rinv (gamma ++ [b]) c
    Atm p    -> or [dec_lfoc left a right p | (left, a, right) <- focusSplits gamma]

dec_lfoc :: [LTp] -> LTp -> [LTp] -> Atm -> Bool
dec_lfoc gammaL a gammaR p =
  case a of
    DivL b c -> or [ dec_rinv left b && dec_lfoc right c gammaR p
                   | (left,right) <- splits gammaL ]
    DivR c b -> or [ dec_rinv right b && dec_lfoc gammaL c left p
                   | (left,right) <- splits gammaR ]
    Atm q    -> null gammaL && null gammaR && q == p

isGrammatical :: Lexicon sem -> [String] -> Bool
isGrammatical (Lexicon entries stype) ws =
  or [ dec_rinv types stype
     | types <- sequence [[t | (w, t, _) <- entries, w == word] | word <- ws] ]