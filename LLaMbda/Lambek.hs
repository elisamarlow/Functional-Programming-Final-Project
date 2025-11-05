{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes

splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (x:xs) = ([],x:xs) : [(x:ls,rs) | (ls,rs) <- splits xs]

dec_rinv :: [LTp] -> LTp -> Bool
dec_rinv gamma goal =
  case goal of
    DivL b c -> dec_rinv ([b] ++ gamma) c
    DivR c b -> dec_rinv (gamma ++ [b]) c
    Atm p    -> or [dec_lfoc left a right p | (left, a, right) <- focusSplits gamma]
  where
    focusSplits xs = [(l, a, r) | (l, (a:r)) <- splits xs]

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

