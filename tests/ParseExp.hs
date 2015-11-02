{-# LANGUAGE TemplateHaskell #-}

import Data.Data
import Data.Either
import Data.Generics.Aliases
import Data.Generics.Schemes
import Language.Haskell.TH
import System.Exit

import Language.Haskell.ParseExp



parse :: String -> Exp
parse str = case parseExp str of
    Right exp -> exp
    Left msg  -> error msg

dequalify :: Exp -> Exp
dequalify = everywhere (mkT trans)
  where
    trans (VarE v) = VarE $ mkName $ nameBase v
    trans e        = e

addWhite :: Char -> String
addWhite c
    | c `elem` cs = "   " ++ [c] ++ "   "
    | otherwise   = [c]
  where
    cs = ['(', ')', ',', '-', ' ']

test1 = ("\"sdf\"",                                      [| "sdf" |])
test2 = ("sum [1,2,3]",                                  [| sum [1,2,3] |])
test3 = ("min (max (negate (-34)) 888) (signum (-45))",  [| min (max (negate (-34)) 888) (signum (-45)) |])
test4 = ("[(1,'a',\"a\"),(2,'b',\"b\"),(-3,'c',\"c\")]", [| [(1,'a',"a"),(2,'b',"b"),(-3,'c',"c")] |])

tests = [test1,test2,test3,test4]

main = do
    es1 <- mapM (fmap dequalify . runQ . snd) tests
    let es2  = map (parse . fst) tests
        es3  = map (parse . concatMap addWhite . fst) tests
        oks2 = map (uncurry (==)) $ zip es1 es2
        oks3 = map (uncurry (==)) $ zip es1 es3
        failedTests = [i | (i,(ok2,ok3)) <- zip [1..] (zip oks2 oks3), not (ok2&&ok3)]
    if and oks2 && and oks3
    then putStrLn "All tests passed"
    else do
      putStrLn $ "Tests " ++ show failedTests ++ " failed"
      exitFailure

