{-# LANGUAGE TemplateHaskell #-}

import System.Exit

import ParseExp



tests =
    [ (show ($exp1), show "sdf")
    , (show ($exp2), show (sum [1,2,3]))
    , (show ($exp3), show (min (max (negate (-34)) 888) (signum (-45))))
    , (show ($exp4), show (  min   (  max   (  negate   (  -  34  )  )   888  )   (   signum    (  - 45   )   )  ))
    , (show ($exp5), show ([(1,'a'),(2,'b'),(3,'c')]))
    ]

main = if and oks
    then putStrLn "All tests passed"
    else do
      putStrLn $ "Tests " ++ show [i | (i,False) <- zip [1..] oks] ++ " failed"
      exitFailure
  where
    oks = map (uncurry (==)) tests

