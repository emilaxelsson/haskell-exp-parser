module ParseExp where



import Data.Either
import Language.Haskell.TH

import Language.Haskell.ParseExp



parse :: String -> ExpQ
parse str = return $ case parseExp str of
    Right exp -> exp
    Left msg  -> error msg

exp1 = parse "\"sdf\""
exp2 = parse "sum [1,2,3]"
exp3 = parse "min (max (negate (-34)) 888) (signum (-45))"
exp4 = parse "  min   (  max   (  negate   (  -  34  )  )   888  )   (   signum    (  - 45   )   )  "
exp5 = parse "[(1,'a'),(2,'b'),(3,'c')]"

