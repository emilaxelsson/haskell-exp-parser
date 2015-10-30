-- | Simple parser for a subset of Haskell expressions and patterns to the
-- TemplateHaskell AST
--
-- The following expressions are currently supported:
--
-- * Variables
-- * Integer and string literals
-- * Prefix function application
-- * Lists and tuples
--
-- The following patterns are currently supported:
--
-- * Variables

module Language.Haskell.ParseExp
  ( parseExp
  , parsePat
  ) where



import Control.Monad
import Data.Char
import Language.Haskell.TH
import Text.ParserCombinators.ReadP



-- | Skip any amount of whitespace
skipSpace :: ReadP ()
skipSpace = void $ munch isSpace

-- | Check if a character is a valid non-initial character in a name (variable
-- or constructor)
nameChar :: Char -> Bool
nameChar c = isAlphaNum c || elem c ['\'','_']

-- | Parse a Haskell variable name
name :: ReadP Name
name = do
    skipSpace
    h <- get
    guard ('a' <= h && h <= 'z')
    rest <- munch nameChar
    return $ mkName (h:rest)

-- | Parse a Haskell variable
variable :: ReadP Exp
variable = fmap VarE name

-- | Parse a Haskell Constructor
constructor :: ReadP Exp
constructor = do
    skipSpace
    h <- get
    guard ('A' <= h && h <= 'Z')
    rest <- munch nameChar
    return $ ConE $ mkName (h:rest)

-- | Parse an integer
integer :: Bool -> ReadP Integer
integer first = do
    c:_ <- look
    guard (first || isNumber c)
    readS_to_P reads

-- | Parse a Haskell literal
literal :: Bool -> ReadP Exp
literal first
    =   fmap (LitE . IntegerL) (integer first)
    <++ fmap (LitE . CharL) (readS_to_P reads)
    <++ fmap (LitE . StringL) (readS_to_P reads)

-- | Parse a comma-separated list of expressions
expressionList :: ReadP [Exp]
expressionList = expression `sepBy` char ','

-- | Parse a list expression
list :: ReadP Exp
list = fmap ListE $ between (char '[') (char ']') expressionList

-- | Parse a tuple expression
--
-- This also handles empty tuples ('()') and parenthesized expressions
tuple :: ReadP Exp
tuple = do
    es <- between (char '(') (char ')') (skipSpace >> expressionList)
      -- skipSpace needed to parse empty tuples with space inside
    case es of
        []  -> return $ ConE $ mkName "()"
        [e] -> return e
        _   -> return $ TupE es

-- | Parse an expression that is not an application
expPart :: Bool -> ReadP Exp
expPart first = do
    skipSpace
    pfail <++ variable
          <++ constructor
          <++ list
          <++ literal first
          <++ tuple
  -- Must handle lists before literals, because the "['a']" is accepted as a
  -- String literal

-- | Expression parser
expression :: ReadP Exp
expression = do
    skipSpace
    f    <- expPart True
    args <- many (expPart False)
    let expr = foldl AppE f args
    skipSpace
    return expr

-- | Parse a Haskell expression (the supported subset is given above)
parseExp :: String -> Either String Exp
parseExp str = case [expr | (expr,"") <- readP_to_S expression str] of
    [expr] -> return expr
    _ -> fail $ "parseExp: cannot parse '" ++ str ++ "'"
             ++ " (parseExp only supports a limited subset of Haskell)"

-- | Parse a Haskell pattern (the supported subset is given above)
parsePat :: String -> Either String Pat
parsePat str = case [pat | (pat,"") <- readP_to_S name str] of
    [pat] -> return (VarP pat)
    _ -> fail $ "parsePat: cannot parse '" ++ str ++ "'"
             ++ " (parsePat only supports a limited subset of Haskell)"

