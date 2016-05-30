import Control.Applicative
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

def = emptyDef 
    {
      commentStart = "/*"
    , commentEnd = "*/"
    , identStart = letter
    , identLetter = alphaNum
    , opStart = oneOf "+*/-"
    , reservedOpNames = ["-", "+", "*", "/"]
    , reservedNames = []
    }

TokenParser
    {
     parens = m_parens
   , identifier = m_identifier
   , reservedOp = m_reservedOp
   , reserved = m_reserved
   , semiSep1 = m_semiSep1
   , whiteSpace = m_whiteSpace
    } = makeTokenParser def


