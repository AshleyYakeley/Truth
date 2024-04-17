module Pinafore.Language.Grammar
    ( parseTopExpression
    , parseModule
    , parseType
    , module I
    ) where

import Pinafore.Language.Grammar.Docs as I (Docs)
import Pinafore.Language.Grammar.Interpret as I (interpretImportPinaforeDeclaration)
import Pinafore.Language.Grammar.Interpret
import Pinafore.Language.Grammar.Read as I
import Pinafore.Language.Grammar.Read.Expression as I (operatorFixity)
import Pinafore.Language.Grammar.Read.Expression
import Pinafore.Language.Grammar.Read.Parser
import Pinafore.Language.Grammar.Read.Token as I (allKeywords)
import Pinafore.Language.Grammar.Read.Type
import Pinafore.Language.Grammar.Syntax as I (FixAssoc(..), Fixity(..), typeOperatorFixity)
import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Shapes

parseTopExpression :: Text -> QInterpreter QExpression
parseTopExpression = parseScopedReaderWhole $ fmap interpretExpression readExpression

parseModule :: Text -> QInterpreter QModule
parseModule =
    parseScopedReaderWhole $ do
        smod <- readModule
        return $ interpretModule smod

parseType ::
       forall polarity. Is PolarityType polarity
    => Text
    -> QInterpreter (Some (QType polarity))
parseType text = do
    st <- parseScopedReaderWhole (fmap return readType) text
    interpretType st
