module Pinafore.Language.Read.Type
    ( readTypeName
    , readType3
    , readEntityType3
    , parseType
    ) where

import Pinafore.Language.EntityType
import Pinafore.Language.Literal
import Pinafore.Language.Name
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Pinafore.Language.Type
import Shapes hiding (try)
import Text.Parsec hiding ((<|>), many, optional)

type PinaforeTypeM baseedit = MPolarType (PinaforeType baseedit)

type PinaforeRangeType3 baseedit = MPolarRangeType (PinaforeType baseedit)

readJoinMeet ::
       forall mpolarity. Is MPolarity mpolarity
    => Parser ()
readJoinMeet =
    case representative @_ @MPolarity @mpolarity of
        PositiveMPolarity -> readExactlyThis TokOperator "|"
        NegativeMPolarity -> readExactlyThis TokOperator "&"
        BothMPolarity -> mempty

readType ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeScoped baseedit (PinaforeTypeM baseedit mpolarity))
readType = do
    t1 <- readType1
    mt2 <-
        optional $ do
            readJoinMeet @mpolarity
            readType
    case mt2 of
        Just t2 -> return $ liftA2 (toMPolar (<>)) t1 t2
        Nothing -> return t1

readType1 ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeScoped baseedit (PinaforeTypeM baseedit mpolarity))
readType1 =
    (try $ do
         at1 <- readTypeRange3
         readThis TokPropMap
         at2 <- readTypeRange3
         return $
             liftA2
                 (toMPolar
                      (\(MkAnyInKind t1) (MkAnyInKind t2) ->
                           MkAnyW $
                           singlePinaforeType $
                           GroundPinaforeSingularType MorphismPinaforeGroundType $
                           ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments))
                 at1
                 at2) <|>
    (try $ do
         at1 <- invertMPolarity @mpolarity readType3
         readThis TokMap
         at2 <- readType1
         return $
             liftA2
                 (toMPolar
                      (\(MkAnyW t1) (MkAnyW t2) ->
                           MkAnyW $
                           singlePinaforeType $
                           GroundPinaforeSingularType FuncPinaforeGroundType $
                           ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments))
                 (fmap MkInvertMPolarType at1)
                 at2) <|>
    readType2

readType2 ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeScoped baseedit (PinaforeTypeM baseedit mpolarity))
readType2 =
    (do
         readExactlyThis TokName "Either"
         at1 <- readType3
         at2 <- readType3
         return $
             liftA2
                 (toMPolar
                      (\(MkAnyW t1) (MkAnyW t2) ->
                           MkAnyW $
                           singlePinaforeType $
                           GroundPinaforeSingularType EitherPinaforeGroundType $
                           ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments))
                 at1
                 at2) <|>
    (do
         readExactlyThis TokName "Ref"
         at1 <- readTypeRange3
         return $
             fmap
                 (toMPolar
                      (\(MkAnyInKind t1) ->
                           MkAnyW $
                           singlePinaforeType $
                           GroundPinaforeSingularType ReferencePinaforeGroundType $
                           ConsDolanArguments t1 NilDolanArguments))
                 at1) <|>
    (do
         readExactlyThis TokName "Set"
         at1 <- readTypeRange3
         return $
             fmap
                 (toMPolar
                      (\(MkAnyInKind t1) ->
                           MkAnyW $
                           singlePinaforeType $
                           GroundPinaforeSingularType SetPinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments))
                 at1) <|>
    (do
         readExactlyThis TokName "Order"
         at1 <- invertMPolarity @mpolarity readType3
         return $
             fmap
                 (toMPolar
                      (\(MkAnyW t1) ->
                           MkAnyW $
                           singlePinaforeType $
                           GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments))
                 (fmap MkInvertMPolarType at1)) <|>
    readType3

readType3 ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeScoped baseedit (PinaforeTypeM baseedit mpolarity))
readType3 =
    (do
         at1 <- readBracket $ readType
         return $
             fmap
                 (toMPolar
                      (\(MkAnyW t1) ->
                           MkAnyW $
                           singlePinaforeType $
                           GroundPinaforeSingularType ListPinaforeGroundType $ ConsDolanArguments t1 NilDolanArguments))
                 at1) <|>
    (do
         MkAnyW s <- readTypeVar
         return $ return $ toMPolar $ MkAnyW $ singlePinaforeType $ VarPinaforeSingularType s) <|>
    readTypeConst <|>
    (readParen $ do
         mt <- optional $ readType
         case mt of
             Just at1 -> do
                 comma <- optional $ readThis TokComma
                 case comma of
                     Just () -> do
                         at2 <- readType
                         return $
                             liftA2
                                 (toMPolar
                                      (\(MkAnyW t1) (MkAnyW t2) ->
                                           MkAnyW $
                                           singlePinaforeType $
                                           GroundPinaforeSingularType PairPinaforeGroundType $
                                           ConsDolanArguments t1 $ ConsDolanArguments t2 NilDolanArguments))
                                 at1
                                 at2
                     Nothing -> return at1
             Nothing -> return $ return $ toMPolar $ MkAnyW $ literalPinaforeType UnitLiteralType)

readTypeRange3 ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeScoped baseedit (PinaforeRangeType3 baseedit mpolarity))
readTypeRange3 = (readBracketed TokOpenBrace TokCloseBrace $ readCommaList readTypeRangeItem) <|> readTypeRangeItem

readTypeRangeItem ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeScoped baseedit (PinaforeRangeType3 baseedit mpolarity))
readTypeRangeItem =
    (do
         readExactlyThis TokOperator "+"
         atq <- readType3
         return $ fmap (toMPolar (\(MkAnyW tq) -> MkAnyInKind $ MkRangeType NilPinaforeType tq)) atq) <|>
    (do
         readExactlyThis TokOperator "-"
         atp <- invertMPolarity @mpolarity readType3
         return $
             fmap
                 (toMPolar (\(MkAnyW tp) -> MkAnyInKind $ MkRangeType tp NilPinaforeType))
                 (fmap MkInvertMPolarType atp)) <|>
    (do
         mt <- readType3 @baseedit @'Nothing
         let
             ff :: forall polarity. IsTypePolarity polarity
                => PinaforeTypeM baseedit 'Nothing
                -> AnyInKind (RangeType (PinaforeType baseedit) polarity)
             ff (BothMPolarType atw) =
                 case (invertPolarity @polarity $ atw @(InvertPolarity polarity), atw @polarity) of
                     (MkAnyW tp, MkAnyW tq) -> MkAnyInKind $ MkRangeType tp tq
         return $ fmap (\t -> toMPolar $ ff t) mt)

readTypeVar :: Parser (AnyW SymbolWitness)
readTypeVar =
    try $ do
        s <- readThis TokName
        case unpack s of
            c:_
                | isLower c -> return $ toSymbolWitness (unpack s) MkAnyW
            _ -> mzero

readTypeConst ::
       forall baseedit mpolarity. Is MPolarity mpolarity
    => Parser (PinaforeScoped baseedit (PinaforeTypeM baseedit mpolarity))
readTypeConst = do
    spos <- getPosition
    n <- readTypeName
    case n of
        "Any" ->
            case representative @_ @MPolarity @mpolarity of
                NegativeMPolarity -> return $ return $ toMPolar $ MkAnyW NilPinaforeType
                _ -> fail $ unpack n <> " not allowed as positive type"
        "None" ->
            case representative @_ @MPolarity @mpolarity of
                PositiveMPolarity -> return $ return $ toMPolar $ MkAnyW NilPinaforeType
                _ -> fail $ unpack n <> " not allowed as negative type"
        "Entity" ->
            return $
            return $
            toMPolar $
            MkAnyW $
            singlePinaforeType $
            GroundPinaforeSingularType (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments
        "NewEntity" ->
            return $
            return $
            toMPolar $
            MkAnyW $
            singlePinaforeType $
            GroundPinaforeSingularType (SimpleEntityPinaforeGroundType NewSimpleEntityType) NilDolanArguments
        "Action" ->
            return $
            return $
            toMPolar $
            MkAnyW $ singlePinaforeType $ GroundPinaforeSingularType ActionPinaforeGroundType NilDolanArguments
        "UI" ->
            return $
            return $
            toMPolar $
            MkAnyW $ singlePinaforeType $ GroundPinaforeSingularType UserInterfacePinaforeGroundType NilDolanArguments
        _
            | Just (MkAnyW lt) <- nameToLiteralType n -> return $ return $ toMPolar $ MkAnyW $ literalPinaforeType lt
        _ ->
            return $
            runSourcePos spos $ do
                nt <- lookupNamedType n
                case nt of
                    EntityNamedType (MkAnyW sw) ->
                        return $
                        toMPolar $
                        MkAnyW $
                        singlePinaforeType $
                        GroundPinaforeSingularType
                            (SimpleEntityPinaforeGroundType $ NamedSimpleEntityType sw)
                            NilDolanArguments

nameToLiteralType :: Name -> Maybe (AnyW LiteralType)
nameToLiteralType "Literal" = Just $ MkAnyW LiteralLiteralType
nameToLiteralType "Text" = Just $ MkAnyW TextLiteralType
nameToLiteralType "Number" = Just $ MkAnyW NumberLiteralType
nameToLiteralType "Boolean" = Just $ MkAnyW BooleanLiteralType
nameToLiteralType _ = Nothing

readTypeName :: Parser Name
readTypeName =
    try $ do
        s <- readThis TokName
        case unpack s of
            c:_
                | isUpper c -> return s
            _ -> mzero

readEntityType3 :: Parser (PinaforeScoped baseedit (AnyW EntityType))
readEntityType3 = do
    ctm <- readType3 @_ @'Nothing
    return $ do
        BothMPolarType atm <- ctm
        case atm @'PositivePolarity of
            MkAnyW tm ->
                case typeToEntityType tm of
                    Just t -> return t
                    Nothing -> fail $ show tm <> " is not an Entity type"

parseType ::
       forall baseedit polarity. IsTypePolarity polarity
    => SourcePos
    -> Text
    -> Result Text (PinaforeScoped baseedit (AnyW (PinaforeType baseedit polarity)))
parseType =
    parseReader $ do
        mt <- isMPolarity @polarity $ readType @baseedit @('Just polarity)
        return $ do
            SingleMPolarType atw <- mt
            return atw
