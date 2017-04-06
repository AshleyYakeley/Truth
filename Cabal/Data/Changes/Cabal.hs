{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Changes.Cabal where
{
    import Distribution.PackageDescription.Parse;
    import Distribution.PackageDescription;
    import Distribution.Package;
    import Distribution.ParseUtils;
    import Data.Changes;
    import Data.Result;


    interpretPackageDescription :: WholeLens String (Result PError GenericPackageDescription);
    interpretPackageDescription = resultWholeLens (parseResultToResult . parsePackageDescription) show where
    {
        parseResultToResult :: ParseResult a -> Result PError a;
        parseResultToResult (ParseFailed perr) = FailureResult perr;
        parseResultToResult (ParseOk _ a) = SuccessResult a;
    };

    instance TupleSelector GenericPackageDescription where
    {
        type TList GenericPackageDescription =
            (PackageDescription,
            ([Flag],
            (Maybe (CondTree ConfVar [Dependency] Library),
            ([(String, CondTree ConfVar [Dependency] Executable)],
            ()))));
        fromListTuple (a,(b,(c,(d,())))) = GenericPackageDescription a b c d;
        toListTuple (GenericPackageDescription a b c d) = (a,(b,(c,(d,()))));
    };

    instance Editable GenericPackageDescription where
    {
        type PartEdit GenericPackageDescription = TListPartEdit (TList GenericPackageDescription);
    };
{-

GenericPackageDescription
packageDescription :: PackageDescription
genPackageFlags :: [Flag]
condLibrary :: Maybe (CondTree ConfVar [Dependency] Library)
condExecutables ::


    instance (Editable a,Editable b) => Editable (a,b) where
    {
        type PartEdit (a,b) = TListPartEdit (a,(b,()));
    };
-}
}
