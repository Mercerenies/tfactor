
module Factor.Code.Decl(Declaration(..), TypeInfo(..), RecordInfo(..),
                        desugarRecord) where

import Factor.Id
import Factor.Type
import Factor.Trait.Types
import Factor.Code
import Factor.Names
import qualified Factor.Stack as Stack

import Data.Foldable

data Declaration = FunctionDecl PolyFunctionType Function
                 | MacroDecl PolyFunctionType Macro
                 | ModuleDecl Id [Declaration]
                 | ModuleSyn Id (Either QId TraitRef)
                 | RecordDecl Id [Id] RecordInfo
                 | TraitDecl Id ParameterizedTrait
                 | FunctorDecl Id ParameterizedModule
                 | TypeDecl Id [Id] [TypeInfo]
                 | AliasDecl Id QId
                 | OpenDecl QId
                 | RequireDecl TraitRef
                 | IncludeDecl QId
                   deriving (Show, Eq)

data RecordInfo = RecordInfo {
      recordConstructor :: Id,
      recordFields :: [(Id, Type)],
      recordDecls :: [Declaration]
    } deriving (Show, Eq)

callDrop :: Statement
callDrop = Call $ QId [rootAliasName, primitivesModuleName, Id "drop"] -- TODO Factor.Names

callSwap :: Statement
callSwap = Call $ QId [rootAliasName, primitivesModuleName, Id "swap"] -- TODO Factor.Names

callStar :: Statement
callStar = Call $ QId [Id "*t"] -- TODO Factor.Names?

forNthField :: Int -> Int -> Sequence
forNthField total n = Sequence [
                       Literal (FunctionValue (Function Nothing (initial <> final))),
                       callStar
                      ]
    where initial = fold $ replicate n (Sequence [callDrop])
          final = fold $ replicate (total - 1 - n) (Sequence [callSwap, callDrop])

desugarRecord :: Id -> [Id] -> RecordInfo -> Declaration
desugarRecord name vs (RecordInfo con fields decls) =
    let targettype = NamedType (TypeId (QId [name, Id "t"]) $ fmap QuantVar vs)
        fieldtypes = reverse $ fmap snd fields
        stackvar = freshVar "R" vs
        type_ = TypeDecl (Id "t") vs [TypeVal con (Stack.fromList fieldtypes)]
        fieldcount = length fields
        accessorType t = polyFunctionType (stackvar : vs) [targettype] (RestQuant stackvar)
                                                          [t] (RestQuant stackvar)
        accessorFor (i, (n, t)) = FunctionDecl (accessorType t) (Function (Just n) (forNthField fieldcount i))
        accessors = fmap accessorFor $ zip [0..] fields
        inner = [type_] ++ accessors ++ decls
    in ModuleDecl name inner
