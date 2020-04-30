{-# LANGUAGE RecordWildCards #-}

module Factor.Code.Decl(Declaration(..), TypeInfo(..), ParamModuleDecl(..), RecordInfo(..),
                        desugarRecord) where

import Factor.Id
import Factor.Type
import Factor.Trait.Types
import Factor.Code
import Factor.Names
import qualified Factor.Stack as Stack

import Data.Foldable
import Data.Map(Map)

data Declaration = FunctionDecl PolyFunctionType Function
                 | MacroDecl PolyFunctionType Macro
                 | ModuleDecl Id [Declaration]
                 | ModuleSyn Id (Either QId TraitRef)
                 | RecordDecl Id [Id] (RecordInfo Declaration)
                 | TraitDecl Id ParameterizedTrait
                 | FunctorDecl Id [ModuleArg] (Map Id ParamModuleDecl)
                 | TypeDecl Id [Id] [TypeInfo]
                 | AliasDecl Id QId
                 | OpenDecl QId
                 | RequireDecl TraitRef
                 | IncludeDecl QId
                   deriving (Show, Eq)

-- Most things that can go inside a functor can be represented here in
-- the same way they are later in compilation, namely as a
-- FunctorInfo, but some things, like record declarations, should get
-- desugared out before we get that far.
data ParamModuleDecl = PModFunction PolyFunctionType Function
                     | PModMacro PolyFunctionType Macro
                     | PModModule (Map Id ParamModuleDecl)
                     | PModFunctor [ModuleArg] (Map Id ParamModuleDecl)
                     | PModTrait ParameterizedTrait
                     | PModType [Id] [TypeInfo]
                       deriving (Show, Eq)

data RecordInfo t = RecordInfo {
      recordConstructor :: Id,
      recordFields :: [(Id, Type)],
      recordDecls :: [t]
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

data DesugarRecordImpl t d = DesugarRecordImpl {
      typeDecl :: Id -> [Id] -> [TypeInfo] -> t,
      functionDecl :: Id -> PolyFunctionType -> Function -> t,
      moduleDecl :: Id -> [t] -> d
    }

desugarRecordImpl :: DesugarRecordImpl t d -> Id -> [Id] -> RecordInfo t -> d
desugarRecordImpl (DesugarRecordImpl {..}) name vs (RecordInfo con fields decls) =
    let targettype = NamedType (TypeId (QId [name, Id "t"]) $ fmap QuantVar vs)
        fieldtypes = reverse $ fmap snd fields
        stackvar = freshVar "R" vs
        type_ = typeDecl (Id "t") vs [TypeVal con (Stack.fromList fieldtypes)]
        fieldcount = length fields
        accessorType t = polyFunctionType (stackvar : vs) [targettype] (RestQuant stackvar)
                                                          [t] (RestQuant stackvar)
        accessorFor (i, (n, t)) = functionDecl n (accessorType t) (Function (Just n) (forNthField fieldcount i))
        accessors = fmap accessorFor $ zip [0..] (reverse fields)
        inner = [type_] ++ accessors ++ decls
    in moduleDecl name inner

desugarRecord :: Id -> [Id] -> RecordInfo Declaration -> Declaration
desugarRecord = desugarRecordImpl (DesugarRecordImpl TypeDecl (const FunctionDecl) ModuleDecl)
