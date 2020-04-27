
module Factor.Code.Decl(Declaration(..), TypeInfo(..)) where

import Factor.Id
import Factor.Type
import Factor.Trait.Types
import Factor.Code
import Factor.Stack(Stack)

data Declaration = FunctionDecl PolyFunctionType Function
                 | MacroDecl PolyFunctionType Macro
                 | ModuleDecl Id [Declaration]
                 | ModuleSyn Id (Either QId TraitRef)
--                 | RecordDecl Id [RecordInfo]
                 | TraitDecl Id ParameterizedTrait
                 | FunctorDecl Id ParameterizedModule
--                 | RecordFunctorDecl Id [ModuleArg] [RecordFunInfo]
                 | TypeDecl Id [Id] [TypeInfo]
                 | AliasDecl Id QId
                 | OpenDecl QId
                 | RequireDecl TraitRef
                 | IncludeDecl QId
                   deriving (Show, Eq)

data TypeInfo = TypeVal Id (Stack Type)
                deriving (Show, Eq)

-- data RecordInfo = RecordConstructor Id
--                 | RecordField Id Type
--                 | RecordOrdinaryDecl Declaration
--                   deriving (Show, Eq)

-- data RecordFunInfo = RecordFunConstructor Id
--                    | RecordFunField Id Type
--                    | RecordFunOrdinaryDecl Id FunctorInfo
--                      deriving (Show, Eq)
