
module Factor.Code.Decl(Declaration(..), RecordInfo(..)) where

import Factor.Id
import Factor.Type
import Factor.Trait.Types
import Factor.Code

data Declaration = FunctionDecl PolyFunctionType Function
                 | MacroDecl PolyFunctionType Macro
                 | ModuleDecl Id [Declaration]
                 | ModuleSyn Id (Either QId TraitRef)
                 | RecordDecl Id [RecordInfo]
                 | TraitDecl Id ParameterizedTrait
                 | FunctorDecl Id ParameterizedModule
                 | AliasDecl Id QId
                 | OpenDecl QId
                 | RequireDecl TraitRef
                 | IncludeDecl QId
                   deriving (Show, Eq)

data RecordInfo = RecordConstructor Id
                | RecordField Id Type
                | RecordOrdinaryDecl Declaration
                  deriving (Show, Eq)
