
module Factor.Names where

import Factor.Id

rootAliasName :: Id
rootAliasName = Id "__root"

preludeFileName :: FilePath
preludeFileName = "std/Prelude"

preludeModuleName :: Id
preludeModuleName = Id "Prelude"

primitivesModuleName :: Id
primitivesModuleName = Id "Primitives"

intType :: QId
intType = QId [primitivesModuleName, Id "Int"]

anyType :: QId
anyType = QId [primitivesModuleName, Id "Any"]

nothingType :: QId
nothingType = QId [primitivesModuleName, Id "Nothing"]

boolType :: QId
boolType = QId [primitivesModuleName, Id "Bool"]

stringType :: QId
stringType = QId [primitivesModuleName, Id "String"]

symbolType :: QId
symbolType = QId [primitivesModuleName, Id "Symbol"]
