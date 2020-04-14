
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
