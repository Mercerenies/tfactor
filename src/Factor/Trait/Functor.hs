{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.Trait.Functor(makeMinimalModule, bindModule, makeFreshModuleName) where

import Factor.Trait

-- Note: The functions in this file had to be moved to Factor.Trait
-- due to an unfortunate case of mutual recursion among them that
-- arose. This file exists, for the moment, since a lot of things
-- import it.

-- TODO Change those imports to Factor.Trait and eliminate this file.
