
module Factor.Type where

import Factor.Util

data Type = PrimType PrimType
          | FunType FunctionType
            deriving (Eq)

-- Top of stack is on the right, here.
data FunctionType = FunctionType [Type] [Type]
                    deriving (Eq)

data PrimType = TInt | TAny | TNothing
                deriving (Eq, Ord, Enum)

instance Show PrimType where
    showsPrec _ x =
        case x of
          TInt -> ("Int" ++)
          TAny -> ("Any" ++)
          TNothing -> ("Nothing" ++)

instance Show Type where
    showsPrec n (PrimType t) = showsPrec n t
    showsPrec n (FunType t) = showsPrec n t

instance Show FunctionType where
    showsPrec _ (FunctionType args rets) =
        ("( " ++) . args' . (" -- " ++) . rets' . (" )" ++)
            where args' = sepBy (" " ++) (map (showsPrec 10) args)
                  rets' = sepBy (" " ++) (map (showsPrec 10) rets)

emptyFnType :: FunctionType
emptyFnType = FunctionType [] []
