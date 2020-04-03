
module Factor.Type where

import Factor.Util
import Factor.Stack(Stack)
import qualified Factor.Stack as Stack

import Data.Function

data Type = PrimType PrimType
          | FunType FunctionType
            deriving (Eq)

data FunctionType = FunctionType (Stack Type) (Stack Type)
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
            where listOut = sepBy (" " ++) . fmap (showsPrec 10) . Stack.FromBottom
                  args' = listOut args
                  rets' = listOut rets

emptyFnType :: FunctionType
emptyFnType = FunctionType Stack.empty Stack.empty

-- Top of stack is to the left, as per usual
functionType :: [Type] -> [Type] -> FunctionType
functionType = FunctionType `on` Stack.fromList
