
module Factor.State.Alias where

import Factor.State()
import Factor.Id

import Data.Map(Map)
import qualified Data.Map as Map

defAlias :: Id -> QId -> Map Id QId -> Map Id QId
defAlias = Map.insert

--openModule :: QId -> Map Id ReaderValue -> Map Id QId -> Map Id QId
--openModule mname values = 

