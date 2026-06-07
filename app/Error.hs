module Error where

import Control.Exception
import Pos

data FrankError = FrankError String Pos String

instance Show FrankError where
  show (FrankError reason pos text) =
    let base_msg = (concat [reason, " ", show pos, "\n", text])
    in base_msg++"\n"++(replicate (column pos) ' ')++"^ here"


instance Exception FrankError


