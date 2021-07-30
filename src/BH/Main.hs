
module BH.Main (
  MacIpMap,
  IpMacMap,
  Config(..)
)
where

import qualified Data.Map as M
import qualified Data.Set as S

import BH.IP
import BH.Switch


type MacIpMap = M.Map MacAddr (S.Set IP)
type IpMacMap = M.Map IP MacAddr

data Config = Config
                { macIpMap :: MacIpMap
                , ipMacMap :: IpMacMap
                , swInfoMap :: SwInfoMap
                }
  deriving (Show)
