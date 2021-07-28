
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

-- FIXME: Separate files and other option-like stuff from IP/Mac/Switch maps.
-- The latter may be required by various library converting/quering functions,
-- but files, etc are plain option and are interested only for 'Main'.
data Config = Config
                { macIpMap :: MacIpMap
                , ipMacMap :: IpMacMap
                , swInfoMap :: SwInfoMap
                , ipCacheFile :: FilePath
                }
  deriving (Show)
