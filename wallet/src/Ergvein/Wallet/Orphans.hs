module Ergvein.Wallet.Orphans where

import Network.Socks5

instance Eq SocksConf where
  (==) a b = socksServer a == socksServer b && socksVersion a == socksVersion b