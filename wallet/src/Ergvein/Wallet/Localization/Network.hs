module Ergvein.Wallet.Localization.Network(
    NetworkPageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Types.Transaction
import Ergvein.Wallet.Language

import Data.Text

data NetworkPageStrings =
    NPSTitle
  | NPSStatus
  | NPSStatusVal Int
  | NPSStatusDescr
  | NPSServer
  | NPSServerVal Text
  | NPSServerDescr
  | NPSHeight
  | NPSHeightVal BlockHeight
  | NPSHeightDescr
  | NPSWait
  | NPSNoValue
  | forall e . LocalizedPrint e => NPSError e

instance LocalizedPrint NetworkPageStrings where
  localizedShow l v = case l of
    English -> case v of
      NPSTitle        -> "Network"
      NPSStatus       -> "Status: "
      NPSStatusVal n  -> showt n <> " connections."
      NPSStatusDescr  -> "Amount of indexers connected"
      NPSServer       -> "Server: "
      NPSServerVal s  -> s
      NPSServerDescr  -> "Server indexer for tx history"
      NPSHeight       -> "Height: "
      NPSHeightVal n  -> showt n <> " blocks."
      NPSHeightDescr  -> "Current height (and if there any forks detected)"
      NPSWait         -> "Wait ..."
      NPSNoValue      -> "No value"
      NPSError e      -> localizedShow l e
    Russian -> case v of
      NPSTitle        -> "Сеть"
      NPSStatus       -> "Статус: "
      NPSStatusVal n  -> (<>) (showt n) $ case n of
                            0 -> " соединений."
                            1 -> " соединение."
                            2 -> " соединения."
                            3 -> " соединения."
                            4 -> " соединения."
                            _ -> " соединений."
      NPSStatusDescr  -> "Количество подключенных индексаторов"
      NPSServer       -> "Сервер: "
      NPSServerVal s  -> s
      NPSServerDescr  -> "Сервер индесатор для истории транзаций"
      NPSHeight       -> "Высота: "
      NPSHeightVal n  -> (<>) (showt n) $ case n of
                            0 -> " блоков."
                            1 -> " блок."
                            2 -> " блока."
                            3 -> " блока."
                            4 -> " блока."
                            _ -> " блоков."
      NPSHeightDescr  -> "Текущая высота (и форки, если обнаружены)"
      NPSWait         -> "Ждите ..."
      NPSNoValue      -> "Нет значения"
      NPSError e      -> localizedShow l e
