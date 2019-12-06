{-# LANGUAGE TypeApplications #-}

module Tests.Ergo.Headers where

import Test.Tasty.Hspec

import Control.Monad
import Data.ByteString (ByteString)
import Data.Either.Combinators
import Data.Text (Text)

import qualified Data.ByteString.Base16 as BS16
import qualified Data.Serialize as Serialize
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Ergvein.Interfaces.Ergo.Header
import Ergvein.Interfaces.Ergo.PoPowHeader

spec_HeaderParser :: Spec
spec_HeaderParser = forM_ headerSamples $ \sample -> do

  it ("Parse `Header` from sample " <> (show $ T.take 8 sample <> "..")) $ do
      (leftToMaybe . Serialize.decode @Header . fromHex $ sample)
        `shouldBe` Nothing

  it ("Parse `PoPowHeader` from sample " <> (show $ T.take 8 sample <> "..")) $ do
      (leftToMaybe . Serialize.decode @PoPowHeader . fromHex $ sample)
        `shouldBe` Nothing

fromHex :: Text -> ByteString
fromHex = fst . BS16.decode . TE.encodeUtf8

headerSamples :: [Text]
headerSamples = [
    "01528024e4018001010d958081486cf5361913e8012c0680ffff80ffff00007f7ff7bf7f007f017f013c7d8000c61a01009effae5846800001cf013dffff01ffd7ff3affa280cbff8780dc8cb3ff8000005b00cf01c87f01006a01ba5f0001ff3f47ff8069437fca9980ff808061f57fff7f110105ef0080afff7fff5a800019ff00fdd4caebcac4da9e3b8a01bf5cff010101002bba0143570831475ee58d0052807f7f0001075da901c7207fffffa3d7f5b103000000038b0f29a60fa8d7e1aeafbe512288a6c6bc696547bbf8247db23c95e83014513c03b07e0b08ff31053ec69881781fdce1e2f6549240efb2755f421578f53134f97d395ce4017f000000083e269b6f58ee089a"
  , "01ffff144145ef9c9300ce01006200940e017fb1f7f154e694607f1f0180bcddfc7f010001007ffc4f8000ff008580c9d49f9b142e7f01eb397f00809bf280484b807f00ff2b317fb69791237fc9556cff26000180807ff73f802680fff20087569c730000009a812a3a9d0080d29480d367017f80804c017f6d80788080ff098000e9ab8c98bca3d6f37600007fa701ff60547316c02f10ac67003f80307f000150e80a01ffff7f25b4a4207fffffa8fdf0a206000000038b0f29a60fa8d7e1aeafbe512288a6c6bc696547bbf8247db23c95e83014513c030239c3204139d4f7b8c4026281168a2e5ff8e10af7d410a7558ec4b87ea85bb500b501006e0043ed1fe40593cf6a2ccb0777ad2154f5192550f893853cbb6ffe463725ed77679e41"
  , "3aff000001801af6f5ff7fff8200979501ff000080bd27f80170f9ff00984e01809bc68001004401bed800db7501618dc77f80ff00ff7a123001014948ab7f80017f107f52fffff501ff001f017fff7fff80fbfbaf010001ff01809dfffc008058a7490c4dffffffb1ff1a8052008090cd877f00687fcc7501de56ff0029ed01c100ee9eb7b7b2e2d68f100141321cd97f8ba37fb1ff29043246017f006980d0003bd641ed01ff7fff0d00207fffff80fdfc09000000038b0f29a60fa8d7e1aeafbe512288a6c6bc696547bbf8247db23c95e83014513c027a449c47fe1af599484cead9e22af00f360236f121e40b7e91ad0d285b22465b71541980ff6028800480000001"
  , "80170000dc017f55ff7f2c01fd0a00e606017ae800ff1537a5004affff2cf015ffdf807f80ff4efff0a77f018016c5018025689700007f7fdb0000ff4e592a7f18a2ffffffff007fff5821843c00005200d5ffa8ff809b5034b8c1b40001149400808963217f01af98e5ff676965c901007f83fb9bf3ff82ff2000ff80a1ff843636aba6d690cecaacdb49662543b30000bfff0100016238d2ffea1436d680ea9a85d7ff19a1ffff958073207f454cac838c09000000038b0f29a60fa8d7e1aeafbe512288a6c6bc696547bbf8247db23c95e83014513c0258c990197df8443897ff4fccd7aa33e7215df28ee735e3187eaf00fa4af3c5300180ff804bc57f0008463cad080696c5d5"
  , "806eff80808068c7ffa55b8f4d4189caff807a07ff7f7f00ffff47562f533c0153805d418780010300007f7fadb9eaff7f0101f3ff56d48000932fb6ffae098083800001ffb3ff4938ff7f80af80c700047f80017f7f0080de25ad007f01e3ffa1014801528081807f13ff3e01807f80925c7f00807ff300747f7fffdc2360ce80fefebbac96a398b9ec71017f80d27f7f01d43c898080ffff7f7e80807fff7f72bc2715ff05217f7fbe58207ffffff680edda05000000038b0f29a60fa8d7e1aeafbe512288a6c6bc696547bbf8247db23c95e83014513c031c0bbbb82fd8f61b5b6026bcd2e707096d39471d47672780492719b4bfa56af10480ff7f49c600010102"
  , "85d801ff01007fc42fd980808481c2247f1000ebccfe800080bd06800198012fe900cc0188014a005ecf7f158001a60180378080794f00feff0000ffdd33ff007fb7f10100aa7ff27f7f807f7f8c4d7f003e3900a001801c15c334bdf97f0780011d940001011531ff01a90180e68080d87fff7f457fc1177fb8c33eff018b6fff00b78ad2e98df4d8971400000080ede57f807f00f880f1e9807f017f7fb900ef8082ffea865adc94ff0001010000b9b0bcfe06000000038b0f29a60fa8d7e1aeafbe512288a6c6bc696547bbf8247db23c95e83014513c035073fac37a14bc28db84b4119c499decaa6af61abeb8a39a0253f9bf5c6e133c8008c78027a9841920fffffffffffffffffe2de2a8f374cddd272bf5811a3fbf42bc8f130869f6f6c9"
  , "930100430ec30100d7b381ff9de8000103ffff7a800185007f00e7880100fff1fa47d9c500e8eda0929d57ff53014e9cbe0165829bc900002380017f09e5d21f01801a007917b57d231a4dff84002f3f7f7f003a8d00737523007f7f9153ff807201d6010b3f013a7f00004b9896533b0163800180fb01cf000000e680b30bffe5ffc7aceff2dd93a8ce1c00290101a09380a8fa7fcc7f008001ff0187000125ff798d2280018000cfb8ff207fffffe7fcd68e04000000038b0f29a60fa8d7e1aeafbe512288a6c6bc696547bbf8247db23c95e83014513c035ca4cbb6b88b07f4b3f18a8497a768261e8092778ced231fa6da82b451da160d4ebdff00f1807ff617ba9e1e23289145ffeee5db0f366d54c56c30eb6b7fc601"
  , "ac000a0000012fd4f27fff7f72807fe2c0aa807f008601697face9217fd1c1ff007b3809b384013d01299b4e7f8094018080ff00380069800001ff7f010c7f970136016aff7f1d6080ffcc27801f7fff0080b30d7fff938001014180807f809d000001802d019fba02747f0106c67f0020017f488000947f00b2268a7f00ff728084d5e5e3818a84d7a747e8008071f26a02ff4500807a01c1feff29a862807f8dd80001007f017f4900ff10018653d5f4bddb04000000038b0f29a60fa8d7e1aeafbe512288a6c6bc696547bbf8247db23c95e83014513c02f606ec6a6c31b6a7165fb9fd2f885e21e14a02cb0cd688e34505975d90a0b5fcb57ffff700003a4920fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8c50364140"
  , "b6c680cbff804800004f00007f7f003691ff01007f138080700040017f7f7f7fffbfc438fc74ff01800a7f007f7f8001b9ffe3768021187f7f017f00c9b301177f017f77360100ffa3ff22ff016c7f459180017f7f80b58001df35807f3c0d00d20000722df27f800101ff39806c01ff9a80ff0e1992857f32d8c2d9017fff7f15fff8b4d795aef787ec59b9010080547f01005a51ff739600005d47d89fac10001199a0631b8001019a34010200008995f5a204000000038b0f29a60fa8d7e1aeafbe512288a6c6bc696547bbf8247db23c95e83014513c022c65954f3487dee276b6e402deb17d8b454aff60fa2678c3168b9ec9ad6bee9321987f7049767dff20fffffffffffffffffffffffffffffffebaaedce6af48a03b3fd25e8cd0364140"
  , "ff006a00bb9e808d01005da8807f48ff009780841f1901006480950700b9ff8067ff010001c40180558001914e007fff7f00c0e3b1d6ffb880cbdd7b007f7fe559d80100720164cd001300bfff34addc547fdc7fbd08ff32804d807ff3f00031ff9aff8001f0cb80001f353ed918de0157ff5cff7f7f0100b180808100358029807ffe9292dae4f5f1b02cffc2c536c801ca4ad1737f1e36ce2db0ffe1ff0000d0ffffcf010017161f8e7a1002088eb78de0c906000000038b0f29a60fa8d7e1aeafbe512288a6c6bc696547bbf8247db23c95e83014513c0338e50f272e9f8515fa55fab6bf5d9b994a748cf1eeeee951061ba310f314b9a453d9ff08677fff5120fffffffffffffffffffffffffffffffebaaedce6af48a03b51af00eeeaf65a5c"
  ]
