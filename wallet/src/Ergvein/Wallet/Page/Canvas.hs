module Ergvein.Wallet.Page.Canvas(
    elementPosition
  , rawJSCall
  -- JS Raw Calls
  , drawGridT
  , drawGridBorderT
  , drawLineT
  , drawLinesT
  , drawLineZeroT
  , clearCanvasT
  , beginPathT
  , strokeStyleT
  , strokeT
  , rectZeroT
  , fillRectT
  , rectT
  , lineWidthT
  , moveToT
  , lineToT
  -- Auxiliary types
  , ClientRect(..)
  , Square(..)
  , DrawCommand(..)
  , Position(..)
  , TouchState(..)
  , PatternTry(..)
  -- Canvas Type
  , CanvasOptions(..)
  , defCanvasOptions
  , createCanvas
  ) where

import Ergvein.Text
import Ergvein.Wallet.Monad

import Reflex.Dom

import           Data.Aeson.Types as A
import           Data.List  (find)
import           Data.List.Split
import           Data.Maybe
import           Data.Map   (fromList)
import qualified Data.Text        as T

import Language.Javascript.JSaddle hiding ((!!))

import qualified GHCJS.DOM.Types as JS

type Square  = (Double, Double, Double, Double)
type Position = (Double, Double)

data DrawCommand = AddSquare | Clear deriving (Show)

data TouchState = Pressed | Unpressed deriving (Show)

data PatternTry = FirstTry | SecondTry | Done deriving (Show)

data CanvasOptions = CanvasOptions {
  coWidth   :: !Int
, coHeight  :: !Int
, coId      :: !Text
, coClass   :: !Text
} deriving (Show)

defCanvasOptions :: Text -> CanvasOptions
defCanvasOptions t = CanvasOptions 256 256 t "canvas"

createCanvas :: MonadFrontBase t m => CanvasOptions -> m (Element EventResult GhcjsDomSpace t)
createCanvas CanvasOptions{..} = do
  (canvasEl , _) <- elAttr' "canvas" canvasAttrs blank
  pure canvasEl
  where
    canvasAttrs = fromList
      [ ("height", showt coHeight)
      , ("width" , showt coWidth)
      , ("id"    , coId)
      , ("class" , coClass)
      ]

jstFilter :: (Maybe Int, a) -> Bool
jstFilter (a,_) = case a of
  Just _ -> True
  Nothing -> False

concatMyLists :: [(Maybe Int, Square)] -> [(Maybe Int, Square)] -> [(Maybe Int, Square)]
concatMyLists a b = (\((mi1,f1),(mi2,f2)) -> case mi1 of
  Just n1 -> (Just n1, f1)
  Nothing -> case mi2 of
    Just n2 -> (Just n2, f2)
    Nothing -> (Nothing,f2)
  ) <$> (zip a b)

drawGridT :: Int -> Int -> [(Maybe Int, Square)] -> Text
drawGridT cW cH r = (clearCanvasT cW cH)
                    <> beginPathT
--                    <> (rectZeroT cW cH)
                    <> (T.concat  (fmap fillRects r))
                    <> strokeStyleT
                    <> strokeT
  where
    fillRects (mN, (a,b,c,d)) = case mN of
      Just _ -> fillRectT a b c d
      Nothing -> rectT a b c d

drawGridBorderT :: Int -> Int -> [(Maybe Int, Square)] -> Text
drawGridBorderT cW cH r = (clearCanvasT cW cH)
                    <> beginPathT
                    <> (rectZeroT cW cH)
                    <> (T.concat  (fmap fillRects r))
                    <> strokeStyleT
                    <> strokeT
  where
    fillRects (mN, (a,b,c,d)) = case mN of
      Just _ -> fillRectT a b c d
      Nothing -> rectT a b c d

clearCanvasT :: Int -> Int -> Text
clearCanvasT cW cH = " ctx.clearRect(0,0," <> (showt cW) <> "," <> (showt cH) <> "); "

beginPathT :: Text
beginPathT = " ctx.beginPath(); "

strokeStyleT :: Text
strokeStyleT = " ctx.strokeStyle = \"#000000\"; "

strokeT :: Text
strokeT = " ctx.stroke(); "

rectZeroT :: Int -> Int -> Text
rectZeroT cW cH = " ctx.rect(0,0," <> (showt cW) <> "," <> (showt cH) <> "); "

fillRectT :: Double -> Double -> Double -> Double -> Text
fillRectT a b c d = " ctx.fillRect(" <> (showt a) <> "," <> (showt b) <> "," <> (showt c) <> "," <> (showt d) <> "); "

rectT :: Double -> Double -> Double -> Double -> Text
rectT a b c d = " ctx.rect(" <> (showt a) <> "," <> (showt b) <> "," <> (showt c) <> "," <> (showt d) <> "); "

lineWidthT :: Int -> Text
lineWidthT lw = " ctx.lineWidth = " <> (showt lw) <> "; "

moveToT :: Double -> Double -> Text
moveToT mX mY = " ctx.moveTo(" <> (showt mX) <> "," <> (showt mY) <> "); "

lineToT :: Double -> Double -> Text
lineToT mX mY = " ctx.lineTo(" <> (showt mX) <> "," <> (showt mY) <> "); "

drawLineT :: Int -> Int -> Double -> Double -> Double -> Double -> (DrawCommand,(Double,Double)) -> [(Maybe Int, Square)] -> Text
drawLineT canvasW canvasH coordX coordY fromX fromY (a,(cntX,cntY)) r = case a of
  Clear ->  drawGridT canvasW canvasH r
  AddSquare -> (drawGridT canvasW canvasH r)
            <> (moveToT cntX cntY)
            <> (lineToT coordX coordY)
            <> strokeT
  --Save -> ""

drawLinesT :: (DrawCommand, [Maybe Int]) -> [(Int, Square)] -> Text
drawLinesT (dc, mi) z = case dc of
  AddSquare -> if ((length mi) < 2)
    then ""
    else T.concat $ fmap drawLs pointsList
      where
        (fjMi :: [Int]) = fmap fromJust mi
        (prepList :: [Int]) = ([head fjMi]) <> (concat (fmap (\a -> [a,a]) fjMi)) <> ([last fjMi])
        pointsList  = chunksOf 2 $ fmap (\a -> case (find (\(num,_) -> num == a ) z) of
            Just (num, (a,b,c,d)) -> (a+c/2,b+d/2)
            Nothing -> (0,0) ) prepList
        drawLs :: [(Double, Double)] -> Text
        drawLs [(ax,ay),(bx,by)] = beginPathT
                                <> (lineWidthT 2)
                                <> (moveToT ax ay)
                                <> (lineToT bx by)
                                <> strokeT
  Clear -> ""
  --Save -> ""

drawLineZeroT :: Text
drawLineZeroT = " ctx.moveTo(0,0); "

data ClientRect = ClientRect {
    crBottom :: !Double
  , crHeight :: !Double
  , crLeft   :: !Double
  , crRight  :: !Double
  , crTop    :: !Double
  , crWidth  :: !Double
  } deriving (Show)

instance FromJSON ClientRect where
  parseJSON = withObject "ClientRect" $ \o -> do
    crBottom <- o .: "bottom"
    crHeight <- o .: "height"
    crLeft   <- o .: "left"
    crRight  <- o .: "right"
    crTop    <- o .: "top"
    crWidth  <- o .: "width"
    pure ClientRect{..}

instance FromJSVal ClientRect where
  fromJSVal v = do
    av <- fromJSVal v
    case av of
      Nothing -> fail "Failed to convert ClientRect to aeson"
      Just a -> case fromJSON a of
        A.Error s -> fail $ "Failed to parse aeson ClientRect " ++ s
        A.Success b -> pure $ Just b

elementPosition :: MonadJSM m => RawElement GhcjsDomSpace -> m ClientRect
elementPosition el = liftJSM $ do
  eval ("ergvein_elementPosition = function(a) { return a.getBoundingClientRect(); }" :: Text)
  jsv <- liftJSM $ jsg1 ("ergvein_elementPosition" :: Text) (toJSVal el)
  fromJSValUnchecked jsv


rawJSBeginPath :: MonadJSM m => RawElement GhcjsDomSpace -> m ()
rawJSBeginPath el = liftJSM $ do
  eval func1
  _ <- liftJSM $ jsg1 func2 (toJSVal el)
  pure ()
  where
    (func2 :: Text) = "ergvein_drawtouchline"
    (func1 :: Text) = " ergvein_drawtouchline = function(cnv) { "
                   <> " var ctx = cnv.getContext(\"2d\");"
                   <> " ctx.beginPath(); "
                   <> " ctx.lineWidth = 2; "
                   <> " ctx.moveTo(0,0); "
                   <> " ctx.lineTo(100,100); "
                   <> " ctx.strokeStyle = \"#000000\"; "
                   <> " ctx.stroke(); "
                   <> " }"

rawJSCall :: MonadJSM m => RawElement GhcjsDomSpace -> Text -> m ()
rawJSCall el t = liftJSM $ do
  eval func1
  _ <- liftJSM $ jsg1 func2 (toJSVal el)
  pure ()
  where
    (func2 :: Text) = "ergvein_drawgrid"
    (func1 :: Text) = " ergvein_drawgrid = function(cnv) { " <> " var ctx = cnv.getContext(\"2d\");" <> t <> " }"