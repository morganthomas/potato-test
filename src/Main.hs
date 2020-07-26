{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module Main where


import           Prelude hiding ( div )
import           Control.Monad ( forM_ )
import           Data.Text
import           Language.Javascript.JSaddle
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html
import           Shpadoinkle.Html.Utils
import           Shpadoinkle.Widgets.Types ( Humanize (..) )

default (ClassList)


data Layer = Aerial | AerialWithLabelsOnDemand | RoadOnDemand | CanvasDark | OrdnanceSurvey
  deriving (Eq, Show, Enum, Bounded)


instance Humanize Layer where
  humanize Aerial = "Aerial"
  humanize AerialWithLabelsOnDemand = "Aerial with labels"
  humanize RoadOnDemand = "Road"
  humanize CanvasDark = "Road dark"
  humanize OrdnanceSurvey = "Ordnance survey"


data Model = Model { mMap :: RawNode, mLayer :: Layer }


instance Eq Model where
  m == n = mLayer m == mLayer n


instance Show Model where
  show = show . mLayer


view :: Monad m => Model -> HtmlM m Model
view m =
  div [ id' "page" ] [
    PotatoM (return $ mMap m),
    liftMC (\l m' -> m' { mLayer = l }) mLayer .
    select [ id' "layer-select", value (pack (show (mLayer m))) ] $ layerOption <$> [minBound..maxBound] ]
  where layerOption l = option [ value (pack (show l)), onClick l ] [ text (humanize l) ]


main :: IO ()
main = runJSorWarp 8080 $ do
  addStyle "https://cdn.jsdelivr.net/gh/openlayers/openlayers.github.io@master/en/v6.3.1/css/ol.css"
  addScriptSrc "https://cdn.jsdelivr.net/gh/openlayers/openlayers.github.io@master/en/v6.3.1/build/ol.js"
  infinity <- toJSVal ((1.0/0.0) :: Double)
  openLayers <- jsg ("ol" :: Text)
  olLayerCtor <- openLayers ! ("TileLayer" :: Text)
  olBingMapsCtor <- openLayers ! ("BingMaps" :: Text)
  olLayers <- array ([] :: [JSVal])
  forM_ [minBound..maxBound] $ \(layer :: Layer) -> do
    olBingMapArgs <- obj
    (olBingMapArgs <# ("key" :: Text)) <$> toJSVal ("AmNkXbNpoH-6RYX42lfQcNzEXUXBSfDwPHJEAhDNH0EOToN99hKICJ4eq7K35BLh" :: Text)
    (olBingMapArgs <# ("imagerySet" :: Text)) <$> toJSVal (show layer)
    olBingMap <- new olBingMapsCtor =<< toJSVal olBingMapArgs
    olLayerArgs <- obj
    olLayerArgs <# ("visible" :: Text) $ False
    olLayerArgs <# ("preload" :: Text) $ infinity
    (olLayerArgs <# ("source" :: Text)) =<< toJSVal olBingMap
    olLayer <- toJSVal =<< new olLayerCtor =<< toJSVal olLayerArgs
    olLayers # ("push" :: Text) $ olLayer
  olViewCtor <- openLayers ! ("View" :: Text)
  olViewArgs <- obj
  olViewArgs <# ("center" :: Text) $ array [-6655.5402445057125 :: Double, 6709968.258934638]
  (olViewArgs <# ("zoom" :: Text)) =<< toJSVal (13 :: Int)
  olView <- new olViewCtor =<< toJSVal olViewArgs
  olMapCtor <- openLayers ! ("Map" :: Text)
  olMapArgs <- obj
  (olMapArgs <# ("target" :: Text)) <$> toJSVal ("map" :: Text)
  olMapArgs <# ("view" :: Text) $ olView
  olMapArgs <# ("layers" :: Text) $ olLayers
  olMapNode <- RawNode <$> (new olMapCtor =<< toJSVal olMapArgs)
  simple runParDiff (Model olMapNode Aerial) view getBody
