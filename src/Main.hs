{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module Main where


import           Prelude hiding ( div, (!!) )
import           Control.Monad ( forM_, join, void )
import           Data.Functor ( (<&>) )
import           Data.Text hiding ( zip, length )
import           Text.Read ( readMaybe )
import           Language.Javascript.JSaddle
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html
import           Shpadoinkle.Html.Utils
import           Shpadoinkle.Widgets.Types ( Humanize (..) )

default (ClassList)


data Layer = RoadOnDemand | Aerial | AerialWithLabelsOnDemand | CanvasDark | OrdnanceSurvey
  deriving (Eq, Read, Show, Enum, Bounded)


numLayers :: Int
numLayers = length [minBound :: Layer .. maxBound]


instance Humanize Layer where
  humanize Aerial = "Aerial"
  humanize AerialWithLabelsOnDemand = "Aerial with labels"
  humanize RoadOnDemand = "Road"
  humanize CanvasDark = "Road dark"
  humanize OrdnanceSurvey = "Ordnance survey"


data Model = Model { mMap :: RawNode, mLayers :: Object, mLayer :: Layer }


instance Eq Model where
  m == n = mLayer m == mLayer n


instance Show Model where
  show = show . mLayer


view :: MonadJSM m => Model -> HtmlM m Model
view m =
  div [ id' "page" ] [
    PotatoM (return $ mMap m),
    select [ id' "layer-select", value (pack (show (mLayer m))), onChangeP ]
      $ layerOption <$> [minBound..maxBound] ]

  where layerOption :: Monad m => Layer -> HtmlM m a
        layerOption l = option [ value (pack (show l)) ] [ text (humanize l) ]

        onChangeP :: MonadJSM m => (Text, PropM m Model)
        onChangeP = listenRaw "onchange" onChangeHandler

        onChangeHandler :: MonadJSM m => RawNode -> RawEvent -> JSM (Continuation m Model)
        onChangeHandler (RawNode selectNode) _ = return . kleisli $ \m -> liftJSM $ do
          mv <- selectNode ! ("value" :: Text) >>= fromJSVal >>= return . join . fmap readMaybe
          case mv of
            Just l -> do
              forM_ (zip [minBound..maxBound] [0..numLayers-1]) $ \((l',i) :: (Layer, Int)) -> do
                layer <- mLayers m !! i
                (layer <# ("setVisible" :: Text)) =<< toJSVal (l' == l)
              return . pur $ \m' -> m' { mLayer = l }


runApp :: JSM ()
runApp = do
  console <- jsg ("console" :: Text)
  let log x = (console # ("log" :: Text)) =<< toJSVal (x :: Text)
  log "running app"
  infinity <- eval ("Infinity" :: Text)
  console # ("log" :: Text) $ infinity
  win <- jsg ("window" :: Text)
  openLayers <- win ! ("ol" :: Text)
  openLayers_layer <- openLayers ! ("layer" :: Text)
  openLayers_source <- openLayers ! ("source" :: Text)
  olLayerCtor <- openLayers_layer ! ("Tile" :: Text)
  olBingMapsCtor <- openLayers_source ! ("BingMaps" :: Text)
  olLayers <- array ([] :: [JSVal])
  forM_ [minBound..maxBound] $ \(layer :: Layer) -> do
    olBingMapArgs <- obj
    (olBingMapArgs <# ("key" :: Text)) <$> toJSVal ("AmNkXbNpoH-6RYX42lfQcNzEXUXBSfDwPHJEAhDNH0EOToN99hKICJ4eq7K35BLh" :: Text)
    log (pack (show layer))
    (olBingMapArgs <# ("imagerySet" :: Text)) <$> toJSVal (pack $ show layer)
    
    log "new BingMaps"
    olBingMap <- new olBingMapsCtor =<< toJSVal olBingMapArgs
    olLayerArgs <- obj
    olLayerArgs <# ("visible" :: Text) $ True -- TODO False
    olLayerArgs <# ("preload" :: Text) $ infinity
    (olLayerArgs <# ("source" :: Text)) =<< toJSVal olBingMap
    log "new Tile"
    olLayer <- toJSVal =<< new olLayerCtor =<< toJSVal olLayerArgs
    olLayers # ("push" :: Text) $ olLayer
  olViewCtor <- openLayers ! ("View" :: Text)
  olViewArgs <- obj
  olViewArgs <# ("center" :: Text) $ array [-6655.5402445057125 :: Double, 6709968.258934638]
  (olViewArgs <# ("zoom" :: Text)) =<< toJSVal (13 :: Int)
  log "new View"
  olView <- new olViewCtor =<< toJSVal olViewArgs
  olMapCtor <- openLayers ! ("Map" :: Text)
  olMapArgs <- obj
  (olMapArgs <# ("target" :: Text)) <$> toJSVal ("map" :: Text)
  olMapArgs <# ("view" :: Text) $ olView
  olMapArgs <# ("layers" :: Text) $ olLayers
  log "new Map"
   -- Insert map directly in DOM and bypass S11 in order to test up to this point
  log "append map"
  doc <- win ! ("document" :: Text)
  bod <- doc ! ("body" :: Text)
  div <- (doc # ("createElement" :: Text)) =<< toJSVal ("div" :: Text)
  (div <# ("id" :: Text)) =<< toJSVal ("map" :: Text)
  (div <# ("className" :: Text)) =<< toJSVal ("map" :: Text)
  void $ win ! ("document" :: Text) >>= (! ("body" :: Text)) >>= ($ div) . (# ("appendChild" :: Text))

  olMap <- new olMapCtor =<< toJSVal olMapArgs
  (olLayers !! (0 :: Int) <&> (# ("setVisible" :: Text))) <*> toJSVal True
  --simple runParDiff (Model olMapNode olLayers Aerial) view getBody
  void $ log "appended map"


main :: IO ()
main = runJSorWarp 8080 $ do
  addStyle "https://cdn.jsdelivr.net/gh/openlayers/openlayers.github.io@master/en/v6.3.1/css/ol.css"
  addInlineStyle (".map { width: 100%; height: 500px }" :: Text)
  addScriptSrc "https://cdn.jsdelivr.net/gh/openlayers/openlayers.github.io@master/en/v6.3.1/build/ol.js"
  -- wait for dependencies to load, then run app
  win <- jsg ("window" :: Text)
  ready <- toJSVal (const . const . const $ runApp :: JSVal -> JSVal -> [JSVal] -> JSM ())
  wait <- toJSVal (1000 :: Int)
  void $ win # ("setTimeout" :: Text) $ [ ready, wait ]
  --runApp
