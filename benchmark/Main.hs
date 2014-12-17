{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Monad
import Criterion
import Criterion.Main
import Data.Text ( Text )
import qualified Data.Text as T
import Foreign.Marshal.Alloc
import Graphics.Text

setupEnv :: IO RenderContext
setupEnv = do
    ctx <- newRenderContext
    setFont ctx "DejaVu Sans 28"
    return ctx

main :: IO ()
main = defaultMain [
          bgroup "meta" [
            bench "newRenderContext" $ whnfIO newRenderContext
          , env setupEnv $ \ctx ->
              bench "setFont" $ whnfIO $ setFont ctx "Droid Sans 28"
        ]
        , env setupEnv $ \ctx -> bgroup "rendering" [
          bench (snd fun ++ "[\"" ++ T.unpack str ++ "\"]") $ whnfIO $ (fst fun) ctx str |
              str <- ["A", "12345", "Hello world!", "宁夏回族自治区"]
            , fun <- [(renderPlainString, "renderText")
                     ,(renderByteString, "renderByteString")
                     ,(renderVector, "renderVector")
                     ,(plainInkRectangle, "inkRectangle")]
        ]
    ]

plainInkRectangle :: RenderContext -> Text -> IO ()
plainInkRectangle ctx txt = do
    void $ inkRectangle txt Nothing ctx
    return ()

renderByteString :: RenderContext -> Text -> IO ()
renderByteString ctx txt = do
    PangoRectangle x y w h <- inkRectangle txt Nothing ctx
    let iw = floor w
        ih = floor h
    void $ renderTextByteString txt iw ih FormatARGB32 (V2 (-x) (-y)) (V4 0 0 0 1) ctx

renderVector :: RenderContext -> Text -> IO ()
renderVector ctx txt = do
    PangoRectangle x y w h <- inkRectangle txt Nothing ctx
    let iw = floor w
        ih = floor h
    void $ renderTextVector txt iw ih FormatARGB32 (V2 (-x) (-y)) (V4 0 0 0 1) ctx

renderPlainString :: RenderContext -> Text -> IO ()
renderPlainString ctx txt = do
    PangoRectangle x y w h <- inkRectangle txt Nothing ctx
    let iw = floor w
        ih = floor h
        (bytes, stride) = bytesNeededStride iw ih FormatARGB32
    allocaBytes bytes $ \ptr -> do
        renderText txt ptr iw ih stride FormatARGB32
                   (V2 (-x) (-y)) (V4 0 0 0 1) ctx

