{-# LANGUAGE ViewPatterns #-}

module Main ( main ) where

import Codec.Picture
import Data.List ( intersperse )
import Data.Monoid
import Graphics.Text
import System.Environment
import System.Exit
import qualified Data.Text as T
import qualified Data.Vector.Storable as V

main :: IO ()
main = do
    args <- getArgs
    if length args < 3
      then do
          putStrLn "Usage: text-to-image FILENAME FONT TEXT"
          putStrLn "An image file, depicting TEXT, will be written as a png image to FILENAME, using FONT."
          putStrLn "Example: text-to-image hello.png \"Droid Sand Mono 32\" \"Hello world!\""
          exitFailure
      else let f = args !! 0
               font = args !! 1
               txt = mconcat $ intersperse " " (tail $ tail args)
            in makeTextToImage f font txt

makeTextToImage :: FilePath -> String -> String -> IO ()
makeTextToImage filename (T.pack -> fdesc) (T.pack -> string) = do
    ctx <- newRenderContext
    setFont ctx fdesc
    PangoRectangle x y w h <- inkRectangle string Nothing ctx
    let iw = floor w
        ih = floor h
    (vec, _) <- renderTextVector string iw
                                        ih
                                        FormatARGB32
                                        (V2 (-x) (-y))
                                        (V4 0 0 0 1)
                                        ctx
    -- Generate a JuicyPixels image out of the vector.
    let img = generateImage (\x y ->
                  let offset = (x+y*iw)*4
                   in PixelRGBA8 (vec V.! offset)
                                 (vec V.! (offset+1))
                                 (vec V.! (offset+2))
                                 (vec V.! (offset+3)))
                            iw
                            ih
    writePng filename img

