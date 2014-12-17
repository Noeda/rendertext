{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module implements an interface for rendering text on a pixel image,
-- with Pango and Cairo.
--
-- See `renderText` to start out.
--
-- Benchmarks are available if you want to get an intuition how fast it is to
-- render text. See "Graphics.Text#benchmarks".
--

module Graphics.Text
    ( 
    -- * Rendering text
      renderText
    , renderTextByteString
    , renderTextVector
    -- ** How large your buffers should be
    , inkRectangle
    , bytesNeededStride
    -- * Render context
    , newRenderContext
    -- ** Escape hatches
    , getPangoContext
    , newRenderContextFromPangoContext
    -- * Fonts
    , setFont
    -- * Types
    , Color
    , FontDesc
    , RenderContext()
    , V2(..)
    , V4(..)
    -- ** Image units
    , Columns
    , RowBytes
    , Rows
    -- ** Re-exported from pango/cairo
    , Format(..)
    , PangoRectangle(..)
    -- * Benchmarks
    -- $benchmarks
    )
    where

import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import Control.Lens
import Control.Monad ( void )
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Tuple ( swap )
import Data.Typeable ( Typeable )
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango hiding ( Color )
import Linear.V2
import Linear.V4
import qualified Data.Vector.Storable as V

foreign import ccall unsafe "memset" c_memset :: Ptr a -> CInt -> CSize -> IO (Ptr b)

type FontDesc = Text
type RowBytes = Int
type Rows = Int
type Columns = Int
type Width = Int

-- | This is a render context. It holds the internal state needed to render to
-- text efficiently.
--
-- `RenderContext` is safe to use from any thread, even simultaneously.
-- However, access is protected internally by `MVar` so two different threads
-- cannot be doing text rendering at the same with the same `RenderContext`,
-- other one will be blocked until the other finishes.
newtype RenderContext = RenderContext (MVar RenderContext_)
                        deriving ( Eq, Typeable )

instance NFData RenderContext where
    rnf (RenderContext !_) = ()

-- | Gets the internal Pango context from `RenderContext`.
getPangoContext :: MonadIO m => RenderContext -> m PangoContext
getPangoContext (RenderContext mvar) = liftIO $ withMVar mvar $ return . _pango

data RenderContext_ = RenderContext_
    { _cairo :: !CairoState
    , _pango :: !PangoContext }
    deriving ( Typeable )

data CairoState = CairoState
    { _colorSurface :: !(Maybe Surface)
    , _grayscaleSurface :: !(Maybe Surface) }
    deriving ( Typeable )
makeClassy ''CairoState
makeClassy ''RenderContext_

-- | Creates a new render context. See `RenderContext`.
newRenderContext :: MonadIO m => m RenderContext
newRenderContext = liftIO $
    newRenderContextFromPangoContext =<< cairoCreateContext Nothing

-- | Creates a rendering context from a given Pango context.
--
-- The context should be a pango/cairo context.
--
-- @
--     newRenderContext = newRenderContextFromPangoContext =<< cairoCreateContext Nothing
-- @
newRenderContextFromPangoContext :: MonadIO m => PangoContext -> m RenderContext
newRenderContextFromPangoContext ctx = liftIO $ do
    mvar <- newMVar (RenderContext_ {
                _cairo = CairoState {
                    _colorSurface = Nothing
                  , _grayscaleSurface = Nothing }
              , _pango = ctx })
    return $ RenderContext mvar

runStateTMVar :: MonadIO m => MVar a -> StateT a IO b -> m b
runStateTMVar mvar stateful = liftIO $
    modifyMVar mvar $ \state ->
        swap <$> runStateT stateful state

-- | Choose a font to use in a `RenderContext`.
--
-- This is a description of the font, not a file name. See this piece of Pango
-- documentation for instruction on what to put here.
-- <https://developer.gnome.org/pango/stable/pango-Fonts.html#pango-font-description-from-string>.
--
-- One example: @ \"Droid Sans Mono 14\" @.
setFont :: MonadIO m => RenderContext -> FontDesc -> m ()
setFont (RenderContext mvar) fdesc = liftIO $ do
    desc <- fontDescriptionFromString fdesc
    runStateTMVar mvar $ do
        ctx <- use pango
        liftIO $ contextSetFontDescription ctx desc

-- | Given a `PangoRectangle`, returns a rectangle whose edges are aligned to
-- pixels but always contains the original rectangle.
pixelify :: PangoRectangle -> PangoRectangle
pixelify (PangoRectangle x y w h) =
    PangoRectangle (fromIntegral fx)
                   (fromIntegral fy)
                   (fromIntegral $ right-fx)
                   (fromIntegral $ bottom-fy)
  where
    fx = floor x :: Int
    fy = floor y :: Int

    right = ceiling $ x+w
    bottom = ceiling $ y+h

-- | Calculates the offset and size of a rectangle that bounds the text.
--
-- @ PangoRectangle x y w h @.
--
-- You will want to use the x, y position to offset the text. You can use w, h
-- to determine how large your buffer needs to be for `renderText` family of
-- functions.
--
-- The left and top of the rectangle are not necessarily (and often are not)
-- zero. Use the values to offset your result. The values are, however,
-- guaranteed to be integer:
--
-- @
--     PangoRectangle x y w h <- inkRectangle ...
--     -- x == floor x, y == floor y, w == floor w, h == floor h
-- @
inkRectangle :: MonadIO m
             => Text              -- ^ The text you would render.
             -> Maybe Width       -- ^ Rectangle width, in pixels. This can affect the ink rectangle because of word wrapping.
             -> RenderContext
             -> m PangoRectangle
inkRectangle txt width (RenderContext mvar) = liftIO $ do
    withMVar mvar $ \(_pango -> ctx) -> do
        layout <- liftIO $ layoutText ctx txt
        case width of
            Just w ->
                liftIO $ layoutSetWidth layout (Just $ fromIntegral w)
            _ -> return ()
        (ink@(PangoRectangle _ iy _ ih), logical) <- layoutGetExtents layout
        -- We are sneaky here. We use ink extent height but logical width.
        --
        -- We have to use logical width because otherwise Pango will word-wrap
        -- for us when we don't expect it to (ink width is too narrow).
        return $ let PangoRectangle cx _ cw _ = containsBoth ink logical
                  in pixelify $ PangoRectangle cx iy cw ih

-- | Given two pango rectangles, returns the one that contains both.
containsBoth :: PangoRectangle -> PangoRectangle -> PangoRectangle
containsBoth (PangoRectangle x1 y1 w1 h1) (PangoRectangle x2 y2 w2 h2) =
    PangoRectangle x y (max right1 right2 - x) (max bottom1 bottom2 - y)
  where
    x = min x1 x2
    y = min y1 y2

    right1 = x1+w1
    bottom1 = y1+h1
    right2 = x2+w2
    bottom2 = y2+h2

-- | Color types, values range from 0 to 1.
--
-- @ V4 red green blue alpha @
type Color = V4 Double

-- | Renders text.
--
-- This is the most generic rendering function in this module.
--
-- You should know how to calculate how many bytes your image pointer needs.
--
-- Some examples: `FormatA8` wants RowBytes*Rows bytes. `FormatRGB24` wants
-- RowBytes*Rows*4 bytes (yes 4, not 3). `FormatARGB32` wants RowBytes*Rows*4
-- bytes.
--
-- `FormatA1` uses 1 bit per pixel but pixels are packed into 32-bit units with
-- machine-dependent endianness. See:
-- <http://cairographics.org/manual/cairo-Image-Surfaces.html#cairo-format-t>.
--
-- This is a complete example from the text-to-image toy program in this
-- package that writes a text and uses JuicyPixels to write it in a file:
--
-- @
-- makeTextToImage :: FilePath -> Text -> Text -> IO ()
-- makeTextToImage filename fdesc string = do
--     ctx <- newRenderContext
--     setFont ctx fdesc
--     PangoRectangle x y w h <- inkRectangle string Nothing ctx
--     let iw = floor w
--         ih = floor h
--     (vec, _) <- renderTextVector string iw
--                                         ih
--                                         FormatARGB32
--                                         (V2 (-x) (-y))
--                                         (V4 0 0 0 1)
--                                         ctx
--     -- Generate a JuicyPixels image out of the vector.
--     let img = generateImage (\x y ->
--                   let offset = (x+y*iw)*4
--                    in PixelRGBA8 (vec V.! offset)
--                                  (vec V.! (offset+1))
--                                  (vec V.! (offset+2))
--                                  (vec V.! (offset+3)))
--                             iw
--                             ih
--     writePng filename img
--     -- Another program might want to offset the image \"back\" e.g.:
--     -- renderSomething vec xlocation ylocation (Offset (V2 x y))
-- @
--

renderText :: MonadIO m
           => Text           -- ^ The text you want to render.
           -> Ptr a          -- ^ Where to put the raw text (this is a pointer to image data).
           -> Columns        -- ^ How many columns there are in your image.
           -> Rows           -- ^ How many rows there are in your image.
           -> RowBytes       -- ^ Number of bytes there are in a row in the given pointer. This is also known as stride or pitch. It should be a multiple of 4.
           -> Format         -- ^ Which format do you want to use? This affects how many bytes are used per pixel.
           -> V2 Double      -- ^ Offset the rendering by this amount.
           -> Color          -- ^ Which color to use?
           -> RenderContext  -- ^ Render context.
           -> m ()
renderText txt
           ptr
           cols
           rows
           row_bytes
           format
           (V2 ox oy)
           (V4 red green blue alpha)
           (RenderContext mvar) =
    runStateTMVar mvar $ do
        surf <- liftIO $
                createImageSurfaceForData
                (castPtr ptr)
                format
                cols
                rows
                row_bytes
        ctx <- use pango
        liftIO $ renderWith surf $ do
            layout <- liftIO $ layoutText ctx txt
            liftIO $ layoutSetWidth layout (Just $ fromIntegral cols)
            liftIO $ layoutSetText layout (T.unpack txt)
            setSourceRGBA red green blue alpha
            translate ox oy
            showLayout layout

-- | Renders text to a strict bytestring.
--
-- This creates a bytestring of given size and then renders the text on it.
--
-- Note: at the moment, `FormatA1` is not supported as a format for this
-- function.
renderTextByteString :: MonadIO m
                     => Text
                     -> Columns
                     -> Rows
                     -> Format
                     -> V2 Double
                     -> Color
                     -> RenderContext
                     -> m (B.ByteString, RowBytes)
renderTextByteString txt cols rows format offset color ctx = liftIO $ mask_ $ do
    ptrs <- mallocBytes bytes_needed
    void $ c_memset ptrs 0 (fromIntegral bytes_needed)
    onException
        (renderText txt ptrs cols rows stride format offset color ctx)
        (free ptrs)
    flip (,) stride <$>
        B.unsafePackMallocCStringLen (castPtr ptrs, bytes_needed)
  where
    (bytes_needed, stride) = bytesNeededStride cols rows format

-- | Given number of columns, rows and format, returns number of bytes needed
-- to store the result and the bytes needed per row.
bytesNeededStride :: Columns -> Rows -> Format -> (Int, RowBytes)
bytesNeededStride cols rows format =
    case format of
        FormatA8 ->
            let cols4 = ((cols+3 `div` 4) * 4)
             in (cols4*rows, cols4)
        FormatRGB24 -> (cols*rows*4, cols*4)
        FormatARGB32 -> (cols*rows*4, cols*4)

        -- I'm lazy...
        FormatA1 ->
            error "Graphics.Text.bytesNeededStride: FormatA1 is not supported."

-- | Renders text to a storable Word8 vector.
--
-- Works with the same principle as `renderTextByteString`.
renderTextVector :: MonadIO m
                 => Text
                 -> Columns
                 -> Rows
                 -> Format
                 -> V2 Double
                 -> Color
                 -> RenderContext
                 -> m (V.Vector Word8, RowBytes)
renderTextVector txt cols rows format offset color ctx = liftIO $ mask_ $ do
    fptr <- mallocForeignPtrBytes bytes_needed
    withForeignPtr fptr $ \ptr -> do
        void $ c_memset ptr 0 (fromIntegral bytes_needed)
        renderText txt (castPtr ptr) cols rows stride format offset color ctx
        return (V.unsafeFromForeignPtr0 fptr bytes_needed, stride)
  where
    (bytes_needed, stride) = bytesNeededStride cols rows format


-- $benchmarks
-- #benchmarks#
--
-- These are benchmarks measuring how fast the functions are in this module
-- are. This should help you build intuition so you can answer questions such
-- as \"How much can I render with these functions in one frame before I need
-- to take special measures?\".
--
-- The benchmark is available in the .cabal file of this package so you can run
-- these yourself too.
--
-- The benchmarks measure the three text rendering functions and
-- `inkRectangle`. The font is set to \"DejaVu Sans 28\". The benchmarks were
-- run on a Macbook Pro Retina from late-2013, running Linux 64-bit, GHC 7.8.3.
--
-- @
-- benchmarking meta/newRenderContext
-- time                 1.360 μs   (1.140 μs .. 1.639 μs)
--                      0.806 R²   (0.727 R² .. 0.920 R²)
-- mean                 1.706 μs   (1.484 μs .. 2.054 μs)
-- std dev              1.077 μs   (616.1 ns .. 1.614 μs)
-- variance introduced by outliers: 99% (severely inflated)
-- 
-- benchmarking meta/setFont
-- time                 1.043 μs   (1.033 μs .. 1.060 μs)
--                      0.998 R²   (0.994 R² .. 1.000 R²)
-- mean                 1.051 μs   (1.042 μs .. 1.078 μs)
-- std dev              52.10 ns   (15.76 ns .. 95.64 ns)
-- variance introduced by outliers: 66% (severely inflated)
-- 
-- benchmarking rendering/renderText[\"A\"]
-- time                 26.37 μs   (25.95 μs .. 26.77 μs)
--                      0.998 R²   (0.997 R² .. 0.998 R²)
-- mean                 25.82 μs   (25.26 μs .. 26.33 μs)
-- std dev              1.620 μs   (1.410 μs .. 1.889 μs)
-- variance introduced by outliers: 68% (severely inflated)
-- 
-- benchmarking rendering/renderByteString[\"A\"]
-- time                 27.62 μs   (26.69 μs .. 28.51 μs)
--                      0.994 R²   (0.992 R² .. 0.996 R²)
-- mean                 27.49 μs   (26.74 μs .. 28.05 μs)
-- std dev              2.261 μs   (1.951 μs .. 2.599 μs)
-- variance introduced by outliers: 79% (severely inflated)
-- 
-- benchmarking rendering/renderVector[\"A\"]
-- time                 27.33 μs   (26.62 μs .. 28.04 μs)
--                      0.994 R²   (0.992 R² .. 0.997 R²)
-- mean                 29.17 μs   (28.38 μs .. 30.05 μs)
-- std dev              2.927 μs   (2.494 μs .. 3.528 μs)
-- variance introduced by outliers: 85% (severely inflated)
-- 
-- benchmarking rendering/inkRectangle[\"A\"]
-- time                 4.717 μs   (4.658 μs .. 4.778 μs)
--                      0.998 R²   (0.997 R² .. 0.999 R²)
-- mean                 4.846 μs   (4.732 μs .. 5.182 μs)
-- std dev              678.9 ns   (230.2 ns .. 1.472 μs)
-- variance introduced by outliers: 93% (severely inflated)
-- 
-- benchmarking rendering/renderText[\"12345\"]
-- time                 37.72 μs   (37.32 μs .. 38.17 μs)
--                      0.998 R²   (0.996 R² .. 1.000 R²)
-- mean                 37.48 μs   (37.17 μs .. 38.21 μs)
-- std dev              1.521 μs   (986.6 ns .. 2.639 μs)
-- variance introduced by outliers: 45% (moderately inflated)
-- 
-- benchmarking rendering/renderByteString[\"12345\"]
-- time                 42.18 μs   (41.17 μs .. 43.33 μs)
--                      0.997 R²   (0.996 R² .. 0.999 R²)
-- mean                 41.71 μs   (41.33 μs .. 42.26 μs)
-- std dev              1.534 μs   (1.127 μs .. 2.243 μs)
-- variance introduced by outliers: 40% (moderately inflated)
-- 
-- benchmarking rendering/renderVector[\"12345\"]
-- time                 38.28 μs   (38.00 μs .. 38.52 μs)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 38.16 μs   (38.02 μs .. 38.29 μs)
-- std dev              513.3 ns   (418.7 ns .. 631.4 ns)
-- 
-- benchmarking rendering/inkRectangle[\"12345\"]
-- time                 6.072 μs   (5.958 μs .. 6.220 μs)
--                      0.997 R²   (0.995 R² .. 0.999 R²)
-- mean                 6.041 μs   (5.959 μs .. 6.190 μs)
-- std dev              370.1 ns   (234.4 ns .. 535.6 ns)
-- variance introduced by outliers: 71% (severely inflated)
-- 
-- benchmarking rendering/renderText[\"Hello world!\"]
-- time                 56.89 μs   (56.56 μs .. 57.19 μs)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 56.67 μs   (56.45 μs .. 56.89 μs)
-- std dev              744.8 ns   (598.2 ns .. 1.009 μs)
-- 
-- benchmarking rendering/renderByteString[\"Hello world!\"]
-- time                 68.58 μs   (67.85 μs .. 69.44 μs)
--                      0.999 R²   (0.997 R² .. 1.000 R²)
-- mean                 68.38 μs   (67.98 μs .. 68.82 μs)
-- std dev              1.365 μs   (1.096 μs .. 1.701 μs)
-- variance introduced by outliers: 15% (moderately inflated)
-- 
-- benchmarking rendering/renderVector[\"Hello world!\"]
-- time                 59.97 μs   (59.32 μs .. 60.61 μs)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 58.85 μs   (58.45 μs .. 59.39 μs)
-- std dev              1.453 μs   (1.235 μs .. 1.882 μs)
-- variance introduced by outliers: 22% (moderately inflated)
-- 
-- benchmarking rendering/inkRectangle[\"Hello world!\"]
-- time                 9.466 μs   (9.396 μs .. 9.562 μs)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 9.456 μs   (9.406 μs .. 9.557 μs)
-- std dev              237.4 ns   (100.7 ns .. 374.3 ns)
-- variance introduced by outliers: 27% (moderately inflated)
-- 
-- benchmarking rendering/renderText[\"宁夏回族自治区\"]
-- time                 67.50 μs   (66.71 μs .. 68.24 μs)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 67.88 μs   (67.47 μs .. 68.38 μs)
-- std dev              1.479 μs   (1.043 μs .. 2.332 μs)
-- variance introduced by outliers: 18% (moderately inflated)
-- 
-- benchmarking rendering/renderByteString[\"宁夏回族自治区\"]
-- time                 81.40 μs   (80.37 μs .. 82.34 μs)
--                      0.998 R²   (0.997 R² .. 0.999 R²)
-- mean                 80.61 μs   (79.73 μs .. 81.46 μs)
-- std dev              2.977 μs   (2.458 μs .. 3.878 μs)
-- variance introduced by outliers: 38% (moderately inflated)
-- 
-- benchmarking rendering/renderVector[\"宁夏回族自治区\"]
-- time                 71.86 μs   (71.35 μs .. 72.25 μs)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 71.93 μs   (71.32 μs .. 72.58 μs)
-- std dev              2.070 μs   (1.698 μs .. 2.885 μs)
-- variance introduced by outliers: 27% (moderately inflated)
-- 
-- benchmarking rendering/inkRectangle[\"宁夏回族自治区\"]
-- time                 7.901 μs   (7.857 μs .. 7.955 μs)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 7.875 μs   (7.845 μs .. 7.903 μs)
-- std dev              101.0 ns   (78.77 ns .. 128.6 ns)
-- 
-- @
--

