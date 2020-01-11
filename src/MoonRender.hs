module MoonRender
    ( render
    ) where

import Graphics.Image
import Graphics.Image.Interface
import Data.List

pixToMoon :: Double -> Maybe Double -> Maybe Double -> Char
pixToMoon selfLuma left right = case (left, right) of
      (Just leftLuma, Just rightLuma) -> if leftLuma < rightLuma then snd pair else fst pair
      (Nothing, Just rightLuma) -> if selfLuma < rightLuma then snd pair else fst pair
      (Just leftLuma, Nothing) -> if leftLuma < selfLuma then snd pair else fst pair
      _ -> fst pair
  where
    moonPairs= [('🌑', '🌑'), ('🌘', '🌒'), ('🌗', '🌓'), ('🌖', '🌔'),  ('🌕', '🌕')]
    pairCount = fromIntegral $ length moonPairs
    pair = moonPairs !! (fromEnum $ (pairCount-1) * selfLuma)

renderToMoons :: Int -> Image VU Y Double -> String
renderToMoons width img = intercalate "\n" lines
  where
    img' = resize Bilinear Edge ((fromIntegral $ (rows img) * width) `div` (fromIntegral $ cols img), width) img
    lines = do
      (i, row) <- zip [0..] $ toLists img'
      return $ do
        (j, pix) <- zip [0..] row
        let getPixV p = getPxC p LumaY in
          return $ pixToMoon (getPixV pix) (fmap getPixV $ maybeIndex img' (i, j-1)) (fmap getPixV $ maybeIndex img' (i, j+1))

render :: FilePath -> Int -> IO String
render filePath width = do
  img <- readImageY VU filePath
  return $ renderToMoons width img
