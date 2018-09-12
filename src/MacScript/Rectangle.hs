-- | Storable rectangles that can be used with the Apple Sdk functions. 

module MacScript.Rectangle
  ( Rect(..)
  , Point(..)
  , Size(..)
  , rectFromCorners
  , area
  , intersection
  , removeFromWidthStable
  , removeFromHeightStable
  , pad
  , rectFloor
  , splitHorizontally
  , splitVertically
  , splitHorizontallyN
  , splitVerticallyN
  , rectDist
  , center

  -- * Lenses
  , originL
  , sizeL
  , xL
  , yL
  , widthL
  , heightL
  
  ) where

import MacSdk.Framework.CoreGraphics.Rect

import Lens.Micro

left, right, top, bottom :: Rect -> Double
left = (^.originL.xL)
right r = r^.originL.xL + r^.sizeL.widthL
top = (^.originL.yL)
bottom r = r^.originL.yL + r^.sizeL.heightL

-- | Computes the area of a rectangle.
area :: Rect -> Double
area r = (r^.sizeL.widthL) * (r^.sizeL.heightL)

-- | Computes the intersection of two rectangles.
intersection :: Rect -> Rect -> Rect
intersection r1 r2 =
  rectFromCorners
    (Point (max (left r1) (left r2)) (max (top r1) (top r2)))
    (Point (min (right r1) (right r2)) (min (bottom r1) (bottom r2)))

-- | Creates a rectangle from the top-left and bottom-right corners.
rectFromCorners :: Point -> Point -> Rect
rectFromCorners (Point x1 y1) (Point x2 y2) = Rect (Point x y) (Size w h)
  where
    x = min x1 x2
    y = min y1 y2
    w = abs (x1-x2)
    h = abs (y1-y2)

rectFloor :: Rect -> Rect
rectFloor =
  over originL (over xL floorD . over yL floorD) .
  over sizeL (over widthL floorD . over heightL floorD)
  where floorD = (fromIntegral :: Int -> Double) . floor

addToX, addToY :: Double -> Point -> Point
addToX d = over xL (+d)
addToY d = over yL (+d)

addToWidth, addToHeight :: Double -> Size -> Size
addToWidth d = over widthL (+d)
addToHeight d = over heightL (+d)

halfHeight, halfWidth :: Size -> Size
halfHeight = over heightL (/2)
halfWidth = over widthL (/2)

-- | Adds a 'Double' value to both the left and right sides of a rectangle,
-- while keeping it centered on the same point.
removeFromWidthStable :: Double -> Rect -> Rect
removeFromWidthStable d =
  over originL (over xL (+ d)) . over sizeL (over widthL (\x -> x - d * 2))

-- | Adds a 'Double' value to both the top and bottom sides of a rectangle,
-- while keeping it centered on the same point.
removeFromHeightStable :: Double -> Rect -> Rect
removeFromHeightStable d =
  over originL (over yL (+ d)) . over sizeL (over heightL (\x -> x - d * 2))

-- | @pad d r == removeFromWidthStable d (removeFromHeightStable d r)@.
pad :: Double -> Rect -> Rect
pad p = over originL (over xL (+p) . over yL (+p)) .
        over sizeL (over widthL f . over heightL f) where f x = x - p * 2

splitHorizontallyN, splitVerticallyN :: Int -> Rect -> [Rect]
splitHorizontallyN n r =
  [ Rect (addToY (hPiece * fromIntegral x) p) (heightL .~ hPiece $ size r)
  | x <- [0 .. (n - 1)] ]
  where hPiece = r ^.sizeL.heightL / fromIntegral n ; p = origin r
splitVerticallyN n r =
  [ Rect (addToX (wPiece * fromIntegral x) p) (widthL .~ wPiece $ size r)
  | x <- [0 .. (n - 1)] ]
  where wPiece = (width . size $ r) / (fromIntegral n) ; p = origin r

splitHorizontally, splitVertically :: Double -> Rect -> (Rect, Rect)
splitHorizontally ratio r =
  (Rect p (over heightL (* ratio) sz) ,
   Rect (addToY halfH p) (over heightL (* (1 - ratio)) sz))
  where halfH = (height . size $ r) * ratio ; p = origin r ; sz = size r
splitVertically ratio r =
  (Rect p (over widthL (* ratio) sz) ,
   Rect (addToX halfW p) (over widthL (* (1 - ratio)) sz))
  where halfW = (width . size $ r) * ratio ; p = origin r ; sz = size r

data XRelativePos = RightOf | LeftOf deriving Show
data YRelativePos = AboveOf | BelowOf deriving Show

data RelativePos = RPos
  { xAxisRPos :: XRelativePos
  , yAxisRPos :: YRelativePos
  } deriving Show

-- | Center of the rectangle.
center :: Rect -> Point
center r =
  Point ((xCoord . origin $ r) + ((width  . size $ r) / 2))
        ((yCoord . origin $ r) + ((height . size $ r) / 2))

dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) =
  sqrt (((x2 - x1) ^ (2 :: Int)) + ((y2 - y1) ^ (2 :: Int)))

-- | Distance between two rectangles (i.e., the length of the segment from one
-- rectangle's center to the other's).
rectDist :: Rect -> Rect -> Double
rectDist r1 r2 = dist (center r2) (center r1)

rposWrtX :: Point -> Point -> XRelativePos
rposWrtX p q = if (xCoord p - xCoord q) >= 0 then RightOf else LeftOf

rposWrtY :: Point -> Point -> YRelativePos
rposWrtY p q = if (yCoord p - yCoord q) >= 0 then BelowOf else AboveOf

rposWrt :: Point -> Point -> RelativePos
rposWrt p q = RPos (rposWrtX p q) (rposWrtY p q)
