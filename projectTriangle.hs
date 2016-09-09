import qualified Data.Vector as V

data Point    = Point{getX :: Double, getY :: Double, getZ :: Double} deriving(Show, Eq)
data Triangle = Triangle{getPoint0 :: Point, getPoint1 :: Point, getPoint2 :: Point} deriving(Show, Eq)
 
type Colour   = Double
data Pixel    = Pixel{getXp :: Double, getYp :: Double, getColour :: Colour} deriving(Show, Eq)
type Vec3     = Point
data Vec2     = Vec2{getVec2X :: Double, getVec2Y :: Double}
type Position = Point

--Projection from the 3D-Space on the Canvas
projectTriangle :: Triangle -> Triangle
projectTriangle triangle = Triangle{ getPoint0 = projectPoint' . getPoint0 $ triangle
									,getPoint1 = projectPoint' . getPoint1 $ triangle
									,getPoint2 = projectPoint' . getPoint2 $ triangle
								   }
 
projectPoint' :: Point -> Point
projectPoint' point = projectPoint 3.4 point


projectPoint :: Double -> Point -> Point
projectPoint near p = let z = getZ p
					  in Point{ getX = near * getX p / z
							   ,getY = near * getY p / z
							   ,getZ = z
							}

projectTriangles :: [Triangle]->[Triangle]
projectTriangles triangles = map projectTriangle triangles

cutOfZ :: Vec3->Vec2
cutOfZ v = Vec2{getVec2X = getX v, getVec2Y = getY v}

--perspectiveProject :: Vec -> Point -> Vec
{-
pixelPainter :: Pixel-> Vec-> Vec-> Vec->Point->Point->Point-> [Pixel]
pixelPainter p v0 v1 v2 p0 p1 p2 = 

getEdge :: Point ->Point-> Vec
getEdge p0 p1 = Point{getX = getX p1 - getX p0, getY = getY p1 - getY p0, getZ = z1 - z0}
map projectTriangle (x:xs)
rasteriser :: [Triangle]->[Pixel]-> Position ->[Pixel]
rasteriser (x:xs) pixel pos = 

							let v0 = perspectiveProject $ getEdge ((getPoint0 x) (getPoint1 x)) pos 
							      v1 = perspectiveProject $ getEdge ((getPoint1 x) (getPoint2 x)) pos
							   	  v2 = perspectiveProject $ getEdge ((getPoint2 x) (getPoint0 x)) pos
							   in rasteriser $ xs pixelPainter ( pixel v0 v1 v2 ) pos
-}							    
 