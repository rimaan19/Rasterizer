import qualified Data.Vector as V

data Point    = Point{ getX :: !Double
                     , getY :: !Double
                     , getZ :: !Double
                     } deriving(Show, Eq)

type Colour   = Double
data Vertex   = Vertex{ getPoint  :: !Point
                      , getColour :: !Colour
                      } deriving(Show, Eq)

data Triangle = Triangle{ getVertex1 :: !Vertex
                        , getVertex2 :: !Vertex
                        , getVertex3 :: !Vertex
                        } deriving(Show, Eq)
 

type Pixel    = Vertex
type Vec3     = Point
data Vec2     = Vec2{getVec2X :: Double, getVec2Y :: Double}
type Position = Point


 
projectPoint' :: Point -> Point
projectPoint' point = projectPoint 3.4 point


projectPoint :: Double -> Point -> Point
projectPoint near p = let z = getZ p
                      in Point{ getX = near * getX p / z
                              , getY = near * getY p / z
                              , getZ = z
                              }

projectTriangles :: [Triangle] -> [Triangle]
projectTriangles triangles = map projectTriangle triangles
  where
    --Projection from the 3D-Space on the Canvas
    projectTriangle :: Triangle -> Triangle
    projectTriangle triangle = Triangle{ getVertex1 = projectPoint' . getVertex1 $ triangle
                                        ,getVertex2 = projectPoint' . getVertex2 $ triangle
                                        ,getVertex3 = projectPoint' . getVertex3 $ triangle
                                        }

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
 