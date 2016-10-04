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

type Position = Point

defaultPoint :: Point
defaultPoint = Point{ getX = 1.0
                    , getY = 1.0
                    , getZ = 1.0
                    }
--Takes the projectors px and py which will be applied to the Points of the Vertizes of the provided 
--Triangles
projectTriangle :: (Double->Double->Double->Double)->(Double->Double->Double->Double)
                    ->(Double->Double->Double->Double)-> Triangle ->Triangle
projectTriangle px py pz triangle = Triangle{getVertex1 = (projectVertex px py pz) . getVertex1 $ triangle
                                            ,getVertex2 = (projectVertex px py pz) . getVertex2 $ triangle
                                            ,getVertex3 = (projectVertex px py pz) . getVertex3 $ triangle
                                            }
  where
    projectVertex ::  (Double->Double->Double->Double)->(Double->Double->Double->Double)
                      -> (Double->Double->Double->Double)->Vertex ->Vertex
    projectVertex px py pz vertex = let point = getPoint $ vertex
                                        x     = getX point
                                        y     = getY point
                                        z     = getZ point
                                    in  Vertex{getPoint = Point{ getX = px (x) (y) (z)
                                                                ,getY = py (x) (y) (z)
                                                                ,getZ = pz (x) (y) (z)
                                                                }
                                                ,getColour = getColour $ vertex               
                                                }


projectTrianglesToCameraSpace :: [Triangle] -> [Triangle]
projectTrianglesToCameraSpace triangles = map projectTriangleToCameraSpace triangles
  where
    --Projection from the 3D-Space on the Canvas
    projectTriangleToCameraSpace :: Triangle -> Triangle
    projectTriangleToCameraSpace triangle = projectTriangle px py pz triangle
    px x _ z = 3.4 * x / z 
    py _ y z = 3.4 * y / z
    pz _ _ z = - z

projectTrianglesToNDCSpace ::  Double->Double ->[Triangle]->[Triangle]
projectTrianglesToNDCSpace width hight triangles = map (projectTriangleToNDCSpace width hight) triangles 
  where
    --Projection from the Canvas on the NDC-Space
    projectTriangleToNDCSpace :: Double->Double->Triangle-> Triangle
    projectTriangleToNDCSpace width hight triangle = let px x _ z = (x + width/2) / width
                                                         py _ y z = (y + hight/2)/hight
                                                         pz _ _ z = z
                                                     in projectTriangle px py pz triangle


--projectTrianglesToRasterSpace :: [Triangle]->[Pixel]

--I should abstract these Projections a bit away first order function for the win!!!
-- projector :: Triangle -> (Double -> Double)->(Double -> Double)->(Double -> Double)->Triangle 
--vielleicht mehr als eine?

projectTrianglesToRasterSpace :: Double->Double-> [Triangle]-> [Triangle]
projectTrianglesToRasterSpace imageWidth imageHeight triangles = map (projectTriangleToRasterSpace imageWidth imageHeight) triangles
  where 
    projectTriangleToRasterSpace :: Double->Double->Triangle->Triangle
    projectTriangleToRasterSpace imageWidth imageHeight triangle = let px x _ _ = (x + 1 ) / (2 * imageWidth)
                                                                      --in rasterspace y is down so invert direction
                                                                       py _ y _ = (1 - y)/(imageHeight * 2)
                                                                       pz _ _ z = - z
                                                                   in projectTriangle px py pz triangle
