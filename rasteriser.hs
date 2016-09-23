
type Colour   = Double


interpolate :: ZBuffer -> Triangle -> ([Pixels],ZBuffer)

edgeFunction :: Vec-> Vec-> Point-> Double
edgeFunction vec1 vec2 p = (getX p - getX vec1) * (getY vec2 - getY vec1)  - (getY p - getY vec1) * (getX vec2 - getX vec1)
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
 