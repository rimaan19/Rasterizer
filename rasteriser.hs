import System.IO

main = do
	writeFile "frame.ppm" ("P3\n1280\n720\n255" ++ rasteriser (listOfTriangles))
	  where 
	  	listOfTriangles = 


