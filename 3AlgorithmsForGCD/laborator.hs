
import Data.Time
import Data.Bits

dijkstraImproved=do
	print "x="
	x <- getLine
	print "y="
	y <- getLine
	print "GCD of x and y is:"
	start <- getCurrentTime
	print (intListToString (reverseL (dijkstraImprovedAlgorithm (reverseL (inputToIntList x)) (reverseL (inputToIntList y)))))
	stop <- getCurrentTime
	print $ diffUTCTime stop start

intListToString (x:rest) = (show x) ++ (intListToString rest)
intListToString rest = []

stringToStringList (x:rest) = [x]:(stringToStringList rest)
stringToStringList rest = []

stringToInt a=read a::Int

stringListToIntList (x:rest) = (stringToInt x):(stringListToIntList rest)
stringListToIntList rest = []

inputToIntList x= stringListToIntList (stringToStringList (x))


-- dijkstraImproved x y = dijkstraImprovedAlgorithm (inputToIntList x) (inputToIntList y)



-- Dijkstra algorithm for large numbers
dijkstraImprovedAlgorithm a b =
	if (a==b)
		then a
	else if ((compareLists (reverseL a) (reverseL b))==(reverseL a))
		then (dijkstraImprovedAlgorithm (substractListsAndEliminate0 a b 0) b)
	else (dijkstraImprovedAlgorithm a (substractListsAndEliminate0 b a 0) )


-- substractLists([x:xres],[y:yres],r)=x-y-r : substract([xres],[yres],0),if x-y-r>=0
							-- =x-y-r +10 : substract([xres],[yres],1),if x-y-r<0
							-- =x-y-r=,if xres= yres =[]
							-- =x-y-r : [xres],if x-y-r>=0 and yres =[]
							-- =x-y +10 : substract([xres],[0],1),if x-y-r<0 and yres =[]

substractListsAndEliminate0 x y r =reverseL (eliminate0InFrontOfNumber (reverseL (substractLists x y r)))

eliminate0InFrontOfNumber (x:resx)=
	if ( x==0)
		then (eliminate0InFrontOfNumber resx)
	else (x:resx)

substractLists :: [Int] -> [Int] -> Int -> [Int]
substractLists (x:resx) (y:resy) r =
	if ( (length resx)==0 && (length resy)==0 && x-y-r==0)
		then []
	else if ( (length resx)==0 && (length resy)==0)
		then [x-y-r]
	else if (  x-y-r>=0 && (length resy)==0)
		then (x-y-r):resx
	else if (  x-y-r<0 && (length resy)==0)
		then (x-y-r+10):(substractLists resx [0] 1)
	else if ( x-y-r>=0)
		then (x-y-r):(substractLists resx resy 0)
	else if ( x-y-r<0)
		then (x-y-r+10):(substractLists resx resy 1)

	else [-1]




compareEqualLists (x:resx) (y:resy)=
	if ( x> y)
		then (x:resx)
	else if ( x < y)
		then (y:resy)
	else [x] ++ compareEqualLists resx resy

compareLists x y=
	if (( length x)>(length y))
		then x
	else
		if (( length x)<(length y))
		then y
	else
		(compareEqualLists x y)

reverseInputStrings =do
	print "x="
	x <- getLine
	print(reverseL x)

reverseL [] = []
reverseL (x:xs) = reverseL xs ++ [x]


--Euclidean algorithm
euclidean a b =
    if (b>0)
        then (euclidean b (mod a b) )
        else a

-- --Dijkstra algorithm
-- dijkstra a b =
-- 	if (a==b)
-- 		then a
-- 	else if (a>b)
-- 		then (dijkstra (a-b) b)
-- 	else (dijkstra a (b-a) )


andBitwise :: Int -> Int -> Int
andBitwise a b = a .&. b


--Binary algorithm (Stein's algorithm)
binary a b =
	if (a==b)
		then a

	else
		if (a== 0)
		then b

	else
		if ( (andBitwise (complement a) 1)==1 ) then
			(if ( (andBitwise b 1)==1 )
			then (binary (shiftR a 1) b)
			else
				( shiftL (binary (shiftR a 1) (shiftR b 1)) 1 )  )

	else
		if ( (andBitwise (complement b) 1)==1 )
		then (binary a (shiftR b 1))

	else
		if (a>b)
		then (binary (a-b) b )
	else
		(binary (b-a) a)


f x y = x + y