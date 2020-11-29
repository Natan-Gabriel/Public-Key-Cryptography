{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All
import System.Random
import Data.Bits

generateBinary l n=
    if (n==0 && (length l)==0)
        then [0]
    else
        if (n==0)
            then l
        else (generateBinary ( [(mod n 2)] ++ l ) (quot n 2))
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
rsme b k n (ht:tt)= do
    if (n==0)
        then 0
    else do
        let a = 1
        if (k==0)
            then a
            else do
                let c=b
                let aa = if (ht==1)
                    then b
                    else a
                if (length(tt)==0)
                    then aa
                else (forLoopRsme aa c n k tt)
forLoopRsme :: Integer->Integer->Integer->Integer->[Integer]->Integer
forLoopRsme a c n k (ht:tt) = do 
    let cc =(mod (c*c) n)
    
    let aa = if (ht==1)
            then
                (mod (cc*a) n)
            else 
                a
    if ( (length tt)==0 ) 
        then aa
    else (forLoopRsme aa cc n k tt)
-- rsmeWrapper::Integer -> Integer -> Integer -> Integer 
rsmeWrapper b k n=rsme b k n (reverseList (generateBinary [] k))
euclidean::Integer -> Integer -> Integer
euclidean a b =
    if (b>0)
        then (euclidean b (mod a b) )
        else a
computeLCMFor2Numbers a b= (quot (a*b) (euclidean a b) )
computeLCMForAList (x:xs) result=
    if ((length xs)==0)
        then (computeLCMFor2Numbers x result)
    else (computeLCMForAList xs (computeLCMFor2Numbers x result) )
computeLCMForAListWrapper l=
    if ((length l)==0)
        then 1
    else (computeLCMForAList l 1)
pollardFunction n b a = do
    let k=computeLCMForAListWrapper [x | x <- [1..b]]
    let aa=(rsmeWrapper a k n)
    let d= euclidean (aa-1) n
    if (d==1 || d==n)
        then  0
    else   d
pollard n b=do
    r <- randomRIO(2,n-2)
    let a=pollardFunction n b r
    if(a==0)
        then (pollard n b)
        else (return a)
pollard_wrapper n=do
    print  "input a valid value for the bound,or input NO if you want to use the default bound,that is 17" 
    inputBound <- getLine 

    let bound = if(inputBound=="NO" || inputBound=="no")
        then 17
        else (read inputBound :: Integer)
    (pollard n bound)
pollardWithIterations n b iterations=do
    r <- randomRIO(2,n-2)
    let a=pollardFunction n b r
    if((a==0) && (iterations>0))
        then (pollardWithIterations n b (iterations-1))
        else (return a)
pollard_with_iterations_wrapper n iterations=do
    print  "input a valid value for the bound,or input NO if you want to use the default bound,that is 17" 
    inputBound <- getLine 

    let bound = if(inputBound=="NO" || inputBound=="no")
        then 17
        else (read inputBound :: Integer)
    (pollardWithIterations n bound iterations)
-- rsmeTest :: Int -> Int -> Int -> Bool
rsmeTest b k n = (rsmeWrapper b k n) == (mod (b^k) n)


prop_1=rsmeTest 15 12 37
prop_2=rsmeTest 22 11 45
prop_3=rsmeTest 12 87 34
prop_4=rsmeTest 3 16 7
prop_5=rsmeTest 9034 623 11
prop_6=rsmeTest (5^7) (2^7) 13
prop_7=rsmeTest ((2^7)*(11^3)) (3^3) 74
prop_8=rsmeTest ((11^7)*(17^4)) (23^2) 153



test_lcm a b c =(computeLCMFor2Numbers a b)==c
test_lcmForList l c =(computeLCMForAListWrapper l)==c

prop_101=test_lcm 8 12 (lcm 8 12)
prop_102=test_lcm 264 165 (lcm 264 165)
prop_103=test_lcm 2451 4253 (lcm 2451 4253)
prop_104=test_lcm 75364 254634 (lcm 75364 254634)
prop_105=test_lcm 9645124 5462343 (lcm 9645124 5462343)

prop_151=test_lcmForList [165,205,310] (lcm 165 (lcm 205 310))
prop_152=test_lcmForList [132,162,90] (lcm 132 (lcm 162 90))
prop_153=test_lcmForList [342547,44253,1423] (lcm 342547 (lcm 44253 1423))
prop_154=test_lcmForList [41243499,775353,5464657,875624] (lcm 41243499 (lcm 775353 (lcm 5464657 875624)))
prop_155=test_lcmForList [((2^3)*(5^7)*(7^2)),((2^9)*(5^3)*(7^3)),((5^7)*(7^10)*(9^2)),((7^2)*(11^2))] (lcm ((2^3)*(5^7)*(7^2)) (lcm ((2^9)*(5^3)*(7^3)) (lcm ((5^7)*(7^10)*(9^2)) ((7^2)*(11^2)))))


return []
test_pollard n b=do
    r <- randomRIO(2,n-2)
    let a=pollardFunction n b r
    if(a==0)
        then (test_pollard n b)
        else (print (a>0))


test_pollard_with_iterations n b iterations=do
    r <- randomRIO(2,n-2)
    let a=pollardFunction n b r
    if((a==0) && (iterations>0))
        then (test_pollard_with_iterations n b (iterations-1))
        else (print (a==0))

main=do
    $quickCheckAll

    print "(test_pollard 15 5) evaluates as:" 
    (test_pollard 15 5)
    print "(test_pollard (7*13) 8) evaluates as:" 
    (test_pollard (7*13) 11)
    print "(test_pollard ((2^5)*(3^3)) 3) evaluates as:" 
    (test_pollard ((3^5)*(5^3))  3)
    print "(test_pollard ((2^6)*(3^7)*(5^5)) 5) evaluates as:" 
    (test_pollard ((3^6)*(7^7)*(5^5)) 5)
    print "(test_pollard ((3^7)*(5^5)*(7^3)) 7) evaluates as:" 
    (test_pollard ((3^7)*(5^5)*(7^3)) 7)
    print "(test_pollard ((11^3)*(13^5)) 13 ) evaluates as:" 
    (test_pollard ((11^3)*(13^5)) 13 )
    print "(test_pollard ((2^4)*11*(17^5)) 17 ) evaluates as:" 
    (test_pollard ((7^4)*11*(17^5)) 17 )

    -- --here I use numbers of the form 2^n-1 which are not Mersenne numbers
    -- --we will test that these nubmers CAN be decomposed

    print "(test_pollard (2^15-1) 13 ) evaluates as:" 
    (test_pollard (2^15-1) 13 )
    print "(test_pollard (2^18-1) 17 ) evaluates as:" 
    (test_pollard (2^18-1) 17 )
    print "(test_pollard (2^22-1) 17 ) evaluates as:" 
    (test_pollard (2^22-1) 17 )
    print "(test_pollard (2^25-1) 17 ) evaluates as:" 
    (test_pollard (2^25-1) 17 )
    print "(test_pollard (2^50-1) 17 ) evaluates as:" 
    (test_pollard (2^50-1) 17 )
    print "(test_pollard (2^60-1) 17 ) evaluates as:" 
    (test_pollard (2^60-1) 17 )
    print "(test_pollard (2^70-1) 17 ) evaluates as:" 
    (test_pollard (2^70-1) 17 )
    print "(test_pollard (2^100-1) 23 ) evaluates as:" 
    (test_pollard (2^100-1) 23 )
    print "(test_pollard (2^1231-1) 23 ) evaluates as:" 
    (test_pollard (2^1231-1) 23 )
    print "(test_pollard (2^4250-1) 23 ) evaluates as:" 
    (test_pollard (2^4250-1) 23 )
    print "(test_pollard (2^85329-1) 29 ) evaluates as:" 
    (test_pollard (2^85329-1) 29 )
    print "(test_pollard (2^133562-1) 29 ) evaluates as:" 
    (test_pollard (2^133562-1) 29 )

    -- --here I use numbers of the form 2^n-1 which are not Mersenne numbers,but 2^(n+1)-1 are Mersenne numbers
    -- --we will test that these nubmers CAN be decomposed


    print "(test_pollard (2^6-1) 11 ) evaluates as:" 
    (test_pollard (2^6-1) 11 )
    print "(test_pollard (2^12-1) 11 ) evaluates as:" 
    (test_pollard (2^12-1) 11 )
    print "(test_pollard (2^16-1) 11 ) evaluates as:" 
    (test_pollard (2^16-1) 11)
    print "(test_pollard (2^30-1) 13 ) evaluates as:" 
    (test_pollard (2^30-1) 13 )
    print "(test_pollard (2^60-1) 17 ) evaluates as:" 
    (test_pollard (2^60-1) 17 )
    print "(test_pollard (2^126-1) 19) evaluates as:" 
    (test_pollard (2^126-1) 19 )
    print "(test_pollard (2^520-1) 29) evaluates as:" 
    (test_pollard (2^520-1) 29)
    print "(test_pollard (2^2202-1) 31) evaluates as:" 
    (test_pollard (2^2202-1) 31)
    print "(test_pollard (2^21700-1) 31) evaluates as:" 
    (test_pollard (2^21700-1) 31)
    print "(test_pollard (2^110502-1) 31) evaluates as:" 
    (test_pollard (2^110502-1) 31)
    print "(test_pollard (2^216090-1) 37) evaluates as:" 
    (test_pollard (2^216090-1) 37)

    --here I use numbers of the form 2^n-1 which are Mersenne numbers
    --here we will test that these nubmers CAN NOT be decomposed in a number of given iterations

    print "(test_pollard_with_iterations (2^7-1) 11 10) evaluates as:" 
    (test_pollard_with_iterations (2^7-1) 11 10)
    print "(test_pollard_with_iterations (2^13-1) 11 10) evaluates as:" 
    (test_pollard_with_iterations (2^13-1) 11 10)
    print "(test_pollard_with_iterations (2^17-1) 11 10) evaluates as:" 
    (test_pollard_with_iterations (2^17-1) 11 10)
    print "(test_pollard_with_iterations (2^31-1) 13 10) evaluates as:" 
    (test_pollard_with_iterations (2^31-1) 13 10)
    print "(test_pollard_with_iterations (2^61-1) 17 10) evaluates as:" 
    (test_pollard_with_iterations (2^61-1) 17 10)
    print "(test_pollard_with_iterations (2^127-1) 19 20) evaluates as:" 
    (test_pollard_with_iterations (2^127-1) 19 20)
    print "(test_pollard_with_iterations (2^521-1) 29 20) evaluates as:" 
    (test_pollard_with_iterations (2^521-1) 29 20)
    print "(test_pollard_with_iterations (2^2203-1) 31 20) evaluates as:" 
    (test_pollard_with_iterations (2^2203-1) 31 20)
    print "(test_pollard_with_iterations (2^21701-1) 31 30) evaluates as:" 
    (test_pollard_with_iterations (2^21701-1) 31 30)
    print "(test_pollard_with_iterations (2^110503-1) 31 40) evaluates as:" 
    (test_pollard_with_iterations (2^110503-1) 31 40)
    print "(test_pollard_with_iterations (2^216091-1) 37 40) evaluates as:" 
    (test_pollard_with_iterations (2^216091-1) 37 40)


