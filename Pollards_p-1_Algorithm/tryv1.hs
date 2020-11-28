{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property
import System.Random
import Data.Time
import Data.Bits
import Control.Monad.IO.Class


-- Function generateBinary transforms a decimal number into a binary number.
-- This is the mathematical model:

-- $$
--  generateBinary(l,n)= \begin{cases}  [0] , if\ n=0 and l=[]
--              \\ l , if\ n=0 \\  generateBinary([n\%2] \bigcup l,n/2) ,else  \end{cases}
-- $$

-- ~~~~~{.haskell}
-- <<generateBinary>>=
generateBinary l n=
    if (n==0 && (length l)==0)
        then [0]
    else
        if (n==0)
            then l
        else (generateBinary ( [(mod n 2)] ++ l ) (quot n 2))
-- @
-- ~~~~~

-- Function reverseList reverses a given list.The mathematical model is:

-- $$
--  reverseList(x1,x2,...xn)= \begin{cases} [], if n=0 \\  reverseList(x2,...xn) \bigcup x1 ,else  \end{cases}
-- $$

-- ~~~~~{.haskell}
-- <<reverseList>>=
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
-- @
-- ~~~~~


-- Let define Repeated Squaring Modular Exponentiation (rsme) function:

-- Input: b, k, n in N with b < n and k = $\sum_{i=0}^{t} k_i*2^{i}$

-- Output: a = $b^{k}$ mod n.


-- a=1

-- if k=0 then write(a)

-- c=b

-- if $k_0$ =1 then a=b

-- for i=1 to t do

-- \quad c= $c^{2}$ mod n

-- \quad if $k_i$ =1 then a=c*a mod n

-- write(a)


-- **Proof of corectness for Repeated Squaring Modular Exponentiation:**

-- We have k = $\sum_{i=0}^{t} k_i*2^{i}$

-- if k=0 => we return 1 because any integer to the power 0 is 1.

-- At each step we have $b^{2^{i}}$ mod n,starting from i=1(from right in binary representation).
-- Using $b^{2^{i}}$ mod n, we compute $b^{2^{i+1}}$ mod n = $(b^{2^{i}})^{2}$ mod n = $(b^{2^{i}} mod\ n)^{2}$.
-- If the ${k_i}$ is 1 then we compute :  ( new a= new c* old a mod n ).This holds because
--  $b^{\sum_{i=0}^{t} k_i*2^{i}}$ = $b^{ k_0*2^{0}}$ * $b^{ k_1*2^{1}}$ * ...* $b^{ k_t*2^{t}}$ and then 
--  $b^{k}$ mod n = $b^{\sum_{i=0}^{t} k_i*2^{i}}$ mod n = ($b^{ k_0*2^{0}}$ mod n) * ($b^{ k_1*2^{1}}$ mod n) * ...* ($b^{ k_t*2^{t}}$ mod n) ,
-- where ($b^{ k_i*2^{i}}$ mod n) is our c computed at i-th iteration(for $k_i$=1).




-- ~~~~~{.haskell}
-- <<rsme>>=
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
-- @
-- ~~~~~

-- The forLoopRsme function represents the for loop of rsme function written in a functional style.
-- More precisely,this loop:

-- for i=1 to t do

-- \quad c= $c^{2}$ mod n

-- \quad if $k_i$ =1 then a=c*a mod n

-- ~~~~~{.haskell}
-- <<forLoopRsme>>=
-- forLoopRsme :: Integer->Integer->Integer->Integer->[Integer]->Integer
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
-- @
-- ~~~~~
-- rsmeWrapper::Integer -> Integer -> Integer -> Integer 
rsmeWrapper b k n=rsme b k n (reverseList (generateBinary [] k))



getMod b k n=
    if (n==0)
        then 0
    else
        if (k==0) --this also trats the case 0^0
            then 1
        else (mod (b^k) n)

euclidean::Integer -> Integer -> Integer
euclidean a b =
    if (b>0)
        then (euclidean b (mod a b) )
        else a


-- The above relation only holds for two numbers,
-- The idea here is to extend our relation for more than 2 numbers

computeLCMFor2Numbers a b= (quot (a*b) (euclidean a b) )

computeLCMForAList (x:xs) result=
    if ((length xs)==0)
        then (computeLCMFor2Numbers x result)
    else (computeLCMForAList xs (computeLCMFor2Numbers x result) )

computeLCMForAListWrapper l=
    if ((length l)==0)
        then 1
    else (computeLCMForAList l 1)

-- Pollard's p-1 algorithm is efficiently in finding any prime factor p of an 
--odd composite number for which p-1 has only small prime divisors

-- pollardFunction :: Integer-> Integer->Integer->Integer
pollardFunction n b a = do
    let k=computeLCMForAListWrapper [x | x <- [1..b]]
    let aa=(rsmeWrapper a k n)
    let d= euclidean (aa-1) n
    if (d==1 || d==n)
        then  0
    else   d



-- pollard n=do
--     r <- randomRIO(2,n-2)
--     -- print r

--     -- print $ pollard n b x
--     print  "input a valid value for the bound,or input NO if you want to use the default bound,that is 17" 
--     inputBound <- getLine 

--     let bound = if(inputBound=="NO" || inputBound=="no")
--         then 17
--         else (read inputBound :: Integer)
--     -- print $ pollard n bound x --euclidean b (euclidean n x)
--     -- print bound
--     return $ pollardFunction n bound r


pollard_wrapper n=do
    print  "input a valid value for the bound,or input NO if you want to use the default bound,that is 17" 
    inputBound <- getLine 

    let bound = if(inputBound=="NO" || inputBound=="no")
        then 17
        else (read inputBound :: Integer)
    return $ pollard n bound


pollard n b=do
    r <- randomRIO(2,n-2)
    -- print r
    let a=pollardFunction n b r
    -- print a
    -- print 1
    if(a==0)
        then (pollard n b)
        else (return a)
    -- print 1


pollard_with_iterations_wrapper n iterations=do
    print  "input a valid value for the bound,or input NO if you want to use the default bound,that is 17" 
    inputBound <- getLine 

    let bound = if(inputBound=="NO" || inputBound=="no")
        then 17
        else (read inputBound :: Integer)
    return $ pollardWithIterations n bound iterations

pollardWithIterations n b iterations=do
    r <- randomRIO(2,n-2)
    -- print r
    let a=pollardFunction n b r
    -- print a
    -- print 1
    if((a==0) && (iterations>0))
        then (pollardWithIterations n b (iterations-1))
        else (return a)
-- -- gggg::Integer-> Integer  -> Integer->m0 Integer
-- gggg n b iterations=( pollardWithIterations n b iterations)

-- gggg::Integer-> Integer  -> Integer->m0 Integer
-- gggg n b =(eq ( pollard n b) 1)

-- rop_random :: Integer-> Integer -> Integer -> Bool
-- rop_random b k n = (rsmeWrapper b k n) == (getMod b k n)

-- rsmeTest :: Int -> Int -> Int -> Bool
rsmeTest b k n = (rsmeWrapper b k n) == (getMod b k n)

prop_1=rsmeTest 15 12 37
prop_2=rsmeTest 22 11 45
prop_3=rsmeTest 12 87 34
prop_4=rsmeTest 3 16 7
prop_5=rsmeTest 90 6 7
prop_6=rsmeTest 34 5 7
prop_7=rsmeTest 34 51 74
prop_8=rsmeTest 23 45 2



test_lcm a b c =(computeLCMFor2Numbers a b)==c
test_lcmForList l c =(computeLCMForAListWrapper l)==c

prop_101=test_lcm 8 12 (lcm 8 12)
prop_102=test_lcm 26 165 (lcm 26 165)
prop_103=test_lcm 135 205 (lcm 135 205)

prop_151=test_lcmForList [165,205,310] (lcm 165 (lcm 205 310))
prop_152=test_lcmForList [132,162,90] (lcm 132 (lcm 162 90))
prop_153=test_lcmForList [192,101,7] (lcm 192 (lcm 101 7))
prop_154=test_lcmForList [72,245,90,83] (lcm 72 (lcm 245 (lcm 90 83)))

-- poateamu :: IO ()
-- poateamu = do
--   a <- getStdRandom $ randomR (0, 1 :: Int)
--   b <- (pollard 7 8) 
--   if a == b then (putStrLn "user") else (putStrLn "user")


-- uuu n b res=do
--     r <- randomRIO(2,n-2)
--     -- print r
--     let a=pollardFunction n b r
--     -- print a
--     -- print 1
--     let variable1=if(a==0)
--         then (pollard n b)
--         else (res==a)
--     return variable1


rezerva n b res=do
    r <- randomRIO(2,n-2)
    -- print r
    let a=pollardFunction n b r
    -- print a
    -- print 1
    if(a==0)
        then (rezerva n b res)
        else (print (a>res))







-- length <$> getLine

-- test_pollard n b =do
--     a<-(pollard n b)
--     c<-0
--     if a == c then (return a) else (return a)


test_pollard n b=do
    r <- randomRIO(2,n-2)
    -- print r
    let a=pollardFunction n b r
    -- print a
    -- print 1
    if(a==0)
        then (test_pollard n b)
        else (print (a>0))


test_pollard_with_iterations n b iterations=do
    r <- randomRIO(2,n-2)
    -- print r
    let a=pollardFunction n b r
    -- print a
    -- print 1
    if((a==0) && (iterations>0))
        then (test_pollard_with_iterations n b (iterations-1))
        else (print (a==0))

    -- if((a==0))
    --     then do
    --         if((iterations>0))
    --             then (test_pollard_with_iterations n b (iterations-1))
    --             else 0
    --     else (return a)

-- test_pollard::Integer->Integer->monadIO mo
-- test_pollard n b = do
--     r <- randomRIO(2,n-2)
--     return (pollard n r) -- (pollard n b) > 1 
-- test_pollard n b = print ( (pollard n b) == 1 )
-- test_pollard_with_iterations::Integer ->Integer ->Integer ->Integer ->Bool
-- test_pollard_with_iterations n b iterations res= (gggg n b iterations) == res

-- prop_factor n b iterations res= 
--     assert ((gggg n b iterations) == res)


-- testFilesEqual = TestCase (do x <- readFile "a.txt"
--                               y <- readFile "b.txt"
--                               assertEqual "files not equal" x y)


-- prop_200=test_pollard_with_iterations (2^7-1) 17 10 1

-- pollard_with_iterations

-- rezerva1 n b res=do
--     r <- randomRIO(2,n-2)
--     -- print r
--     let a=pollardFunction n b r
--     -- print a
--     -- print 1
--     if(a==0)
--         then (rezerva1 n b res)
--         else (return (a>res))
    -- print aa
-- prop301=(rezerva1 10 15 0)
-- prop302=assert (rezerva1 10 15 0) True
-- prop303=assert "True is True!" True

-- prop_200=test_pollard_with_iterations (2^7-1) 17 10 1
-- prop_201=test_pollard_with_iterations (2^13-1) 17 10 0
-- prop_202=test_pollard_with_iterations (2^17-1) 17 10 0
-- prop_203=test_pollard_with_iterations (2^19-1) 17 20 0
-- prop_204=test_pollard_with_iterations (2^31-1) 17 20 0
-- prop_205=test_pollard_with_iterations (2^61-1) 17 20 0
-- prop_206=test_pollard_with_iterations (2^89-1) 17 20 0

-- prop_250=testCCC_pollard (2^31-1) 71 0 
-- prop_251=testCCC_pollard (2^61-1) 73 0 




-- mainTest = quickCheck $ prop_factor (2^7-1) 17 10 1 -- $(quickCheckAll)

-- main = quickCheck $ assert True True --prop303
-- main = $(quickCheckAll)

main=do
    -- print "(test_pollard 15 5) evaluates as:" 
    -- (test_pollard 15 5)
    -- print "(test_pollard (7*13) 8) evaluates as:" 
    -- (test_pollard (7*13) 8)
    -- print "(test_pollard ((2^5)*(3^3)) 3) evaluates as:" 
    -- (test_pollard ((2^5)*(3^3))  3)
    -- print "(test_pollard ((2^6)*(3^7)*(5^5)) 5) evaluates as:" 
    -- (test_pollard ((2^6)*(3^7)*(5^5)) 5)
    -- print "(test_pollard ((3^7)*(5^5)*(7^3)) 7) evaluates as:" 
    -- (test_pollard ((3^7)*(5^5)*(7^3)) 7)
    -- print "(test_pollard ((11^3)*(13^5)) 13 ) evaluates as:" 
    -- (test_pollard ((11^3)*(13^5)) 13 )
    -- print "(test_pollard ((2^4)*11*(17^5)) 17 ) evaluates as:" 
    -- (test_pollard ((2^4)*11*(17^5)) 17 )

    -- --here I use numbers of the form 2^n-1 which are not Mersenne numbers
    -- --we will test that these nubmers CAN be decomposed

    -- print "(test_pollard (2^15-1) 13 ) evaluates as:" 
    -- (test_pollard (2^15-1) 13 )
    -- print "(test_pollard (2^18-1) 17 ) evaluates as:" 
    -- (test_pollard (2^18-1) 17 )
    -- print "(test_pollard (2^22-1) 17 ) evaluates as:" 
    -- (test_pollard (2^22-1) 17 )
    -- print "(test_pollard (2^25-1) 17 ) evaluates as:" 
    -- (test_pollard (2^25-1) 17 )
    -- print "(test_pollard (2^50-1) 17 ) evaluates as:" 
    -- (test_pollard (2^50-1) 17 )
    -- print "(test_pollard (2^60-1) 17 ) evaluates as:" 
    -- (test_pollard (2^60-1) 17 )
    -- print "(test_pollard (2^70-1) 17 ) evaluates as:" 
    -- (test_pollard (2^70-1) 17 )
    -- print "(test_pollard (2^100-1) 17 ) evaluates as:" 
    -- (test_pollard (2^100-1) 17 )
    -- print "(test_pollard (2^1231-1) 17 ) evaluates as:" 
    -- (test_pollard (2^1231-1) 17 )
    -- print "(test_pollard (2^4250-1) 17 ) evaluates as:" 
    -- (test_pollard (2^4250-1) 17 )
    -- print "(test_pollard (2^85329-1) 17 ) evaluates as:" 
    -- (test_pollard (2^85329-1) 17 )
    -- print "(test_pollard (2^133562-1) 17 ) evaluates as:" 
    -- (test_pollard (2^133562-1) 17 )
    -- print "(test_pollard (2^859301-1) 17 ) evaluates as:" 
    -- (test_pollard (2^859301-1) 17 )
    -- print "(test_pollard (2^1000025-1) 17 ) evaluates as:" 
    -- (test_pollard (2^1000025-1) 17 )

    --here I use numbers of the form 2^n-1 which are not Mersenne numbers
    --here we will test that these nubmers CAN NOT be decomposed in a number of given iterations

    print "(test_pollard_with_iterations (2^7-1) 10 10) evaluates as:" 
    (test_pollard_with_iterations (2^7-1) 10 10)
    print "(test_pollard_with_iterations (2^13-1) 10 10) evaluates as:" 
    (test_pollard_with_iterations (2^13-1) 10 10)
    print "(test_pollard_with_iterations (2^17-1) 10 10) evaluates as:" 
    (test_pollard_with_iterations (2^17-1) 10 10)
    print "(test_pollard_with_iterations (2^31-1) 10 10) evaluates as:" 
    (test_pollard_with_iterations (2^31-1) 10 10)
    print "(test_pollard_with_iterations (2^61-1) 10 10) evaluates as:" 
    (test_pollard_with_iterations (2^61-1) 10 10)
    print "(test_pollard_with_iterations (2^127-1) 10 10) evaluates as:" 
    (test_pollard_with_iterations (2^127-1) 10 10)
    print "(test_pollard_with_iterations (2^521-1) 10 10) evaluates as:" 
    (test_pollard_with_iterations (2^521-1) 10 10)
    print "(test_pollard_with_iterations (2^2203-1) 10 10) evaluates as:" 
    (test_pollard_with_iterations (2^2203-1) 10 10)
    print "(test_pollard_with_iterations (2^21701-1) 10 10) evaluates as:" 
    (test_pollard_with_iterations (2^21701-1) 10 30)
    print "(test_pollard_with_iterations (2^110503-1) 10 10) evaluates as:" 
    (test_pollard_with_iterations (2^110503-1) 10 30)
    print "(test_pollard_with_iterations (2^216091-1) 10 10) evaluates as:" 
    (test_pollard_with_iterations (2^216091-1) 10 30)

