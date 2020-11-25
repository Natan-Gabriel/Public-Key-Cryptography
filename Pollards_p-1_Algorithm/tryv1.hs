{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All
import Numeric.Natural
import System.Random
import System.Random.MWC as MWC
import System.Random.Stateful
import System.Random(randomIO)
import Control.Monad (replicateM)
import Data.Time
import Control.Exception


-- import System.Random.MWC as MWC

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

-- pollardFct :: Integer-> Integer->Integer
pollard n b a = do
    let k=computeLCMForAListWrapper [x | x <- [1..b]]
    let aa=(rsmeWrapper a k n)
    let d= euclidean (aa-1) n
    if (d==1 || d==n)
        then  0
    else   d


memestoica n b a =pollard n b a -- do
--   n <- getLine
--   let result = n
--   print result

pollardWrapper n b=do
    x <- randomRIO(7,10)
    print $ pollard n b x
    -- print  "enter value for y: " 
    -- input2 <- getLine 
    -- let x = (read input1 :: Int)
    -- print $ pollard n b x --euclidean b (euclidean n x)

-- mainFunction=do 
--     print "Choose a bound"
--     x <- getLine
--     print 1
    -- print input1
    -- putStrLn "enter value for y: " 
    -- input2 <- getLine 
    -- let x = (read input1 :: Int)
    -- let y = (read input2 :: Int)


mainFunction=do
    print "Choose a bound,the implicit one is 13:"
    x <- getLine
    let x1 = (read x :: Int) 
    

    -- result <- try (let x1 = (read x :: Int)) :: IO (Either SomeException Int)
    -- case result of
    --     Left ex  -> putStrLn $ "Caught exception: " ++ show ex
    --     Right val -> putStrLn $ "The answer was: " ++ show val

    -- catch ( let x1 = (read x :: Int) ) handler
    -- where
    --     handler :: SomeException -> IO ()
    --     handler ex = putStrLn $ "Next time input an integer! " ++ show ex
--     print "GCD of x and y is:"
--     start <- getCurrentTime
    print x1

mainEx = do
    x <- getLine
    result <- try (read x :: Int ) :: IO (Either SomeException Int)
    case result of
        Left ex  -> putStrLn $ "Caught exception: " ++ show ex
        Right val -> putStrLn $ "The answer was: " ++ show val

-- main=do{
--     g<-getStdGen;
--     let [s]=take 1 (randomStuff g)
--     ;print s
-- }
-- randomStuff g= work (randomRs (0.0,1.0) g)

-- work (r:rs)=
--     let n=truncate (r * 7.0)+1










    

-- data Nat =  Int Int deriving Eq

-- instance Arbitrary Nat where
--   arbitrary = do
--     x <- arbitrary
--     return x

-- prop_gen=prop_random 
-- functionf:: Integer-> Integer
-- functionf a = randomRIO (1, 1000) 

-- prop_gg :: Integer-> Bool
-- prop_gg a=  do
--     let rr=randomRIO (1, 1000) 
--     (rop_random rr rr rr)
    

rop_random :: Integer-> Integer -> Integer -> Bool
rop_random b k n = (rsmeWrapper b k n) > (getMod b k n)

-- revapp :: Int -> Int -> Int -> Bool
-- revapp b k n = (rsmeWrapper b k n) == (getMod b k n)


-- simpleMathTests :: TestTree
-- simpleMathTests = testGroup "Simple Math Tests"
--   [ testCase "Small Numbers" .
--       revapp 3 4 5 @?= 1
--   ]

-- elements :: [a] -> Gen a

-- generate=  

-- prop_1=revapp 15 12 37
-- prop_2=revapp 22 11 45
-- prop_3=revapp 12 87 34
-- prop_4=revapp 3 16 7
-- prop_5=revapp 90 6 7
-- prop_6=revapp 34 5 7
-- prop_7=revapp 34 51 74
-- prop_8=revapp 23 45 2
-- prop_9=revapp 0 0 0
-- prop_10=revapp 1 0 0
-- prop_11=revapp 0 1 0
-- -- prop_12=revapp 0 0 1

test_lcm a b c =(computeLCMFor2Numbers a b)==c
test_lcmForList l c =(computeLCMForAListWrapper l)==c

prop_101=test_lcm 8 12 (lcm 8 12)
prop_102=test_lcm 26 165 (lcm 26 165)
prop_103=test_lcm 135 205 (lcm 135 205)

prop_151=test_lcmForList [165,205,310] (lcm 165 (lcm 205 310))
prop_152=test_lcmForList [132,162,90] (lcm 132 (lcm 162 90))
prop_153=test_lcmForList [192,101,7] (lcm 192 (lcm 101 7))
prop_154=test_lcmForList [72,245,90,83] (lcm 72 (lcm 245 (lcm 90 83)))



return []





mainasdfg = $(quickCheckAll)


-- main = quickCheck revapp