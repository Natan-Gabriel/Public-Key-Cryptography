{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All
import System.Random
import Data.Time


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



pollard n b=do
    r <- randomRIO(0,10)
    -- print r

    -- print $ pollard n b x
    print  "input a valid value for the bound,or input NO if you want to use the default bound,that is 17" 
    inputBound <- getLine 

    let bound = if(inputBound=="NO" || inputBound=="no")
        then 17
        else (read inputBound :: Integer)
    -- print $ pollard n bound x --euclidean b (euclidean n x)
    -- print bound
    print $ pollardFunction n bound r



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


test_pollard n b a result= (pollardFunction n b a )==result

testCCC_pollard n b result=do
    r <- randomRIO(0,10)
    -- print r

    -- print $ pollard n b x
    -- print  "input a valid value for the bound,or input NO if you want to use the default bound,that is 17" 
    -- inputBound <- getLine 

    -- let bound = if(inputBound=="NO" || inputBound=="no")
    --     then 17
    --     else (read inputBound :: Integer)
    -- print $ pollard n bound x --euclidean b (euclidean n x)
    -- print bound
    -- if(print $ pollard n bound x)
    --     then 1
    -- print $ ( test_pollard (2^7-1) 17 r 0 )
    -- let a= test_pollard (2^7-1) 17 r 0
    -- print a
    return $ test_pollard n b r result
    -- return a
    -- print $ (pollardFunction n 17 r)==result


prop_200=test_pollard (2^7-1) 17 9 0
prop_201=test_pollard (2^13-1) 17 17 0
prop_202=test_pollard (2^17-1) 17 53 0
prop_203=test_pollard (2^19-1) 17 61 0
prop_204=test_pollard (2^31-1) 17 63 0

-- prop_250=testCCC_pollard (2^31-1) 71 0 
-- prop_251=testCCC_pollard (2^61-1) 73 0 


return []





mainTest = $(quickCheckAll)


-- main = quickCheck rsmeTest