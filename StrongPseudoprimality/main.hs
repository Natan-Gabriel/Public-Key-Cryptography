{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All

getBasesWrapper n=[1]++[n-1]++(getBases n  ([x | x <- [2..((quot n 2)+1)]]) )
getBases n (hl:tl)=
    if ((euclidean hl n)==1 && (rsme hl (n-1) n (reverseList (generateBinary [] (n-1))))==1) 
        then if ((length tl)==0 ) then [hl] ++ [n-hl] 
        else [hl] ++ [n-hl] ++ (getBases n tl)
    else 
        if ((length tl)==0 ) then []
        else (getBases n tl)
euclidean a b =
    if (b>0)
        then (euclidean b (mod a b) )
        else a
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
generateBinary l n=
    if (n==0)
        then l
    else (generateBinary ( [(mod n 2)] ++ l ) (quot n 2))
rsme b k n (ht:tt)= do
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

getStrongPseudoprimeBasesWrapper n=(getStrongPseudoprimeBases n  ([x | x <- [1..(n-1)]]) )
getStrongPseudoprimeBases n (hl:tl)=
    if ((euclidean hl n)==1 && (strongPseudoprime hl n (getS (n-1) 0) (quot (n-1) (2^(getS (n-1) 0))) )==1) 
        then if ((length tl)==0 ) then [hl]
        else [hl] ++ (getStrongPseudoprimeBases n tl)
    else 
        if ((length tl)==0 ) then []
        else (getStrongPseudoprimeBases n tl)
strongPseudoprime b n s t=

    if( (rsme b t n (reverseList (generateBinary [] t)))==1 || (existsIntermediateJ 0 s b n t)==1 )
        then 1
    else 0
existsIntermediateJ j s b n t=
    if (j==s)
        then 0
    else if ( (rsme b (2^j*t) n (reverseList (generateBinary [] (2^j*t))))==(n-1) )
        then 1
    else (existsIntermediateJ (j+1) s b n t)
-- getS :: Integer->Integer->Integer
getS n s=
    if ((mod n 2)==1)
        then s
    else (getS (quot n 2) (s+1))

f n= length (getStrongPseudoprimeBasesWrapper n) <= (quot (n-1) 4)

prop_1=f 15
prop_2=f 81
prop_3=f 102
prop_4=f 303
prop_5=f 816
prop_6=f 1002
prop_7=f 1205
prop_8=f 2589
prop_9=f 4533
prop_10=f 6054
prop_11=f 10203
prop_12=f (7^4)
prop_13=f (9^4)
prop_14=f (11^4)
prop_15=f (7^5)
prop_16=f (9^6)

return []

main = $(quickCheckAll)


