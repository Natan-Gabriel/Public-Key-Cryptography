
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


