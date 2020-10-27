



rsme b k n (ht:tt)= do
    let a = 1
    if (k==0)
        then a
        else do
            let c=b
            let aa = if (ht==1)
                then b
                else a
            (forLoopRsme aa c n k tt)



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

-- ff :: Integer->Integer->Integer->Integer->Integer->[Integer]->[]
-- ff a c b n k (ht:tt) = 
    
--     if ( (length tt)==0 ) 
--         then tt
--         else (ff a c b n k tt)
    