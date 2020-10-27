



rsme b k n (ht:tt)= do
    let a = 1
    if (k==0)
        then a
        else do
            let cc=b
            let aa = if (ht==1)
                then b
                else a
            (forLoopRsme aa cc b n k tt)



-- forLoopRsme :: Integer->Integer->Integer->Integer->[Integer]->Integer
forLoopRsme a c b n k (ht:tt) = do 
    let cc =(mod (c*c) n)
    
    let aa = if (ht==1)
            then
                (mod (c*a) n)
            else 
                a
    if ( (length tt)==0 ) 
        then aa
    else (forLoopRsme aa cc b n k tt)

-- ff :: Integer->Integer->Integer->Integer->Integer->[Integer]->[]
-- ff a c b n k (ht:tt) = 
    
--     if ( (length tt)==0 ) 
--         then tt
--         else (ff a c b n k tt)
    