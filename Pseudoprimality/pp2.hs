



-- rsme c b n k (ht:tt)=
    -- let a = 1
--     if(k==0)
--         then a
--     let c=b
--     if (ht==1)
--         then let a=b
--     write (forLoopRsme c b n k tt)


-- forLoopRsme c b n k (ht:tt) =
--     -- let c1 = c*c
--     if (ht==1)
--         then  let a=c*a mod n 
--     (forLoopRsme c1 b n k tt)

-- forLoopRsme :: Integer->Integer->Integer->Integer->[Integer]->Integer
forLoopRsme a c b n k (ht:tt) = do 
    let cc = if (ht==1)
            then (mod (c*c) n)
            else c
    
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
    