module Check where

import Data.Function (fix)
import Data.Vector (Vector, (!), (!?), (//), constructN,  fromList, length, unfoldr)
import Prelude hiding (length) 


type Seed = (Int, Vector Int)

unf :: Seed -> Maybe (Vector Int, Seed)
unf (i, acc) =

    (!?) acc i >>= 
    
        Just . ( \el -> 

            let newAcc = (//) acc [(i, el + 5)]
            
            in (newAcc, (i + 1, newAcc))
        )

u :: Vector (Vector Int)
u = unfoldr
        unf
        (0, fromList [1,3,8])

-- [[6,3,8],[6,8,8],[6,8,13]]


--constructN :: Int -> (Vector a -> a) -> Vector a

--constructN 3 f =
--    let a = f <> ;
--        b = f <a> ;
--        c = f <a,b>
--    in <a,b,c>

--u1 :: Vector Int
--u1 = constructN
--        unf
--        (0, fromList [1,3,8])


f rec n =
   if n <= 1
   then 1
   else n * rec (n-1)

--fix :: (a -> a) -> a
--fix f 5
--120

fV rec (i, vector) =
    if i == length vector
    then vector
    else rec (
        i + 1,
        (//)
            vector
            [(
                i,
                (+) 10 $ (!) vector i
            )]
    )
