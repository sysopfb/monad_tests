import Control.Monad.State

type GCDState = (Int, Int)

type FactState = (Int, Int)

fact' :: State FactState Int
fact' = 
  do (a, b) <- get
     if b > 1
       then 
            do put (a*b, b-1)
	       fact'
       else
            return a

run_fact :: Int -> Int
run_fact a = evalState fact' (a, a-1)

--fib :: Int -> Int
--fib 0 = 0
--fib 1 = 1
--fib n = fib (n - 1) + fib (n - 2)

type FibState = (Int, Int, Int)

fib' :: State FibState Int
fib' = 
  do (a, b, c) <- get
     case c<2 of
       True -> return a
       False -> do put (b, a+b, c-1)
       	           fib'
	
run_fib :: Int -> Int
run_fib a = evalState fib' (0, 1, a)


--from mvanier on livejournal
gcd_s3 :: State GCDState Int
gcd_s3 = 
  do (x, y) <- get
     case compare x y of
       EQ -> return x
       LT -> do put (y, x)
                gcd_s3
       GT -> do put (y, x - y)
                gcd_s3

run_gcd_s3 :: Int -> Int -> Int
run_gcd_s3 x y = evalState gcd_s3 (x, y)

