import System.Random

dot a b = sum $ zipWith (*) a b
neuron layer (w,b) activ = activ $ (dot w layer) + b
layer prev wb activ = map (\x -> neuron prev x activ) wb
net first wb activ = scanl (\x y -> layer x y activ) first wb
weights wb = map (\x -> map (\y -> fst y) x) wb
biases wb = map (\x -> map (\y -> snd y) x) wb
wb w b = zipWith (zipWith (,)) w b

select x y 0 = x
select x y r = y
crossover x y r = zipWith3 select x y r
mutate1 x 0 f = f x
mutate1 x r f = x
mutate x r f = zipWith (\a b -> mutate1 a b f) x r
genpopulation x y m = map (\a -> crossover x y a) m

rand01 n = sequence $ replicate n $ randomRIO (0,1::Int)
randmat01 n m = sequence $ replicate n $ rand01 m
rgenome n wlimit = sequence $ replicate n $ randomRIO(-wlimit::Float, wlimit::Float)
exnihilopop n size wlimit = sequence $ replicate n $ rgenome size wlimit
reproduce x y n = do
  rm <- randmat01 n (length x)
  return $ genpopulation x y rm
rmutate pop f = do
  r <- rand01 $ (length pop)
  return $ mutate pop r f
main = do
        print $ net [0,1] (wb [[[0.1,0.2],[0.2,0.3]]] [[0,1]]) (\x -> 2 * atan x)
        nai <- randmat01 2 2
        print $ genpopulation [1,2] [3,4] nai
