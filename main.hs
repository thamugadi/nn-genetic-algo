import System.Random

dot a b = sum $ zipWith (*) a b
neuron layer (w,b) activ = activ $ (dot w layer) + b
layer prev wb activ = map (\x -> neuron prev x activ) wb
net first wb activ = scanl (\x y -> layer x y activ) first wb

weights wb = map (\x -> map (\y -> fst y) x) wb
biases wb = map (\x -> map (\y -> snd y) x) wb
wb w b = zipWith (zipWith (,)) w b

crossover [] [] _ = []
crossover (x:xs) (y:ys) (0:rs) = x : crossover xs ys rs
crossover (x:xs) (y:ys) (1:rs) = y : crossover xs ys rs

mutate [] [] _ = []
mutate (x:xs) (0:rs) f = f x : mutate xs rs f
mutate (x:xs) (1:rs) f = x : mutate xs rs f

genpopulation _ _ [] = []
genpopulation x y (m:ms) = [crossover x y m] : genpopulation x y ms 


randmat01 n m = sequence $ replicate n $ rand01 m
rand01 n = sequence $ replicate n $ randomRIO (0,1::Int)
rgenome n wlimit = sequence $ replicate n $ randomRIO(-wlimit::Float, wlimit::Float)
exnihilopop n size wlimit = sequence $ replicate n $ rgenome size wlimit

reproduce x y n = do
  rm <- randmat01 n (length x)
  return $ genpopulation x y rm

rmutate pop f = do
  r <- rand01 $ (length pop)
  return $ mutate pop r f


main = print $ net [0,1] (wb [[[0.1,0.2],[0.2,0.3]]] [[0,1]]) id
