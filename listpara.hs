parar            :: (a -> [a] -> b -> b) -> b -> [a] -> b
parar f z []     = z
parar f z (x:xs) = f x xs (parar f z xs)

paral            :: (b -> a -> [a] -> b) -> b -> [a] -> b
paral f z []     = z
paral f z (x:xs) = paral f (f z x xs) xs

tails :: [a] -> [[a]]
tails = parar (\x xs r -> (x:xs):r) [[]]
--tails = parar (((:) .) . (:)) [[]]

tailsl = (:) [] . paral (\r x xs -> (x:xs):r) []


dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p = parar (\x xs r -> if p x then r else x:xs) []

mapPara :: (a -> b) -> [a] -> [b]
mapPara f = parar (\x xs r -> (f x):r) []
--mapPara f = parar (const . (:) . f) []


--mapAccumR/L can be expressed as a fold 

--foldr           :: (x -> (acc, [y]) -> (acc, [y])) -> (acc, [y]) -> [x] -> (acc, [y])
mapAccumR'        :: (acc -> x -> (acc, y))          -> acc        -> [x] -> (acc, [y])
mapAccumR' f z xs = foldr (\x (acc, ys) -> let (acc', y) = f acc x in (acc', y:ys)) (z, []) xs

r = snd . mapAccumR' (\_ x -> (undefined, 2 * x)) undefined $ [1,2,3]

mapAsMapAccumR'   :: (a -> b) -> [a] -> [b]
mapAsMapAccumR' f = snd . mapAccumR' (\_ x -> (undefined, f x)) undefined
