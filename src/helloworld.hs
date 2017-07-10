doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else doubleMe x

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]
len' :: (Num b) => [a] -> b
len' [] = 0
len' (_: xs) = 1 + len' xs

removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number Seven!"
lucky x = "Sorry, not lucky pal!"

factorial :: (Integral a) => a -> a

factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a

head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

sum' :: (Num a) => [a] -> a

sum' [] = 0
sum' (x : xs) = x + sum' xs

capital :: String -> String
capital "" = error " Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "you are underweight, you emo "
  | bmi <= normal = " Your supposedly normal, bet you ugly"
  | bmi <= fat = "your fat loose some weight fatty !"
  | otherwise = "congrats, your a while"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b 

myCompare :: (Ord a) => a -> a -> Ordering

a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
   where (f:_) = firstname
         (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <-xs, let bmi = w/h ^ 2, bmi >= 25.0]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea

maximum' :: (Ord a) => [a]  -> a
maximum' [] = error "Cant take max of empty list"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: (Num i , Ord a) => i -> a -> [a]
replicate' (0) (x) = []
replicate' (i) (x) = x : replicate (i - 1) x
