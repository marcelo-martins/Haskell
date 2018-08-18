--tamanho de uma lista
tamanhoLista [] = 0
tamanhoLista list = 1 + tamanhoLista (tail(list))

--soma dos elementos de uma lista
sumLista [] = 0
sumLista (x:xs) = x + (sumLista xs)

--soma dos pares
par x = (if (x `mod` 2) == 0 then True else False)
par' x = if mod x 2 ==0 then True else False


somaParesLista [] = 0
somaParesLista (x:xs) = (if par x then x else 0) + somaParesLista xs

--soma dos elementos nas posicoes pares da lista(se primeira pos =0)
somaPosPar [] = 0
somaPosPar [x] = x
somaPosPar (x:xs) = x + somaPosPar(tail xs)

--existe item na lista
existeItem item [] = False
existeItem item (x:xs) = if x==item then True else existeItem item xs

--posicao do item na lista, 0 se nao ta, 1 se é o primeiro
posItemLista item [] = 0
posItemLista item (x:xs)
  | x == item = 1
 -- | existeItem item xs = 1 + posItemLista item xs
  | otherwise = 1 + posItemLista item xs

--conta quantas vezes o item aparece na lista
contaVezes item [] = 0
contaVezes item (x:xs)
 | x==item = 1 + contaVezes item xs
 |otherwise = contaVezes item xs

--retorna o maior elemento
maiorElem [] = 0
maiorElem [x] = x
maiorElem (x:xs)
  | maiorElem xs > x = maiorElem xs
  | otherwise = x

--reverter a lista
reverte [] = []
reverte (x:xs) = reverte xs ++ [x]

--itercala 1 => [1,2,3] [4,5,6,7,8] = [1,4,2,5,3,6]
intercala1 [] [] = []
intercala1 x [] = []
intercala1 [] x = []
intercala1 (x:xs) (y:ys) = x:y:intercala1 xs ys

--ouuuuu
intercala1' x y
  | x == [] = []
  | y==[] = []
  |otherwise = (head x):(head y):intercala1' (tail x) (tail y)

--intercala2 => [1,2,3] [4,5,6,7,8] = [1,4,2,5,3,6,7,8]
intercala2 [] [] = []
intercala2 x [] = x
intercala2  [] x = x
intercala2 (x:xs) (y:ys) = x:y:intercala2 xs ys

--a lista ja ta ordenada?
ordenada [] = True
ordenada [x] = True
ordenada (x:y:xs) = (x<=y) && ordenada xs

--gera lista de 1 a n
gera 0 = []
gera n = gera (n-1) ++ [n]

--retorna o ultimo elemento da lista
returnLast [x] = x
returnLast (x:xs) = returnLast xs

--retorna sem o ultimo elemento
returnWithoutLast [x] = []
returnWithoutLast (x:xs) = x:returnWithoutLast xs

--shift right =>
shiftr [] = []
shiftr list = head(reverte list): reverte(tail(reverte list))

--shift right n vezes                       --la embaxio
shiftrN 0 list = list
shiftrN 1 list = shiftr list
shiftrN n list = shiftrN (n-1) list

--shift lef
--[1,2,3,4] --> [2,3,4,1]
shiftl [] = []
--shiftl list = tail list : head list
shiftl (x:xs) = xs ++ [x]

--remove item da lista 1 vez
remove1vez item [] = []
remove1vez item list
  | head list == item = tail list
  |otherwise = head list : remove1vez item (tail list)

--remove item da lista todas as vezes
removeVariasvez item [] = []
removeVariasvez item list
  | head list == item = removeVariasvez item (tail list)
  |otherwise = head list : removeVariasvez item (tail list)

--remove item da lista n vezes
removeNvez n item [] = []
removeNvez n item list
  |n==0 = list
  |head list == item = removeNvez (n-1) item (tail list)
  |otherwise = head list : removeNvez (n) item (tail list)

--remove item da lista(a ultima vez que ele aparece)
removeLastTime item [] = []

removeLastTime item list = reverte(remove1vez item (reverte list))

-- remove item da lista (a ultima vez que ele aparece) **
removeUlt e [] = []
removeUlt e (x:xs)
  | x /= e = x : removeUlt e xs
  | otherwise = if xs == removeUlt e xs then xs
                                        else x : removeUlt e xs

--troca velho por novo 1 vez
changeOld1 n v [] = []
changeOld1 n v (x:xs)
  | x == v = n:xs
  |otherwise = x:changeOld1 n v xs

--troca velho por novo todas as vezes
changeOldAll n v [] = []
changeOldAll n v (x:xs)
  | x==v = n:changeOldAll n v xs
  |otherwise = x: changeOldAll n v xs

--troca velho por novo "vez" vezes
changeOldVez n v vez [] = []
changeOldVez n v vez (x:xs)
  | vez==0 = x:xs
  | x==v = n:changeOldVez n v (vez -1) xs
  |otherwise = x: changeOldVez n v (vez) xs





                     --os do chave

-- shift right
-- shiftr [1,2,3,4]
-- ==> [4,1,2,3]
shiftrChave [] = []
shiftrChave [x] = [x]
shiftrChave (x:xs) = head (shiftr xs) : x : tail (shiftrChave xs)

-- shiftr n lista (shift right n vezes)
shiftrNChave [] n = []
shiftrNChave [x] n = [x]
shiftrNChave (x:xs) 0 = (x:xs)
shiftrNChave (x:xs) n = shiftrNChave (head (shiftrNChave xs 1) : x : tail (shiftrNChave xs 1)) (n-1)

-- shift lef
-- shiftl [1,2,3,4]
-- ==> [2,3,4,1]
shiftlChave [] = []
shiftlChave (x:xs) = xs ++ [x]

-- shift lef n vezes
shiftlNChave [] n = []
shiftlNChave [x] n = [x]
shiftlNChave (x:xs) 0 = (x:xs)
shiftlNChave (x:xs) n = shiftlNChave (xs ++ [x]) (n-1)

{-
-- remove item da lista (a ultima vez que ele aparece) **
removeUlt e [] = []
removeUlt e (x:xs)
  | x /= e = x : removeUlt e xs
  | otherwise = if xs == removeUlt e xs then xs
                                        else x : removeUlt e xs
                                        -}

