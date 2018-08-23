soma x = soma' x 0
  where soma' [] acc = acc
        soma' (x:xs) acc = soma' xs (acc+x)

--tamanho de uma lista
tamanho list = soma [1 | x<-list]

--soma dos pares
somaPar list = soma [x | x<-list , mod x 2 ==0]

--conta qtas vezes o item aparece na lista
vezesLista it lista = soma [1 | x<-lista, x==it]

--remover item na lista
remove it lista = [x | x<-lista, x/=it]

--troca velho por novo todas as vezes
troca v n lista = [if x == v then n else x | x<-lista]

--quiksort
qs [] = []
qs (x:xs) = menor ++ [x] ++ maior
  where menor = qs[y | y<-xs, y<=x]
        maior = qs[y | y<-xs, y>x]

--os do chave ai fds
-- zip
pareie [] _ = []
pareie _ [] = []
pareie (x:xs) (y:ys) = (x, y) : pareie xs ys

-- posicoes - dado um item e uma lista, retorna uma lista com todas as posicoes (primeiro elemento esta na posicao 1) do item na lista
posicoes e xs = let par = pareie xs [1..(tamanho xs)] in
                [i | (x, i) <- par, x == e]

