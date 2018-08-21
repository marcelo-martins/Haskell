--tamanho de uma lista
tamanhoLista list = tamanhoLista' list 0
    where tamanhoLista' [] acc = acc
          tamanhoLista' (x:xs) acc = tamanhoLista' xs (acc+1)

--soma elementos
soma lista = soma' lista 0
    where soma' [] acc = acc
          soma' (x:xs) acc = soma' xs (acc+x)

--soma dos numeros pares de uma lista
somaPar lista = somaPar' lista 0
    where somaPar' [] acc = acc
          somaPar' (x:xs) acc
            |mod x 2 ==0 = somaPar' xs (acc+x)
            |otherwise = somaPar' xs acc

--soma elementos nas posicoes pares
somaPosPar lista = somaPosPar' lista 0
    where somaPosPar' [] acc = acc
          somaPosPar' [x] acc = (acc)
          somaPosPar' (x:xs) acc = head xs + somaPosPar' (tail xs) acc

--existe item na lista
existe it lista = existe' it lista False
    where existe' it [] acc = acc
          existe' it (x:xs) acc
            |x==it = True
            |otherwise = existe' it xs acc

--posicao do elemento na lista(0 se nao ta, 1 se eh o primeiro)
posicaoLista it lista = posicaoLista' it lista 0
    where posicaoLista' it [] acc= acc
          posicaoLista' it (x:xs) acc
            |x==it = (acc + 1)
            |otherwise = posicaoLista' it xs (acc+1)

--conta quantas vezes o item aparece na lista
qtasVezes it lista = qtasVezes' it lista 0
    where qtasVezes' it [] acc = acc
          qtasVezes' it (x:xs) acc
            |x==it = qtasVezes' it xs (acc+1)
            |otherwise = qtasVezes' it xs acc

--maior
maior [x] = x
maior (x:xs)
  |mm>=x = mm
  |otherwise = x
  where mm = maior xs

--reverter lista com acc
reverte lista = reverte' lista []
    where reverte' [] acc = acc
          reverte' (x:xs) acc = reverte' xs (x:acc)

--intercalas ficam iguais

--ja ordenada fica igual

--gerar lista de 1 a n
gera n = gera' n []
    where gera' n acc
            |n==0 = acc
            |otherwise = gera' (n-1) ([n]++acc)


--o resto fica igual

--posicoes do elemento na lista
posicoes it lista = posicoes' it lista 1
    where posicoes' it [] acc = []
          posicoes' it (x:xs) acc
            |x==it = acc:posicoes' it xs (acc+1)
            |otherwise = posicoes' it xs (acc+1)

--split "qwertyuiopoiuyt" 't' ==> ["qwer", "yuiopoiuyt"]
split char str = split' char str []
    where split' char "" acc = [acc, []]
          split' char (x:xs) acc
            |char == x = [acc, xs]
            |otherwise = split' char xs (acc++[x])

--splitall mesma coisa mas retorna todas as sublistas
splitall char str = splitall' char str []
    where splitall' char "" acc = [acc]
          splitall' char (x:xs) acc
            |char==x = (acc:splitall' char xs [])
            |otherwise = splitall' char xs (acc++[x])

--drop n lista = a lista sem os primeiros n elementos
drope n [] = []
drope 0 list = list
drope n (x:xs) = drope (n-1) xs

--take n lista = os primeiros n elementos da lista
takee n lista = takee' n lista []
    where takee' n [] acc = acc
          takee' 0 lista acc = acc
          takee' n (x:xs) acc = takee' (n-1) xs (acc++[x])
