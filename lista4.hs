data Tree a = Vazia | No a (Tree a) (Tree a) deriving (Eq,Show,Read)

--acha item em arvore binaria
acha :: (Ord a) => a -> (Tree a) -> Bool
acha it Vazia = False
acha it (No a esq dir)
  |it == a = True
  |it < a = acha it esq
  |otherwise = acha it dir



exemplo =
    No 25
        (No 20
            (No 10
                (No 1 Vazia Vazia)
                (No 12 Vazia Vazia)
            )
            (No 22
                (No 21 Vazia Vazia)
                (No 23 Vazia Vazia)
            )
        )
        (No 36
            (No 30
                (No 28 Vazia Vazia)
                (Vazia)
            )
            (No 48
                (No 45 Vazia Vazia)
                (No 50 Vazia Vazia)
            )
        )
exemplo2 =
    No 5
        (Vazia)
        (No 21
            (No 19 Vazia Vazia)
            (No 25 Vazia Vazia)
        )

--retorna o item mais a direita
mais_dir :: (Ord a) => (Tree a) -> a
mais_dir (No a _ Vazia) = a
mais_dir (No a _ dir) = mais_dir dir

--retorna o item mais a esq
mais_esq :: (Ord a) => (Tree a) -> a
mais_esq (No a Vazia _) = a
mais_esq (No a esq _) = mais_esq esq

--verifica se a arvore eh uma abbusca
eh_abb :: (Ord a) => (Tree a) -> Bool
eh_abb (No a Vazia _) = True
eh_abb (No a _ Vazia) = True
eh_abb (No a esq dir)
  |a<min = False
  |a>max = False
  |otherwise = (eh_abb esq) && (eh_abb dir)
  where min = mais_esq (No a esq dir)
        max = mais_dir (No a esq dir)



--insere item numa abb
insere:: (Ord a) => a -> (Tree a) -> (Tree a)
insere it (Vazia) = (No it Vazia Vazia)
insere it (No a esq dir)
  |it==a = (No a esq dir)
  |it>a = No a esq (insere it dir)
  |otherwise = No a (insere it esq) dir

--profundidade
prof :: (Ord a) => (Tree a) -> Int
prof Vazia = 0
prof (No a esq dir) = 1 + max(prof esq) (prof dir)

--remove item de uma abb
remove :: (Ord a) => a -> (Tree a) -> (Tree a)
remove it Vazia = Vazia
remove it (No a esq dir)
  |it == a = remove_raiz (No a esq dir)
  |it<a = (No a (remove it esq) dir)
  |it>a = (No a esq (remove it dir))

remove_raiz :: (Ord a) => (Tree a) -> (Tree a)
remove_raiz (No a Vazia dir) = dir
remove_raiz (No a esq Vazia) = esq
remove_raiz (No a esq dir) = (No v esq (remove v dir))
  where v = mais_esq dir

--converte uma abb em uma lista em ordem infixa (arv esq, no, arv dir)
ordem_inf :: (Ord a) => (Tree a) -> [a]
ordem_inf Vazia = []
ordem_inf (No a esq dir) = (ordem_inf esq) ++ [a] ++ (ordem_inf dir)

--converte uma abb em uma lista em ordem prefixa(no, arv esq, arv dir)
ordem_pref :: (Ord a) => (Tree a) -> [a]
ordem_pref Vazia = []
ordem_pref (No a esq dir) = [a] ++ (ordem_pref esq) ++ (ordem_pref dir)

--transforma uma lista em uma abb
gera_arv :: (Ord a) => [a] -> (Tree a)
gera_arv list = gera_arv' list Vazia
    where gera_arv' [] arv = arv
          gera_arv' (x:xs) arv = gera_arv' xs (insere x arv)










--

