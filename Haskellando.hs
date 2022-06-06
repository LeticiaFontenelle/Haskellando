module Exercicios where

Prof. Mestre Alexandre Garcia 

--Cap2 

-- 2.1
geraListaa :: [Int]
geraListaa = [11 ^ x | x <- [0..7] ]

geraListab :: [Int]
geraListab = [ x | x <- [1..39], x `mod` 4/=0]

geraListac :: [String]
geraListac = [ "A" ++ x: "BB" | x <- ['a'..'g']]

geraListad :: [Int] 
geraListad = [ x - 1 | x <- [5..44], mod x 3 == 0, x /= 15, x /= 24, x /= 36]

geraListae :: [Double]
geraListae = [ 0.5 ^ x | x <- [0..5] ]

geraListaf :: [Int]
geraListaf = [ x + 1 | x <- [0..64], mod x 9 == 0 ]

geraListag :: [Int]
geraListag = [ x | x <- [1..30], mod x 2 == 0, x /=6, x /= 14, x /= 20, x /= 26]

geraListah :: [String]
geraListah = [ [x] | x <- ['@', 'A'..'L'], x /='B', x /='F', x /='H', x /= 'I', x /= 'K']

--2.2
tamanhoString :: String-> Bool
tamanhoString x = length x `mod` 2 == 0

--2.3
vetorString :: [String]
vetorString = reverse ["h","a", "s", "k", "e", "l", "l"]


--Cap 3

--3.10
revNum:: String-> Int-> String
revNum s n = (reverse . take n) s ++ drop n s

--3.11
data Binario = Zero | Um deriving (Eq,Show)
data Funcao = Soma2 | Maior | Menor | Mult2 
aplicar :: Funcao-> Binario-> Binario-> Binario
aplicar Soma2 Um Um = Zero
aplicar Maior Um Zero = Um
aplicar Menor Um Zero = Zero
aplicar Mult2 Um Zero = Zero


--3.13
data Metros = Metros Double | MetragemInvalida deriving Show
 
areaQuadrado :: Metros-> Metros
areaQuadrado (Metros l) = Metros (l*l) 

areaRet :: Metros-> Metros-> Metros
areaRet (Metros b) (Metros a) = Metros (b*a)

areaCubo :: Metros-> Metros
areaCubo (Metros c) = Metros (6* (c*c) )


--3.14
data Valido = Sim String| Nao deriving (Eq,Show)

isNomeValido :: String-> Valido
isNomeValido [] = Nao
isNomeValido casocontrario = Sim casocontrario 

--Cap 4

--4.7

data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado deriving Show

listadeDias :: Dia-> Bool
listadeDias Terca = True
listadeDias _ = False

filtTercas :: [Dia]-> [Dia]
filtTercas = filter listadeDias Terca
