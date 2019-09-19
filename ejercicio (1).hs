import Data.List
sumar:: [Int]->Int
sumar [ ] = 0
sumar (x:xs) = x + sumar xs

promedio::[Int]->Int
promedio a = div (sumar a) (length a)

--Primer ejercicio: promedio de los elementos de cada sublista de una lista de listas

promedioListas:: [[Int]]->[Int]
promedioListas y = [promedio x | x<-y]

mayor::[Int]->Int
mayor [x] = x
mayor (x:xs) | x > mayor(xs) = x| otherwise = mayor(xs)

menor::[Int]->Int
menor [x] = x
menor (x:xs) | x < menor(xs) = x | otherwise = menor(xs)

--Segundo ejercicio: mayor elemento de cada sublista de una lista de listas

mayorElemento::[[Int]]->[Int]
mayorElemento y = [mayor x | x<-y]

menorElemento:: [[Int]]->[Int]
menorElemento y = [menor x | x<-y]

--Tercer ejercicio: Realizar unas tuplas dadas unas condiciones a partir de una lista de listas

cantidadMultiplo :: Int ->[Int]->[Int]
cantidadMultiplo x y = [z | z<-y , mod z x == 0 && z/=x]

cantidadDivisor :: Int ->[Int]->[Int]
cantidadDivisor x y = [z | z<-y ,  mod x z == 0 && z/=x]

dupla:: [[Int]]->[(Int,Int,Int,Int)]
dupla d = [(menor l, mayor l, length(cantidadMultiplo (menor l) l), length(cantidadDivisor (mayor l) l))| l <-d]