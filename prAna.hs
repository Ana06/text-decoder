-- PRÁCTICA DE ANA MARíA MARTÍNEZ GÓMEZ DE PROGRAMACIÓN DECLARATIVA
-- 01/06/2015

import Data.Char -- Para poder usar toLower


--descifra es la función que usará el usuario, las demás son funciones auxiliares que se llaman a partir de esta. Lo que hace es pedir al usuario el texto a
--descrifrar y otro texto del que obtendrá las frecuencias y con esta información descifra el texto (obtiene una lista de letras por cada texto ordenada por la
--frecuencia de esa letra en el texto y cambia la letra en la i-ésima posición de la lista del primer texto por la i-ésima de la lista del segundo en el texto a
--descifrar). También pide un texto con las frecuencias de las palabras. Se muestra el texto "descifrado" y se pasa a un menú con opciones para seguir descifrando el texto y guardarlo.
descifra :: IO ()
descifra = do 
	putStr "Introduzca la ruta del fichero con el texto secreto (si pulsa intro se carga el fichero texto.txt):\n"
	fichero1' <- getLine --Lee el fichero con el texto a descifrar
	let fichero1 = if fichero1' == "" then "texto.txt" else fichero1' --Si pulsa intro carga el fichero por defecto
	texto1' <- readFile fichero1 --Lee el contenido del fichero
	let texto1 = map toLower texto1' --Pasa el texto a minusculas
	putStr "Introduzca la ruta del fichero que contiene el texto con el que comparar (si pulsa intro se carga el fichero quijote.txt):\n"
	fichero2' <- getLine --Lee el fichero con el texto a comparar
	let fichero2 = if fichero2' == "" then "quijote.txt" else fichero2' --Si pulsa intro carga el fichero por defecto
	texto2' <- readFile fichero2 --Lee el contenido del fichero
	let texto2 = map toLower texto2' --Pasa el texto a minusculas
	putStr "Introduzca la ruta del fichero que contiene la frecuencia de palabras (si pulsa intro se carga el fichero frecuencias.txt):\n"
	fichero3' <- getLine --Lee el fichero con el texto con las frecuencias
	let fichero3 = if fichero3' == "" then "frecuencias.txt" else fichero3'  --Si pulsa intro carga el fichero por defecto
	texto3 <- readFile fichero3 --Lee el contenido del fichero
	--texto3 no hace falta pasarlo a minúsculas porque podemos suponer que nos lo dan en minusculas, sino basta hacer let texto3' = map toLower texto3
	let palabrasFrecuentes = listaPalabrasFrecuentes texto3 --obtenemos una lista con las palabras más frecuentes a partir del texto3
	putStr "\nDESCIFRANDO EL TEXTO. Esta operacion es muy compleja y puede tardar unos minutos, espere por favor.\n\n"
	--relacionIntercambio es una lista de tuplas, la letra de la primera componente ha de remplazarse en el texto a descifrar por la letra de la segunda
	let descifrado = let relacionIntercambio = zip (frecuenciasTexto texto1) (frecuenciasTexto texto2) 
					 in intercambia texto1 relacionIntercambio --Se intercambian las letras con la relacion dada por relacionIntercambio
	putStr descifrado --muestra el texto descifrado
	menu descifrado palabrasFrecuentes False [] -- pasa al menú

	
-- menú con opciones. Recibe el texto actual, las lista de palabras más frecuentes ordenada, un booleano que indica si se puede volver a un estado anterior del texto (es una de las opciones del menú),
--y el texto anterior en el caso de que este booleano sea True.
menu :: String -> [[Char]] -> Bool -> String -> IO ()
menu texto palabrasFrecuentes deshacer texto2= do
	if deshacer == False
	then putStr "\n\nOPCIONES:\n\n1- Intercambiar dos letras\n2- Intercambiar dos palabras (se deducen los intercambios de letras necesarios)\n3- Mostrar las palabras mas frecuentes (puede servir como ayuda para deducir que letras intercambiar)\n4- Guardar el texto en un fichero\n0- Salir\n\nSeleccione una opcion: "
	--si se puede deshacer hay una opción más en el menú
	else putStr "\n\n\nOPCIONES:\n\n1- Intercambiar dos letras\n2- Intercambiar dos palabras (se deducen los intercambios de letras necesarios)\n3- Mostrar las palabras mas frecuentes (puede servir como ayuda para deducir que letras intercambiar)\n4- Guardar el texto en un fichero\n5- Deshacer el ultimo cambio en el texto\n0- Salir\n\nSeleccione una opcion: "
	(opcion:resto) <- getLine --Solo cogemos el primer caracter introducido, desechando todo lo demas incluido el intro.
	if opcion == '0' then do --Da la opción de guardar un texto en un fichero y sale
		putStr "\nEl texto final es:\n\n"
		putStr texto
		putStr "\nSi desea exportar el texto en un archivo introduzca el nombre del archivo. En caso contrario pulse intro para salir.\n"
		nombreFichero <- getLine
		if nombreFichero == "" 
		then putStr "Hasta pronto.\n\n"
		else do
			writeFile nombreFichero texto --Se escribe en el fichero con el nombre dado el texto
			putStr ("\nSe ha guardado el texto en el fichero " ++ nombreFichero ++ ".\nHasta pronto.\n\n") --Termina
		
	else 
		if opcion == '1' then intercambioManual texto palabrasFrecuentes --Intercambia dos letras
		else 
			if opcion == '2' then intercambioPalabrasManual texto palabrasFrecuentes --Intercambia dos palabras
			else 
				if opcion == '3' then --Muestra el numero pedido de palabras mas frecuentes
					do
					putStr "\nIntroduzca el numero de palabras frecuentes que quiere que contenga la lista a mostrar (como maximo 737799 aunque no se recomienda mostrar mas de 250):\n"
					num <- getInt
					let lista =take num palabrasFrecuentes
					putStr "\n"
					muestraLista lista
					menu texto palabrasFrecuentes deshacer texto2 --Se pasan los mismos argumentos dado que no se ha modificado el texto
				else
					if opcion == '4' then --Guarda el texto en un fichero
						do
						putStr "\nIntroduzca el nombre del archivo.\n"
						nombreFichero <- getLine
						if nombreFichero == "" 
						then putStr "Nombre no valido" --El nombre no puede ser un intro
						else 
							do
							writeFile nombreFichero texto
							putStr ("\nSe ha guardado el texto en el fichero " ++ nombreFichero)
						menu texto palabrasFrecuentes deshacer texto2 --Se pasan los mismos argumentos dado que no se ha modificado el texto
					else
						if (deshacer == True && opcion == '5') then --Deshace el último cambio (cargando el fichero que teniamos antes del que almacenamos una copia)
						do
						putStr texto2
						menu texto2 palabrasFrecuentes True texto
						else do --Si no es ninguna de las opciones anteriores, la opción no es válida
							putStr "\nOPCION NO VALIDA.\n"
							menu texto palabrasFrecuentes deshacer texto2 --Se pasan los mismos argumentos dado que no se ha modificado el texto

							
--intercambioManual pide al usuario dos letras y las intercambia en el texto.
intercambioManual :: [Char] -> [[Char]] -> IO ()
intercambioManual texto palabrasFrecuentes= do
	putStr "\nIntroduzca la primera letra\n"
	(l1:resto) <- getLine --Solo cogemos el primer caracter introducido, desechando todo lo demas incluido el intro.
	putStr ("Introduzca la segunda letra:\n")
	(l2:resto) <- getLine --Solo cogemos el primer caracter introducido, desechando todo lo demas incluido el intro.
	let descifrado = intercambiaLetras l1 l2 texto
	putStr ("\nEL NUEVO TEXTO (intercambiando '" ++ [l1] ++ "' y '" ++ [l2] ++ "') e:\n\n")
	putStr descifrado --Mostramos el texto tras el intercambio
	menu descifrado palabrasFrecuentes True texto --Se vuelve al menú, conservando el texto anterior

	
--pide al usuario las dos palabras a intercambiar y llama a una función auxiliar que las intercambia en el texto, además de realizar todos los intercambios de
--letras que se deducen a partir de este cambio.
intercambioPalabrasManual :: [Char] -> [[Char]] -> IO ()
intercambioPalabrasManual texto palabrasFrecuentes= do
	putStr "\nIntroduzca la primera palabra\n"
	p1 <- getLine
	putStr ("Introduzca la segunda palabra:\n")
	p2 <- getLine
	let descifrado = recorrePalabrasIntercambiando p1 p2 texto []
	putStr ("\nEL NUEVO TEXTO - intercambiando las letras de la primera palabra (" ++ p1 ++ ") que no coinciden en la segunda (" ++ p2 ++ ") y viceversa'- ES:\n\n")
	putStr descifrado --Mostramos el texto tras el intercambio
	menu descifrado palabrasFrecuentes True texto --Se vuelve al menú, conservando el texto anterior

	
--Dadas dos palabras,para cada letra que difiera en ambas hace un intercambio. Si ocurre que una letra debe ser intercambiada por otra más de una vez solo se
--realiza el primer intercambio, dado que podriamos tener marena, que queremos reemplazar por manera y remplazar la n por la r dos veces y volver a obtener marena
recorrePalabrasIntercambiando:: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
recorrePalabrasIntercambiando [] _ texto lista= texto
recorrePalabrasIntercambiando _ [] texto lista= texto
recorrePalabrasIntercambiando (x1:p1) (x2:p2) texto lista
	|(x1 /= x2) && (not((elem x1 lista)||(elem x2 lista)))= recorrePalabrasIntercambiando p1 p2 (intercambiaLetras x1 x2 texto) (x1:x2:lista)
	|otherwise = recorrePalabrasIntercambiando p1 p2 texto lista

	
--Muestra una lista de palabras. Podíamos haber usado print para imprimir la lista de palabras, pero no se muestran bien las tildes (la lista de palabras más frecuentes tiene palabras con tildes)
--y además aparecen los corchetes de la lista, lo cuál no queda muy vistoso.
muestraLista :: [[Char]] -> IO ()					
muestraLista [] = putStr "\n"
muestraLista [x] = putStr (x ++ "\n")
muestraLista(x:xs) = 
	do
	putStr (x ++ ",")
	muestraLista xs

	
--intercambia devuelve la lista xs intercambiando los elementos que se indican en ys (ys es una lista de tuplas donde el segundo elemento es por el que se debe
--intercambiar el primero). Lo hacemos buscando para cada elemento de xs la relación en ys. No valdría recorrer ys y para cada tupla (x1,x2) remplazar todas las
--apariciones de x1 por x2, porque sino al llegar a la tupla (x2,x3) estariamos también remplazando las apariciones de x1 por x3.
intercambia :: Eq a => [a] -> [(a, a)] -> [a]
intercambia xs ys = intercambia' xs ys ys -- Se utiliza una función auxiliar para recorrer ys una vez para cada x de la lista xs
intercambia' :: Eq a => [a] -> [(a, a)] -> [(a, a)] -> [a]
intercambia' [] _ _ = [] --se ha terminado el intercambio
intercambia' (x:xs) [] ys' = x:(intercambia' xs ys' ys') --no hay relación para x, no se reemplaza
intercambia' (x:xs) (y:ys) ys'
	| x == fst y = (snd y):(intercambia' xs ys' ys') --se remplaza x
	| otherwise = intercambia' (x:xs) ys ys' --se sigue buscando la relación para x

	
--intercambiaLetras intercambia cada aparicion de a por b y de b por a en una lista dada
intercambiaLetras :: Eq a => a -> a -> [a] -> [a]
intercambiaLetras _ _ [] = []
intercambiaLetras a b (x:xs) 
	|a== x = b:(intercambiaLetras a b xs)
	|b== x = a:(intercambiaLetras a b xs)
	|otherwise = x:(intercambiaLetras a b xs)
	
	
--frecuenciasTexto devuelve una lista de palabras ordenadas de mayor a menor por su número de apariciones en un texto dado
frecuenciasTexto :: [Char] -> [Char]
frecuenciasTexto texto =
	let alfabeto = ['a' .. 'z'] in 
	let componentes = map (aux texto) alfabeto in --para cada caracter del alfabeto se calcula su número de apariciones obteniendo tuplas (caracter,apariciones)
	map fst (qSortSegunda componentes) --Ordena las tuplas por el número de apariciones (de mayor a menor)
	where aux texto caracter= (caracter, numApariciones caracter texto)

	
--numApariciones devuelve el número de apariciones de un caracter en una lista
numApariciones :: (Num a, Eq a1) => a1 -> [a1] -> a
numApariciones _ [] = 0
numApariciones caracter (x:xs)
	|x == caracter = 1 + numApariciones caracter xs
	|otherwise = numApariciones caracter xs

	
-- utiliza el algoritmo qsort ordenando tuplas por su segunda componente de mayor a menor.
qSortSegunda:: Ord a => [(a1, a)] -> [(a1, a)]
qSortSegunda [] = []
qSortSegunda (x:xs) =  qSortSegunda [ y | y <- xs, snd y >= snd x] ++ [x] ++ qSortSegunda [ y | y <- xs, snd y < snd x] 

	
--Devuelve una lista ordenada de mayor a menor de las palabras más frecuentes en un idioma supuesto que en el texto las palabras aparecen cada una en una linea
--ordenadas por frecuencias y precedidas de la posición que ocupan (i i-ésimaPalabra ...). Vease el fichero frecuencias.txt como ejemplo.
listaPalabrasFrecuentes :: String -> [String]
listaPalabrasFrecuentes texto = map (!! 1) (map words (lines texto))

--getInt de las diapositivas de clase
getInt :: IO Int
getInt = do 
	line <- getLine
	return (read line::Int)
	