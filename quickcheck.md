#####Leyes condicionales
En algunos casos las leyes que queremos definir no pueden ser representadas mediante una simple función y solo son ciertas bajo unas precondiciones muy concretas. Para dichos casos **Quickcheck** cuenta con el operador de implicación **==>** para representar dichas leyes condicionales.
Por ejemplo una ley tan simple como la siguiente:
```haskell
	x <= y ==> max x y == y
```
Puede ser representada por la siguiente definición.
```haskell
	prop_MaxLe :: Int -> Int -> Property
	prop_MaxLe x y = x <= y ==> max x y == y
```
O la propiedad de inserción en listas ordenadas.
```haskell
	prop_Insert :: Int -> [Int] -> Property
	prop_Insert x xs =
		ordered xs ==> ordered (insert x xs)
```

En ambos ejemplos podemos observar que el resultado de la función es de tipo *Property* en vez de *Bool*, lo cual es debido a que en el caso de las leyes condicionales en vez de probar la propiedad por para 100 casos de prueba aleatorios, esta es probada contra 100 casos que cumplan la precondición establecida. Si uno de los candidatos no cumple la propiedad este será descartado y se considerará el siguiente.
Debemos tener en cuenta una cosa más en cuanto a las leyes condicionales para aquellas en las que la condición rara vez sea satisfecha podemos llegar a un mensaje como este.
```haskell
	Arguments exhausted after 64 tests
```

Lo cual significa que despues de generar el máximo número de casos de prueba (que por defecto son 1000) solo ha encontrado 64 de ellos que cumplan la condición. Dicho límite esta pensado para que el programa no busque indefinidamente en caso de que no haya más casos que cumplan la precondición si no que lo intente con un número razonable de casos de prueba, si no encuentra 100 tests válidos antes de dicho número entonces el programa simplemente anuncia cuantos tests pudo realizar correctamente.

#####Monitorizando los datos
Al testear propiedades debemos tener cuidado, pues quizás parezca que hemos probado una propiedad a fondo para estar seguros de su credibilidad pero esta simplemente ser aparente. Voy a intentar ejemplificarlo añadiendo unos cambios a la función anterior *prop_Insert*.
```haskell
	prop_Insert :: Int -> [Int] -> Property
	prop_Insert x xs =
		ordered xs ==>
			classify (null xs) "trivial" $
				ordered (insert x xs)
```
Lo cual nos permite monitorizar cuantos de los casos que satisfacen la condicion consisten en una inserción sobre una lista vacia, en cuyo caso la condición de *ordered xs* es trivial.
Si ejecutamos esta nueva función con **Quickcheck** obtenemos el siguiente mensaje.
```haskell
	Ok, passed 100 tests (43% trivial)
```
Es decir que el 43% de los tests realizados son sobre una lista vacia.

Pero a su vez **quickcheck** nos ofrece la posibilidad de un mejor análisis, más alla de etiquetar uno de los casos que nos interese. Podemos realizar una especie de histograma, utilizando la palabra reservada *collect*, que nos dará una mayor información de la distribución de los casos de prueba, por ejemplo en este caso ´según su longitud.
```haskell
	prop_Insert :: Int -> [Int] -> Property
	prop_Insert x xs =
		ordered xs ==>
			collect (length xs) $
				ordered (insert x xs)
```
Al ejecutarlo obtendriamos un resultado como el siguiente.
```haskell
	Ok, pased 100 tests.
	49% 0.
	32% 1.
	12% 2.
	4% 3.
	2% 4.
	1% 5.
```
Lo cual nos permite observar que de los 100 casos de prueba que cumplían la condición solo 19 de ellos trabajan con listas mayores de tamaño 1, lo cual es uno de los grandes problemas de los generadores por defecto que nos proporciona **quickcheck** y para poder solucionar dicho problema se nos proporciona la posibilidad de definir nuestros propios generadores.

######Definir generadores
En primer lugar vamos a empezar definiendo la clase de tipos **Arbitrary** de la cual un tipo es una instancia si podemos generar casos aleatorios de él. La manera de generar los casos de prueba depende por supuesto en el tipo.
```haskell
	class Arbitrary a where
		arbitrary :: Gen a
```

*Gen*  es un tipo abstracto representando el generador para el tipo a, que bien puede ser el generador por defecto o uno creador por el programador para el caso específico. El tipo abstracto Gen  se define como:
```haskell
	newtype Gen a = Gen (Rand -> a)
```
En esta definición Random se trata de un número semilla aleatorio y un generador no es más que una función que puede generar una *a* de una manera pseudoaleatoria.

Ahora vamos a echarle un vistazo a las posibilidades que nos ofrece **Quickcheck** a la hora de definir los generadores de casos para los tipos de datos definidos por el usuario. El programa se basa en la idea de que a pesar de que se podrían generar dichos generadores mediante un preprocesamiento prefirieron optar por la idea de que sea el usuario el que los defina personalmente para de esta manera mantener la herramienta lo más ligera posible y a la vez que cuando el usuario quiera probar una propiedad no tenga que esperar a que se realice todo el preprocesamiento si no que la prueba sea lo más rápida posible.

Supongamos que definimos el tipo *Colour* de la siguiente manera
```haskell
	data Coulour = Red | Blue | Green
```
Un ejemplo de un generador para dicho tipo en el cual los tres colores son equiprobables sería
```haskell
	instance Arbitrary Colour where
		arbitrary = oneof
			[return Red | return Blue | return Green]
```
en el cual podemos observar el funcionamiento de la función oneof que se encarga de devolver uno de los elementos de la lista dando la misma probabilidad a todos ellos.

Vamos a observar otro ejemplo, un generador para listas de un tipo *a* arbitrario.
```haskell
	instance Arbitrary a => Arbitrary [a] where
		arbitrary = frequency
			[ (1, return [])
			  (4, liftM2 (:) arbitrary arbitrary)]
```
En ella usamos la función *frequency* la cual funciona similar a *oneof* pero dandole pesos diferentes a los diferentes casos. En este ejemplo le damos peso 1 a la lista vacia y peso 4 a la lista compuesta de otras 2 listas, con lo cual obtenemos casos de prueba de una longitud media de 4. Teniendo en cuenta el ejemplo anterior visto sobre la inserción en una lista ordenada en la cual la mayoria de los casos de prueba eran de longitud 0 o 1 podemos observar que es interesante definir los generadores manualmente ya que esto produce unos mejores casos de prueba.

