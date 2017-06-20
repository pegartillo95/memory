---
title: TFG
bibliography: bibliography.bib
---

##Preliminares

### 2.1: CAVI-ART Project
The first point I would like to talk about in this preliminary section is the CAVI-ART project, that consists of a project being developped here at the UCM of which my Final Degree Project is part. 

The CAVI-ART platform consists of a set of tools aimed at assisting the programmer in the validation of programs written in a variety of languages. These aids include the automatic extraction and proving of verification conditions, the automatic proving of termination (whenever this is decidable by using the state-of-the-art technology), the automatic inference of some invariants, and the automatic generation and running of test-cases. Some of its results have been reported elsewhere.

A key aspect of the platform is its Intermediate Representation of programs (in what follows, IR). Source programs written in conventional languages such as C++, Java, Haskell, OCaml, and others, are translated to the IR, so that all the above mentioned activities are performed at the IR level. The intention is to program most of the platform tools once forever, independently of the source language at hand.

The design of the IR was done with the aim of easing the above tasks as much as possible. Our experience on static analysis led us to avoid the complications of mutable state, and also to a simple design having very few primitive constructions. The result was an IR very near to a desugared functional language with flattened expressions. We never thought on the IR as being executable code, but rather we looked at it as an abstract syntax on which it were easy to perform static analysis and formal verification .But on the last months we decided to convert the IR into executable code to make it possible running  language independent tests, and also building language independent testing tools. Most of the work done on testing, and almost all the existing testing tools are tied to a particular language.

![Scheme of CAVI_ART project](imagenes/caviart.jpg "Scheme of CAVI_ART project")


### 2.2: QuickCheck

### 2.3: Generics

El siguiente punto a tratar en estos preliminares es la librería Generics [@generics] de Haskell, una librería utilizada principalmente para la generación automática de instancias de funciones correctas para cualquiera que sea el tipo de datos. En el caso de este proyecto Generics apareció como una librería necesaria pues debiamos conseguir escribir funciones como "compose" de manera que funcionaran para cualquier tipo de datos, incluidos los definidos por el usuario y de los cuales no podemos tener conocimiento en adelantado.

La librería Generics dentro de Haskell es posible por dos caracteristicas del propio lenguaje:

1. En primer lugar la existencia de las clases de tipos, que actuan como una interfaz definiendo el comportamiento de los tipos que pertenecen a dicha clase.

2. En segundo lugar, gracias a la existencia del polimorfismo de tipo Ad-hoc. Este nos permite abstraer una operación sobre una o mas clases de tipos simplemente con la condición de que el tipo concreto tenga unas propiedades como por ejemplo que sea ordenable (pertenezca a la clase Ord) o que sus elementos admitan comparaciones por igualdad (que pertenezca a la clase Eq).

En el caso de esta librería, la clase de tipos principal (Generic) expresa la posibilidad de describir un tipo de datos en terminos de un conjunto simple de combinadores. Estos combinadores son:

- En primer lugar debemos definir el comportamiento deseado para los tipos de datos vacios (representados con **V1** en Generics)

- En segundo lugar debemos definir el comportamiento deseado para los tipos de datos cuyo constructor carece de parámetros (representados con **U1** en Generics).

- En tercer lugar se trata de definir el comportamiento para los tipos compuestos de acuerdo a como se forman. En Haskell los tipos compuestos solo pueden definirse mediante dos operaciones partiendo de los tipos básicos. Estas dos operaciones son la suma y el producto de tipos (representados como **:+:** y **:*:** respectivamente en Generics). Deberemos definir como queremos que sea el comportamiento de las funciones de nuestra clase genérica de acuerdo a como se forma nuestro tipo a partir de los tipos básicos.

- Por último están dos tipos para representar meta-información y etiquetado de tipos(representados respectivamente por M1 y K1), que nos permitirán definir el comportamiento esperado para las funciones cuando esta depende de las etiquetas o parte de la meta-información del tipo.

Una vez definidas las funciones para estos cinco diferentes combinadores es necesario definir algunas instancias para los tipos predefinidos como **Int**, **Char**, **Boolean**... de manera que si el usuario crea un tipo complejo como por ejemplo un Diccionario con valores de **Int** como clave y **Char** como valores tengamos un punto de partida para construir mediante Generics las instancias en nuestra clase para los nuevos tipos de datos.

Adjunto codigo en el cual se puede ver un ejemplo de como escribir usando Generics una clase que nos permita serializar cualquier tipo de datos.

En primer lugar definimos un tipo de datos Bit que será la representación de nuestra serialización.
Después creamos la clase Serialize que será la interfaz externa y visible. Ademas debemos definir la clase GSerialize que será la clase en la que definiremos usando Generics los comportamientos deseados según sea la forma de nuestro tipo de datos. Por último dentro de Serialize debemos incluir una cláusula default que es la que enlaza la función put en Serialize con la función gput en GSerialize.

```haskell
    data Bit = O | I
 
    class Serialize a where
      put :: a -> [Bit]
      default put :: (Generic a, GSerialize (Rep a)) => a -> [Bit]
      put a = gput (from a)
      
    class GSerialize f where
      gput :: f a -> [Bit]
      
```
Despues definimos las diferentes instancias de la clase GSerialize para los 5 combinadores nombrados con anterioridad.
```haskell
    instance GSerialize U1 where
      gput U1 = []
      
    instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
      gput (a :*: b) = gput a ++ gput b
      
    instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
      gput (L1 x) = O : gput x
      gput (R1 x) = I : gput x
      
    instance (GSerialize a) => GSerialize (M1 i c a) where
      gput (M1 x) = gput x
      
    instance (Serialize a) => GSerialize (K1 i a) where
     gput (K1 x) = put x
```

Por último debemos definir algunas instancias para algunos tipos predefinidos básicos.
```haskell 
    instance Serialize Int where
      put i = serializeInt i
    
    instance Serialize Bool where
      put True = [I]
      put False = [O]
 
    instance Serialize a => Serialize [a] where
      put []    = []
      put (h:t) = put h ++ put t
```

### 2.4 Template Haskell
En este apartado trataremos sobre Template Haskell [@template_haskell], una extensión sobre el lenguaje original que añade la posibilidad de realizar metaprogramación en Haskell, de una manera similar al sistema de templates de C++, de ahí su nombre, permitiendo a los programadores computar parte de la generación de código en tiempo de compilación dependiendo de las necesidades.

##### Un ejemplo de la idea básica
Imaginemos que escribir una funcion para imprimir un valor en Haskell siguiendo el estilo de C. Nos gustaría poder escribir algo como esto en Haskell:
```haskell
    printf ”Error: %s on line %d.” msg line
```
El caso es que en Haskell uno no puede definir printf de una manera tan sencilla pues el tipo de printf depende del valor de su primer argumento. En Template Haskell en cambio podemos definir printf de manera que sea definido por el usuario, eficiente y garantice la seguridad de tipos de Haskell.

```haskell
    $(printf ”Error: %s on line %d”) msg line
```
El símbolo $ indica "evaluar en tiempo de compilación". La llamada a la función printf devuelve a Haskell una expresión que es insertada en el lugar de la llamada después de lo cual se puede realizar la compilación de la expresión. Por ejemplo el codigo entre parentesis:

```haskell
    $(printf ”Error: %s on line %d”)
```
 lo traduce en la siguiente expresión lambda en Haskell
```haskell
    (\ s0 -> \ n1 -> ”Error: ” ++ s0 ++ ” on line ” ++ show n1)
```
Sobre la cual se aplicará la comprobación de tipos y se aplicará sobre *msg* y *line*

##### Como usar template Haskell

Lo primero que hay que resaltar es el hecho que las funciones de Template Haskell que son ejecutadas en tiempo de compilación estan escritas en el mismo lenguaje que las funciones utilizadas en tiempo de ejecución. Una gran ventaja de esta aproximación es que todas las librerías existentes y técnicas usadas en Haskell pueden ser utilizadas directamente en Template Haskell. Por otro lado, una de las posibles desventajas de esta aproximación puede ser la necesidad de tener que utilizar notaciones como "$" o "[||]" (conocidas como *splicing* y *quasi-quotes*) para especificar que partes del codigo se deben ejecutar en tiempo de ejecución y cuales en tiempo de compilación.

En los ejemplos más sencillos como el anteriormente presentado sobre como escribir una función printf en Template Haskell la notación del *splicing* o la *quasi-quotation* pueden resultar de gran ayuda. El problema es que tan pronto como empezamos a hacer cosas más complejas en meta-programación esta notación deja de ser suficiente. Por ejemplo no es posible definir una función para seleccionar el i-ésimo elemento de una tupla de n elementos usando solo esas dos notaciones. Dicha función en Template Haskell sería asi.

```haskell
    sel :: Int -> Int -> ExpQ
    sel i n = [| \x -> $(caseE [| x |] [alt]) |]
        where alt :: Match
              alt = simpleM pat rhs

              pat :: PatQ
              pat = ptup (map pvar as)

              rhs :: ExpQ
              rhs = var (as !! (i-1))

              as :: [String]
              as = ["a" ++ show i | i <- [1..n]]
```

Para explicar un poco este código vamos a empezar de abajo a arriba, para entender las partes que usaremos despues en la función principal **sel**.
En primer lugar **as** lo que hace es crear una lista de nombres de aes desde a1 hasta an. La segunda de ellas, rhs se encarga de coger el i-esimo elemento de la lista de aes y devolverlo como una variable de tipo **ExpQ** que es el tipo utilizado en Template Haskell para las expresiones. La función **pat** transforma en primer lugar la lista de **Strings** en una lista de variables de tipo **PatQ** que es el tipo utilizado en TH para referirse a los patrones y después junta dicha lista en una tupla de tipo **PatQ**. Después al realiza un emparejamiento de la tupla tipo **PatQ** con el rhs mediante la funcion **simpleM** (simple Match).
Finalmente la función **caseE** que toma como parametros una variable x (de tipo **ExpQ** como indica la quasi-quotation alrededor de x) y el emparejamiento devuelto por alt,realizando la sustituición de la x al lado izquierdo de la flecha por el patrón correspondiente y colocando al lado izquierdo de la flecha la **ExpQ** devuelta por **rhs** que es elemento de la tupla tomado.

Esta función se traduciria a una expresión lambda que por ejemplo si llamaramos a **sel 4 6** es decir el cuarto elemento de una tupla de 6 tendría esta forma
```haskell
    (\(a1,a2,a3,a4,a5,a6) -> a4)
```

##### Cosificación (Reification)
La *cosificación* es la herramienta presente en Template Haskell para permitir al programador preguntar sobre el estado de la tabla de símbolos que guarda el compilador. Por ejemplo se puede escribir un código como el siguiente:

```haskell
  module M where
  data T a = Tip a | Fork (T a) (T a)
  
    repT :: Decl
    repT = reifyDecl T

    lengthType :: Type
    lengthType = reifyType length

    percentFixity :: Q Int
    percentFixity = reifyFixity (%)

    here :: Q String
    here = reifyLocn
```

La primera de las funciones declaradas devuelve un resultado de tipo **Decl** (equivalente a **Q Dec**), representando la declaración del tipo T. El siguiente computo **reifyType length** devuelve un resultado de tipo **Type** (equivalente a **Q Typ**) representando el conocimiento del compilador sobre el tipo de la función **length**.
En tercer lugar **reifyFixity** devuelve la ¨fixity¨ de los argumentos de la función lo cual es muy util cuando se quiere deducir como imprimir algo. Finalmente **reifyLocn** devuelve un resultado de tipo **Q String** que representa la posición en el código fuente desde donde se ejecutó **reifyLocn**.

De esta manera la cosificacion devuelve un resultado que puede ser analizado y utilizado en otros cálculos, pero hay que recordar que al tratarse de una herramienta del lenguaje para acceder a la tabla de símbolos y estar encapsulado dentro de la monada Q no puede ser usada como una funcion, por ejemplo con la función map (map reifyType xs sería incorrrecto).


## 3. Las clases Allv, Sized, Arbitrary

### 3.1: Black box testing en nuestro contexto

En cuanto al mundo del testing exiten dos grandes posibilidades, sistemas de caja negra o sistemas de caja blanca. En primer lugar los de caja negra son aquellos sistemas de testing que no profundizan en la estructura interna si no que trabajan unicamente con la entrada sobre la que aplican una precondición y la salida sobre la que comprueban si cumple las postcondiciones establecidas. 
En cambio los de caja blanca testean no solo las entradas y salidas del programa aplicandoles precondiciones y comprobando la postcondiciones si no que además profundizan en la estructura interna del programa para realizar algunas pruebas internas sobre por ejemplo estructuras de datos o el metodos auxiliares del mismo.

En el caso de nuestro proyecto nos decidimos por el método de caja negra pues queríamos conseguir un sistema válido para poder probar cualquier programa sin necesidad de tener que adaptar nuestra plataforma para cada nuevo programa, es decir que funcionase fuese cual fuese el programa bajo prueba. Esa es una de las desventajas del testeo de caja blanca que para poder comprobar partes de la estructura interna de un programa tienes que adaptar la plataforma para cada uno de los nuevos programas.

La idea principal detras de nuestro proyecto era principalmente la inmediatez y la comodidad del usuario, es decir que para probar un programa no necesitara escribir código extra aparte del ya existente programa si no que solo debe especificar como quiere que se generen los casos de prueba y los rangos de los dominios a usar y con eso ser ya capaz de probar su programa lo cual se ajusta mucho mas a la idea de testeo de caja negra.

### 3.2: Sized

### 3.3: Allv/TemplateAllv

En primer lugar vamos a tratar la clase **Allv**, cuyas instancias cuentan unicamente con una función, **allv** la cual devuelve la lista de todos los posibles valores del tipo de datos. 

En un primer lugar esta clase estaba pensada para utilizar la librería Generics para ser una única clase y para contar con un método **compose** con la cual ser capaces de generar instancias de la clase **Allv** para tipos definidos por el usuario. Dicha función se encargaría de crear la lista de todos los valores (**allv**) para el nuevo tipo de datos a partir de las listas de los tipos predefinidos, pero encontramos un problema a la hora de integrarlo con la clase **Sized**. La idea que teníamos sobre **Sized** era darle al usuario la posibilidad de pedir los n valores mas pequeños de una clase o los n primeros valores de tamaño menor o igual a un número prefijado por él.
Lo cual entraba en conflicto con la manera en la que generábamos las listas de **allv** para los tipos definidos por el usuario.
Para realizar la composición de dos listas seguimos un método de retículo como el que muestra la siguiente figura. 

//////////IMAGEN RETICULO///////////////////// 

Dadas dos listas a combinar vamos realizando la combinación de las listas por diagonales, asegurandonos asi de que aunque alguna de las dos listas o ambas sean infinitas no vamos a dejar ningun valor del nuevo tipo de datos sin calcular por el medio, el problema de generar la nueva lista por diagonales viene de que dicha lista no se encuentra ordenada por tamaño de sus elementos.
Dicha combinación de listas infinitas podía ser realizada sin problemas usando Generics pero el problema llegaba a la hora de querer devolver los n primeros valores de un tamaño menor o igual a m ya que para ello debiamos ordenar la lista infinita y encontramos el problema de que en dichas listas infinitas los elementos de un tamaño siempre eran infinitos y que siempre habría algun elemento a mayores de tamaño menor o igual a m aunque fuera despues de muchos elementos por el medio que no lo fueran. Dicho problema fue el por que tuvimos que pensar en utilizar **Template Haskell** en lugar de **Generics**.

En la versión definitiva del programa la clase **Allv** esta funcionalidad de crear una instancia de **Allv** para los tipos de datos definidos por el usuario es realizada en **TemplateAllv**, con la ayuda de la ya nombrada compose que tiene la siguiente forma.

```haskell
  compose ::[a] -> [b] -> [(a,b)]
  compose xs ys = concat $ diags 0 0 0 xs ys

  --
  -- It builds the lattice of tuples from two lists, each one may be either
  -- finite or infinite


  diags :: Int -> Int -> Int -> [a] -> [b] -> [[(a,b)]]
  diags _ _ _ [] [] = [[]]
  diags i dx dy xs ys
      | fullDiag     = [tup k | k <- [0..i]] : diags (i+1) dx dy xs ys
      | finiteFirst  = diags (i-1) dx     (dy+1) xs  ysr
      | finiteSecond = diags (i-1) (dx+1) dy     xsr ys
      | otherwise    = diags (i-2) (dx+1) (dy+1) xsr ysr

    where xs'          = drop i xs
          ys'          = drop i ys
          xsr          = tail xs
          ysr          = tail ys
          fullDiag     = not (null xs') && not (null ys')
          finiteFirst  = null xs' && not (null ys')
          finiteSecond = not (null xs') && null ys'
          tup k        = (x,y)
                         where x = xs !! k 
                               y = ys !! (i-k)
```

**Compose** se encarga de concatenar todas las diagonales en una única lista final que es la que se devuelve mediante la función **allv**, por otro lado **diags** se encarga de crear una de las diagonales y mientras no sea la ultima diagonal volver a llamarse a si misma con los parametros para la siguiente. Los parámetros de la función **diags** son:
  - *i* se trata del ordinal de la diagonal que vamos a generar.
  - *dx* y *dy* se tratan respectivamente de los desplazamientos en x e y, por ejemplo si dx es 3 significa que no se usarán en dicha diagonal los 3 primeros elementos de la lsta xs a la hora de combinar.
  - *xs* e *ys* se tratan de las dos listas que vamos a combinar.

La otra parte de **TemplateAllv** que se encarga de investigar el tipo de datos del usuario y adaptar la instancia de **Allv** de dicho tipo se divide en cuatro grandes pasos o funciones.

La primera de ellas **typeInfo** es la que se encarga de extraer la información del tipo declarado por el usuario. Se trata de una función que recibe como parametro una variable del tipo **DecQ** y devuelve una tupla dentro de la monada Q con la siguiente información:
  - En primer lugar el nombre simplificado del tipo del cual vamos a realizar la instancia, refiriendome con simplificado a quitar toda la parte del nombre que se refiere a la estructura de módulos de la cual se hereda dicho tipo. Por ejemplo si crearamos una instacia para el tipo **Integer** del módulo **Prelude** el nombre del tipo sin simplificar sería **Prelude.Integer** y una vez simplificado simplemente **Integer**. Dicho nombre se trata de una variable de tipo Name, que es la manera en la que se maneja el tipo **String** en **TH** para todo tipo de nombres.
  - El segundo se trata del nombre del tipo sin simplificar, devuelto tambien como una variable de tipo **Name**.
  - El tercero es una lista de enteros para cada uno de los diferentes constructores del tipo. Los enteros expresan el numero de argumentos de cada uno de uno de los constructores.
  - El cuarto se trata de una lista de los diferentes nombres de los constructores de tipo. Se trata de una lista de tipo **Name**
  - El último se trata de una lista de listas. Cada una de las listas internas contiene los tipos para uno de los constructores del tipo.

En el codigo a continuación muestro tanto el código para la función **typeInfo** como para la funcion **simpleName** que es la encargada de simplificar el nombre de la función.


```haskell
  typeInfo :: DecQ -> Q (Name, Name,[Int],[Name],[[Type]])
  typeInfo m =
       do d <- m
          case d of
             d@(DataD _ _ _ _ _) ->
              return $ (simpleName $ name d, name d , consA d, termsA d, listTypesA d)
             d@(NewtypeD _ _ _ _ _) ->
              return $ (simpleName $ name d, name d , consA d, termsA d, listTypesA d)
             _ -> error ("derive: not a data type declaration: " ++ show d)
   
       where
          consA (DataD _ _ _ cs _)    = map conA cs
          consA (NewtypeD _ _ _ c _)  = [ conA c ]

          conA (NormalC c xs)         = length xs
          conA (RecC c xs)            = length xs
          conA (InfixC _ c _)         = 2
   
          nameFromTyVar (PlainTV a)    = a
          nameFromTyVar (KindedTV a _) = a
   
          termsA (DataD _ _ _ cs _)   = map termA cs
          termsA (NewtypeD _ _ _ c _) = [ termA c ]
   
          termA (NormalC c xs)      = c
          termA (RecC c xs)         = c
          termA (InfixC t1 c t2)    = c
   
          name (DataD _ n _ _ _)      = n
          name (NewtypeD _ n _ _ _)   = n
          name d                      = error $ show d

          listTypesA (DataD _ _ _ cs _)    = (map typesA cs)
          listTypesA (NewtypeD _ _ _ c _)  = [ typesA c ]

          typesA (NormalC _ xs)         = map snd xs
          typesA (RecC _ xs)            = map (\(_, _, t) -> t) xs
          typesA (InfixC t1 _ t2)       = [snd t1] ++  [snd t2]
   
  simpleName :: Name -> Name
  simpleName nm =
     let s = nameBase nm
     in case dropWhile (/=':') s of
          []          -> mkName s
          _:[]        -> mkName s
          _:t         -> mkName t
```





## 6. Related

### 6.1: Korat

La primera de las herramientas que vamos a tratar en este apartado se trata de Korat [@korat], una extensión de Java que sirve para la generación de casos complejos de prueba a partir de unas restricciones dadas.

La idea detras de Korat es que dado un predicado en Java y una funcion **finitialization** en la cual definimos los dominios para cada uno de las clases del input, es decir los valores válidos para cada una de ellas. Korat explora el espacio de estados de las posibles soluciones pero generando solo soluciones no-isomorficas entre si, consiguiendo de esta manera una gran poda de las soluciones no interesantes del espacio de busqueda.

##### Espacio de estados
Lo primero que hace Korat es reservar el espacio necesario para los objetos especificados por ejemplo en el caso de un BinTree reservaria espacio para un BinTree y para el número de Nodos que queramos. Por ejemplo si queremos un arbol con tres nodos el vector contendría 8 campos:
* 2 para el BinTree (uno para la raíz y otro para el tamaño)
* 2 campos por cada uno de los 3 nodos (hijo izquierdo/hijo derecho)

Cada uno de los posibles candidatos que considere Korat a partir de ese momento será una evaluación de esos 8 campos. Por lo tanto el espacio de estados de busqueda del input consiste en todas las posibles combinaciones de esos campos, donde cada uno de ellos toma valores de su dominio definido en **finitialization**

##### Proceso de busqueda

Para conseguir explorar de manera sistemática y completa el espacio de estados Korat ordena todos los elementos en los dominios de las clases y los dominios de los campos. Dicho orden de cada uno de los dominios de los campos será consistente con el orden del dominio de la clase y todos los valores que pertenezcan al mismo dominio de clase ocurriran de manera consecutiva en el dominio del campo.

Cada uno de los candidatos de la entrada es un vector de indices de su correspondiente dominio del campo. Teniendo en cuenta que el dominio de la clase Nodo en el anterior ejemplo cuenta con 3 elementos [N0,N1,N2] a estos debemos añadir null obteniendo asi un dominio de campo [null, N0, N1, N2] que será el dominio de los campos raiz e hijos derecho e izquierdo de cada uno de los nodos. El dominio del campo tamaño será un único entero, 3.

Tras definir los dominios de cada uno de los campos del vector la busqueda comienza con la inicialización de todos los indices del vector a cero. Tras ello para cada posible candidato fijamos los valores de los campos de acuerdo a los valores en el vector y acto seguido invoca a la funcion repOk que es donde el usuario ha definido la precondición de la función. Durante dicha ejecución Korat monitoriza en que orden son accedidos los campos del vector y construye una lista con los identificadores de los campos, ordenados por la primera vez en que repOk los accede.

Cuando repOk retorna Korat genera el siguiente candidato incrementando el indice del dominio de campo para el campo que se encuentra último en la lista ordenada construida previamente. Si dicho indice es mayor que el tamaño del dominio de su campo este se pone a cero y se incrementa el indice de la posición anterior y asi repetidamente.

Al seguir este método para generar el siguiente candidato conseguiremos podar un gran numero de los candidatos que tienen la misma evaluación parcial. Asimismo podemos estar seguros de que dicha poda no deja fuera ninguna estructura de datos válida porque repOk no leyó dichos campos y podría haber devuelto falso independientemente de su valor. Gracias a esta poda de una gran parte del espacio de busqueda Korat puede explorar espacios de busqueda muy grandes de manera eficiente, dicha eficiencia de la poda depende del método repOk por ejemplo si siempre lee todos los datos de entrada antes de terminar la ejecución forzará a Korat a explorar casi todo el espacio de busqueda sin apenas podas. 

El algoritmo de busqueda descrito aquí genera las entradas en orden lexicográfico. Ademas para los casos en los que repOk no es determinista este método garantiza que todos los candidatos para los que repOk devuelve true son generados, los casos para los que repOk siempre devuelve false nunca son generados y los casos para los que alguna vez se devuelve true y a veces false pueden ser o no generados.

##### Resultados no-isomorfos
Dos candidatos se definen como isomorfos si las partes de sus grafos alcanzables desde la raiz son isomorfas. En el caso de repOk el objeto raiz es el objeto pasado como argumento implicito.

El isomorfismo entre candidatos divide el espacio de estados en particiones isomórficas (debido al ordenamiento lexicográfico introducido por el orden de los valores de los dominios de los campos y la ordenación de los campos realizado por repOk). Para cada una de dichas particiones isomomorficas Korat genera únicamente el candidato lexicográficamente menor.

Ademas con el proceso explicado anteriormente para ir generando el siguiente candidato teniendo en cuenta la lista de ordenación de los campos Korat se asegura de no generar varios candidatos dentro de la misma partición isomórfica.

### 6.2: Smallcheck

La segunda herramienta a tratar en este apartado es Smallcheck [@smallcheck] una librería para Haskell usada en el testing basado en propiedades. Esta librería parte de las ideas del Quickcheck y perfecciona algunos de los puntos flacos de este.

#####Generación de valores pequeños
La principal diferencia de Smallcheck respecto a Quickcheck se trata de la generación de sus casos de prueba. En este caso Smallcheck se apoya en la "hipótesis del ámbito pequeño" la cual dice que si un programa no cumple su especificación en alguno de sus casos casi siempre existirá un caso simple en el cual no la cumpla o lo que viene a ser lo mismo, que si un programa no falla en ninguno de sus casos lo normal es que no falle en ninguno de sus casos.

Partiendo de esta idea cambia la generación existente en Quickcheck, la cual era aleatoria, por una generación exhaustiva de todos los casos de prueba pequeños ordenados por *profundidad* (que es el nombre usado para el tamaño), dejando a criterio del usuario hasta que profundidad deben considerarse como pequeños. A continuacion les presento como están definidas las profundidades más importantes:

- Dicha profundidad suele ser facilmente definida para los valores de los tipos de datos algebraicos. Como es usual, para los términos algebraicos, la profundidad de una construcción de aridad cero es cero mientras que la profundidad de una construcción de aridad positiva es una más que la mayor profundidad de todos sus argumentos.

- En el caso de las tuplas dicha profundidad se define un poco diferente. La profundidad de una tupla de aridad cero es cero pero la profundidad de una tupla de aridad positiva es simplemente la mayor profundidad de entre todas las de sus componentes.

- En el caso de los tipos numéricos la definición de la profundidad es con respecto a una representación imaginaria como una estructura de datos. De esta manera, la profundidad de un entero *i* será su valor absoluto, ya que se construyó de manera algebraica como **Succ^i^ Zero**. A su vez, la profundidad de un numero decimal **s x 2^e^** es la profundidad del par de enteros (s,e).

##### Tipos de datos seriales
Smallcheck define una clase **Serial** de tipos que pueden ser enumerados hasta una determinada profundidad. Para todos los tipos de datos del preludio existen instancias predefinidas de la clase Serial. Sin embargo definir una nueva instancia de la clase Serial para un tipo de datos algebraico es muy fácil, se trata de un conjunto de combinadores *cons<N>*, genericos para cualquier combinación de tipos Serial, donde <N> es la aridad del constructor.

Por ejemplo supongamos un tipo de datos en Haskell **Prop** en el cual tenemos una variable, la negación de una variable y el Or de dos variables

```haskell
  data Prop = Var Name | Not Prop | Or Prop Prop
```

Definir una instancia de la clase Serial para dicho tipo de datos sería como sigue. Asumiendo una definición similar para el tipo **Name**. 

```haskell
  instance Serial Prop where
    series = cons1 Var \/ cons1 Not \/ cons2 Or
```
Una serie simplemente es una funcion que dado un entero devuelve una lista finita.

```haskell
  type Series a = Int -> [a]
```

A su vez el producto y la suma sobre dos series se definen como:
```haskell
  (\/) :: Series a -> Series a -> Series a
  s1 \/ s2 = \d -> s1 d ++ s2 d

  (><) :: Series a -> Series b -> Series (a, b)
  s1 >< s2 = \d -> [(x,y) | x <- s1 d, y <- s2 d]
```
 Por último los combinadores cons<N> están definidos usando **><** decrementando y comprobando la profundidad correctamente.

 ```haskell
  cons0 c = \d -> [c]
  cons1 c = \d -> [c a | d > 0, a <- series (d-1)]
  cons2 c = \d -> [c a b | d > 0, (a,b) <- (series >< series) (d-1)]
 ```


#####Trabajando con grandes espacios de busqueda.
Usar el esquema general para definir series de valores de prueba muchas veces produce que para alguna profundidad pequeña **d** los *10.000-100.000* casos de prueba son rapidamente comprobados pero para la profundidad **d+1** resulte imposible completar los miles de millones de casos de prueba. Por ello resulta necesario reducir algunas dimensiones del espacio de busqueda de manera que otras de las dimensiones puedan ser comprobadas en mayor profundidad.

El primer punto a tener en cuenta es que a pesar de que los numeros enteros pueden parecer una elección obvia como valores bases para las pruebas debemos tener en cuenta que los espacios de busqueda para los tipos compuestos (especialmente funcionales) al usar bases numericas crecen de manera muy rápida. Para muchas propiedades Bool puede ser también una eleccion perfectamente valida con lo que conseguiriamos reducir en gran medida el espacio de busqueda.

#####Lazy SmallCheck
Se trata de una versión de *Smallcheck* pero que a su vez se aprovecha de la evaluación perezosa de Haskell, la cual permite que una función devuelva un valor aunque esta esté aplicada sobre una entrada definida parcialmente. Esta posibilidad de ver el resultado de una función sobre muchas entradas en una sola ejecución puede resultar de gran ayuda en el testeo basado en propiedades ya que si una función se cumple para una solución parcial, esta se cumplirá para todas las funciones totalmente definidas que partan de dicha definición parcial. En eso se centra el *Lazy Smallcheck*, en evitar generar todas esas funciones totalmente definidas que no aportan nada de información extra sobre la definición parcial. La actual versión de *Lazy Smallcheck* es capaz de testear propiedades de primer orden con o sin cuantificadores universales.

Uno de los problemas de *Lazy Smallcheck* es que en algunos casos deberemos tener en cuenta en que orden ponemos las condiciones de la función que queremos probar pues segun sea dicho orden los resultados pueden ser distintos. Por ejemplo vamos a tomar un ejemplo en el cual queremos testear la funcion **isOrderedSet** la cual aplicada sobre una lista nos devuelve si esta es un conjunto y a su vez está ordenada o no.

```haskell
  isOrderedSet s = ordered s && allDiff s

  allDiff [] = True
  allDiff (x:xs) = x `notElem` xs && allDiff xs

  ordered [] = True
  ordered [x] = True
  ordered (x:y:zs) = x <= y && ordered (y:zs)
```

Queremos a su vez comprobar la propiedad *prop_insertOrderedSet* la cual comprueba si dado un conjunto ordenado despues de insertar un nuevo elemento en el este sigue siendo un conjunto ordenado.

```haskell
  prop_insertOrderedSet c s = isOrderedSet s ==> isOrderedSet (insert c s)
```

Al ejecutar *Lazy Smallcheck* sobre esta última propiedad obtenemos los siguientes resultados:
```haskell
  Main> depthCheck 7 prop_insertOrderedSet
  OK, required 964 tests at depth 7
```
En cambio si cambiamos de orden las dos partes de la conjunción de la función *isOrderedSet* objenemos los siguientes resultados.

```haskell
  Main> depthCheck 7 prop_insertOrderedSet
  OK, required 20408 tests at depth 7
```

Esta diferencia de veinte veces más tests necesarios es debido a que el operador "&&" evalua primero la parte izquierda y *ordered* es más restrictivo que *allDifferent* por lo cual en el primer ejemplo muchos de los ejemplos no se llegan a comprobar por no cumplir *ordered* mientras que en el segundo ejemplo son muchos mas los casos de prueba necesarios por ser *allDifferent* menos restrictiva.

Por suerte existe un nuevo operador en *Lazy Smallcheck*, la conjunción parelela *\*&\**, la cual es falsa si cualquiera de sus operandos lo son y que resulta de gran utilidad tanto para conseguir que el programa necesite menos casos de prueba como para que no sea importante el orden en el que esten escritos las condiciones de la precondición.

##References