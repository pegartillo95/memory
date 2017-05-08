Introduction
=============

There has always been a controversy about the relative bug detecting power of black-box cases with
respect to white-box ones. White-box supporters argue that the different covering criteria ensure that
every corner of the UUT has been exercised at least once, while the black-box strategy does not offer
such a guarantee. But, as reported in [5], for small UUTs, black-box achieves the same covering degree
as white-box by executing a similar, or perhaps a little higher, number of test-cases. In this work, we have
decided to try the black-box approach, and within this option we faced choosing between doing random
test-case generation, as it is done for instance in QuickCheck [5, 11], or trying other possibilities.  
&nbsp;&nbsp;&nbsp;&nbsp;QuickCheck is a nice tool, but it requires some cooperation from the user in order to control the
generation and the distribution of the random cases. It provides a Haskell type class Arbitrary, with
a method arbitrary which generates arbitrary values of the type. Several instances of this class are
also provided for most common Haskell types. For user-defined ones, an instance of the class must
be created by the programmer. In order to facilitate this task, QuickCheck offers different primitive
methods, combining which the user can easily build a random generator for the new type.  
&nbsp;&nbsp;&nbsp;&nbsp;Following the approach of the Korat tool [3], we found attractive the idea of doing “exhaustive”
testing. For a given type, this consists of generating all the values of the type up to a given size. The
rationale behind this kind of testing is that, in general, small cases have a higher probability of detecting
bugs, and there are no a priori reasons why a normal program —not written on purpose to fail for specific
inputs— may properly work for small sizes and stop doing it for bigger ones.  
&nbsp;&nbsp;&nbsp;&nbsp;In [3], a notion of “size” and a way of generating values of different sizes are required for every user-
defined type. Some parts of these definitions can be automatically generated from the type declaration,
but in general the approach requires some extra work to be done by programmers.  
&nbsp;&nbsp;&nbsp;&nbsp;Our goal is to do exhaustive testing without requiring any extra work from programmers. The ex-
perience tells us that the more the programmer has to work for doing testing, the less testing will be
done. We have defined a type class Sized with three methods, one for computing the size of a value,
one for obtaining all the values from 0-sized to n-sized ones, for a given size n, and one for obtaining the
n smallest values of the type:
```haskell
	class Arbitrary a => Sized a where
	size :: a -> Int
	sized :: Int -> [a]
	smallest :: Int -> [a]
```

We have done Arbitrary to be a subclass of Sized in order to also be able to use random generation
for any type, should the user require that.  
&nbsp;&nbsp;&nbsp;&nbsp;Then, if the UUT has m arguments of types t1,...,tm, and the user specifies sizes n1,...,nm for them,
the test driver will generate N1×···×Nm tuples of values, being Ni
the number of values of type ti having
a size less than or equal to ni
, for 1 ≤ i ≤ m. Alternatively, the user can specify ni
to be the number of
cases for a particular argument. Then, these will be the ni smallest values of the type. Finally, he/she
may specify ni
to be the number of random cases desired for a particular argument.  
&nbsp;&nbsp;&nbsp;&nbsp;For every user-defined type T, one needs an instance of the class Sized for T. We have used the
GHC library Generics[^1]
to automatically derive instances of Sized and Arbitrary for every user-defined
type. The programmer must add deriving Generic to the definition of T, and the following single line:
```haskell
instance Sized T where
```
The user could alternatively define his/her own instances, should not the automatically provided ones be
satisfactory, but we expect this to happen rarely.  
&nbsp;&nbsp;&nbsp;&nbsp;As it is done by QuickCheck and Korak, our system uses the UUT precondition to filter the generated
test-cases: only those satisfying it will actually be executed. Should this number be less than 10% of the
generated cases, an error will be raised and the system will stop asking the user for new instructions. The
tool is still under construction, and we cannot report for the moment any experimental results.



[^1]:https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.9.0.0/GHC-Generics.html.