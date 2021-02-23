"Anonymous functions"


//Functional type/

val inc: Int => Int = i => i * i
inc(4)
//With explicit parameter
val inc1 = (i: Int) => i * i
//Placeholder syntax
val inc3 = (_: Int) + 1
//partially applied function syntax
def f(a: Int, b: Int) = a * b
val x = f(4, _)

val y = { a => f(a, a)}
y(5)