import Recursion.{potencia, pascal, balanceado, cambioMonedas}

// Pruebas Potencia
potencia(2,3)       //  8
potencia(5,0)       //  1
potencia(3,4)       //  81
potencia(10,1)      //  10
potencia(1,100)     //  1
potencia(2,10)      //  1024


// Pruebas Pascal
pascal(0,0)      // 1 (esquina superior)
pascal(0,2)      // 1 (borde izquierdo)
pascal(2,2)      // 1 (borde derecho)
pascal(1,2)      // 2 (interior)
pascal(1,3)      // 3 (interior)
pascal(2,4)      // 6 (interior)
pascal(3,5)      // 10 (interior)

def filaTriangulo(fila: Int) =
  for (col <- 0 to fila) yield pascal(col, fila)

def triangulo(filas: Int) =
  for (fila <- 0 to filas)
    yield filaTriangulo(fila)

triangulo(0)
triangulo(1)
triangulo(2)
triangulo(3)
triangulo(4)


// Pruebas Balanceado
balanceado("(if (esCero x) max (/ 1 x))".toList)
balanceado("Yo se lo dije (que no está (aún) listo). (Pero él no estaba escuchando)".toList)
balanceado(":-)".toList)
balanceado("())(".toList)
balanceado("(solo un) ejemplo".toList)
balanceado("".toList)

// Pruebas Cambio de monedas
cambioMonedas(4, List(1,2))
assert (cambioMonedas(4, List(1,2)) == 3)

cambioMonedas(300, List(5, 10, 20, 50, 100, 200, 500))
assert (cambioMonedas(300, List(5, 10, 20, 50, 100, 200, 500)) == 1022)

cambioMonedas(301, List(5, 10, 20, 50, 100, 200, 500))
assert (cambioMonedas(301, List(5, 10, 20, 50, 100, 200, 500)) == 0)

cambioMonedas(300, List(500, 5, 50, 100, 20, 200, 10))
assert (cambioMonedas(300, List(500, 5, 50, 100, 20, 200, 10)) == 1022)
