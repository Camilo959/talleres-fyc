import Recursion.{potencia, pascal, balanceado, cambioMonedas}

// Pruebas Potencia
potencia(2,3)
potencia(5,0)
potencia(0,5)
potencia(0,0)
potencia(-2,4)
potencia(-2,3)
potencia(1,100)
potencia(-1,101)

// Pruebas Pascal
def filaTriangulo(fila: Int) =
  for (col <- 0 to fila) yield pascal(col, fila)

def triangulo(filas: Int) =
  for (fila <- 0 to filas)
    yield filaTriangulo(fila)

triangulo(0)
triangulo(1)
triangulo(2)
triangulo(3)

// Pruebas Balanceado

balanceado("(if (esCero x) max (/ 1 x))".toList)
balanceado("Yo se lo dije (que no está (aún) listo). (Pero él no estaba escuchando)".toList)
balanceado(":-)".toList)
balanceado("())(".toList)
balanceado("(solo un) ejemplo".toList)