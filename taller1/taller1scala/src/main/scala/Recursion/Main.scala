import Recursion.balanceado as balanceado
import Recursion.potencia as potencia

// Archivo para probar las funciones
object Demo {

  def main(args: Array[String]): Unit = {
    
    println(potencia(2,3))
    println(potencia(5,0))
    println(potencia(0,5))
    
    println(balanceado("(if (esCero x) max (/ 1 x))".toList))     // true
    println(balanceado("Yo se lo dije (que no está (aún) listo). (Pero él no estaba escuchando)".toList))  // true
    println(balanceado(":-)".toList))                             // false
    println(balanceado("())(".toList))                             // false
    println(balanceado("(solo un) ejemplo".toList))               // true
  }
}