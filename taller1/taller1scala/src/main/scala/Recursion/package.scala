package object Recursion {

  /**
   *  Ejercicio 1
   */
  def potencia(m: Int, n:Int): Int = {
    if (n == 0) 1
    else m * potencia(m, n - 1)
  }


  /**
   * Ejercicio 2
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
   * Ejercicio 3
   */
  def balanceado(chars: List[Char]): Boolean = {

    def balanceadoAux(chars: List[Char], contador: Int): Boolean =
      if (contador < 0) false // cierre sin apertura
      else if (chars.isEmpty) contador == 0 // lista vacía → balanceado si contador = 0
      else if (chars.head == '(') balanceadoAux(chars.tail, contador + 1) // apertura → sumamos 1
      else if (chars.head == ')') balanceadoAux(chars.tail, contador - 1) // cierre → restamos 1
      else balanceadoAux(chars.tail, contador)    // cualquier otro → contador igual

    balanceadoAux(chars, 0) // empezamos con contador 0
  }


  /**
   * Ejercicio 4
   */
  def cambioMonedas(total: Int, denom: List[Int]): Int = {
    if (total == 0) 1                                 // Caso base: total = 0 → 1 forma
    else if (total < 0) 0                              // Caso base: total negativo → 0 formas
    else if (denom.isEmpty) 0                          // Caso base: no quedan monedas → 0 formas
    else
      // Sumar las combinaciones usando la primera moneda y las combinaciones sin usarla
      cambioMonedas(total - denom.head, denom) +      // usamos la primera moneda
        cambioMonedas(total, denom.tail)                // no usamos la primera moneda
  }

}