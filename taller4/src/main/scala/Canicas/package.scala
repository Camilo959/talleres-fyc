package object Canicas {

  type Frasco = (Int, Int)
  type Distr = List[Frasco]

  def canicasPosiblesFrasco(f: Int, c: Int): List[Frasco] = {
    (
      for (i <- 0 to c)
      yield (f, i)
    ).toList

  }

  def canicasPorFrasco(n: Int, c: Int): List[Distr] = {
    (
      for(i <- 1 to n)
      yield canicasPosiblesFrasco(i,c)
    ).toList

  }

  def mezclarLCanicas(lc: List[Distr]): List[Distr] = {

    if (lc.isEmpty)
      List(List())

    else {

      val primera = lc.head
      val resto = lc.tail

      for {
        f <- primera
        mezcla <- mezclarLCanicas(resto)
      } yield f :: mezcla

    }
  }

  def distribucion(m: Int, n: Int, c: Int): List[Distr] = {
    ???
  }

  def agrupaciones(m: Int): List[List[Int]] = {
    ???
  }

}
