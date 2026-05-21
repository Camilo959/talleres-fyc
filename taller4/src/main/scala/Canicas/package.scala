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
    ???
  }

  def mezclarLCanicas(lc: List[Distr]): List[Distr] = {
    ???
  }

  def distribucion(m: Int, n: Int, c: Int): List[Distr] = {
    ???
  }

  def agrupaciones(m: Int): List[List[Int]] = {
    ???
  }

}
