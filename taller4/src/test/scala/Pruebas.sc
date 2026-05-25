import Canicas._


// =====================================================
// PRUEBAS canicasPosiblesFrasco
// =====================================================

canicasPosiblesFrasco(1,0)

canicasPosiblesFrasco(2,3)

canicasPosiblesFrasco(5,2)

canicasPosiblesFrasco(3,1)

canicasPosiblesFrasco(4,5)


// =====================================================
// PRUEBAS canicasPorFrasco
// =====================================================

canicasPorFrasco(1,2)

canicasPorFrasco(2,1)

canicasPorFrasco(3,0)

canicasPorFrasco(2,2)

canicasPorFrasco(3,1)


// =====================================================
// PRUEBAS mezclarLCanicas
// =====================================================

mezclarLCanicas(
  List(
    List((1,0),(1,1))
  )
)

mezclarLCanicas(
  List(
    List((1,0),(1,1)),
    List((2,0),(2,1))
  )
)

mezclarLCanicas(List())

mezclarLCanicas(
  List(
    List((1,0)),
    List((2,0))
  )
)

val cpf1 = canicasPorFrasco(2,2)
mezclarLCanicas(cpf1)


// =====================================================
// PRUEBAS distribucion
// =====================================================

distribucion(3,1,3)

distribucion(0,2,2)

distribucion(2,2,2)

distribucion(1,2,1)

distribucion(10,3,5)


// =====================================================
// PRUEBAS agrupaciones
// =====================================================

agrupaciones(1)

agrupaciones(2)

agrupaciones(3)

agrupaciones(6)

agrupaciones(7)
// agrupaciones(10)