import ConjuntosDifusos.*

def muchoMayorQue(a: Int, m: Int): ConjDifuso = {
  def mma(x: Int): Double = {
    if (x <= a) 0.0
    else if (x > a && x <= m) (x - a).toDouble / (m - a).toDouble
    else 1.0
  }
  mma
}

def cercanosA(a: Int, k: Int): ConjDifuso = {
  def ca(x: Int): Double = {
    val q = 1.0 + k * (x - a) * (x - a)
    1.0 / q
  }
  ca
}

// -----------------------------
// Conjunto muchoMayorQue
// -----------------------------
val mm2 = muchoMayorQue(2, 6)

mm2(5)        // esperado: 0.75
mm2(3)        // esperado: 0.25
mm2(1)        // esperado: 0.0
mm2(2)        // esperado: 0.0

pertenece(3, mm2)  // esperado: 0.25
pertenece(8, mm2)  // esperado: 1.0


// -----------------------------
// Conjunto cercanosA
// -----------------------------
val ca3 = cercanosA(3, 4)

ca3(3)        // esperado: 1.0
ca3(4)        // esperado: 0.2
ca3(2)        // esperado: 0.2
ca3(5)        // esperado: ≈ 0.0588

pertenece(3, ca3)  // esperado: 1.0
pertenece(8, ca3)  // esperado: ≈ 0.0099


// -----------------------------
// Conjunto grande
// -----------------------------
val g1 = grande(1, 10)
val g2 = grande(2, 10)
val g3 = grande(20, 12)

g1(5)      // esperado: ≈ 0.1615
g1(10)     // esperado: ≈ 0.3855
g1(100)    // esperado: ≈ 0.9044

g2(5)      // esperado: ≈ 0.0346
g2(10)     // esperado: ≈ 0.1615
g2(100)    // esperado: ≈ 0.8171

g3(30)     // esperado: ≈ 0.00217
g3(31)     // esperado: ≈ 0.00287
g3(300)    // esperado: ≈ 0.4610


// -----------------------------
// Complementos
// -----------------------------
val cmm2 = complemento(mm2)
val cca3 = complemento(ca3)
val cg1 = complemento(g1)
val cg2 = complemento(g2)

cmm2(5)   // esperado: 0.25
cmm2(3)   // esperado: 0.75
cmm2(1)   // esperado: 1.0
cmm2(2)   // esperado: 1.0

cca3(3)   // esperado: 0.0
cca3(4)   // esperado: 0.8
cca3(2)   // esperado: 0.8
cca3(5)   // esperado: ≈ 0.9412

cg1(5)    // esperado: ≈ 0.8385
cg1(10)   // esperado: ≈ 0.6145
cg1(100)  // esperado: ≈ 0.0956

cg2(5)    // esperado: ≈ 0.9654
cg2(10)   // esperado: ≈ 0.8385
cg2(100)  // esperado: ≈ 0.1829


// -----------------------------
// Unión
// -----------------------------
val mm2_o_ca3 = union(mm2, ca3)

pertenece(3, mm2_o_ca3)  // esperado: max(0.25, 1.0) = 1.0
pertenece(8, mm2_o_ca3)  // esperado: max(1.0, ≈0.0099) = 1.0
pertenece(2, mm2_o_ca3)  // esperado: max(0.0, 0.2) = 0.2
pertenece(4, mm2_o_ca3)  // esperado: max(0.5, 0.2) = 0.5
pertenece(5, mm2_o_ca3)  // esperado: max(0.75, ≈0.0588) = 0.75

// -----------------------------
// Intersección
// -----------------------------
val mm2_y_ca3 = interseccion(mm2, ca3)

pertenece(3, mm2_y_ca3)  // esperado: min(0.25, 1.0) = 0.25
pertenece(8, mm2_y_ca3)  // esperado: min(1.0, ≈0.0099) ≈ 0.0099
pertenece(2, mm2_y_ca3)  // esperado: min(0.0, 0.2) = 0.0
pertenece(4, mm2_y_ca3)  // esperado: min(0.5, 0.2) = 0.2
pertenece(5, mm2_y_ca3)  // esperado: min(0.75, ≈0.0588) ≈ 0.0588


// -----------------------------
// Inclusión
// -----------------------------
inclusion(mm2_y_ca3, mm2_o_ca3)      // esperado: true
inclusion(mm2, ca3)                  // esperado: false
inclusion(ca3, mm2)                  // esperado: false
inclusion(mm2, muchoMayorQue(3, 7))  // esperado: false
inclusion(muchoMayorQue(3, 7), mm2)  // esperado: false


// -----------------------------
// Igualdad
// -----------------------------
igualdad(mm2_y_ca3, interseccion(ca3, mm2))  // esperado: true


// -----------------------------
// Propiedades adicionales
// -----------------------------
igualdad(union(mm2, mm2), mm2)                 // esperado: true
igualdad(interseccion(mm2, mm2), mm2)          // esperado: true
igualdad(complemento(complemento(mm2)), mm2)   // esperado: true
igualdad(mm2, mm2)                             // esperado: true
igualdad(mm2, ca3)                             // esperado: false