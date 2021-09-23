package co.s4n.calnat
import scala.io.StdIn

object Main extends App {
  def leerInt(prompt: String): Int = {
    val s = StdIn.readLine(prompt)
    s.toInt
  }

  def esCero(nat: Nat): Boolean = nat match {
    case Cero() => true
    case Suc(nat) => false
  }

  //Adicionales
  def sumaNat(n: Nat): Nat = Suc(n)
  def restaNat(nat: Nat): Nat = n match {
    case Cero() => Cero()
    case Suc(nat) => nat.nat
  }

  def conIntANat(i: Int): Nat = {
    def iConIntANat(i: Int, r: Nat): Nat = i match {
      case 0 => r
      case i => iConIntANat(i - 1, Suc(r))
    }

    iConIntANat(i, Cero())
  }

  def imprimirNat(nat: Nat): String = {
    def iImprimirNat(nat: Nat, s: String, numPar: Int): String = esCero(nat) match {
      case true => s + "Cero" + ")" * numPar
      case false => iImprimirNat(nat.nat, s + "Suc(", numPar + 1)
    }

    iImprimirNat(nat, "", 0)
  }

  def sumaNat(nat1: Nat, nat2: Nat): Nat = {
    if (esCero(nat1))
      nat2
    else {
      sumaNat(restaNat(nat1), sumaNat(nat2))
    }
  }

  //Leer dos números
  val num1 = leerInt("Leer primer entero ")
  val num2 = leerInt("Leer segudo entero ")

  //Convertir Integer a Nat
  val entero1 = conIntANat(num1)
  val entero2 = conIntANat(num2)

  //Sumar los dos enteros
  val sumNat = sumaNat(entero1, entero2)

  //Imprimir resultado
  println(imprimirNat(sumNat))
}

