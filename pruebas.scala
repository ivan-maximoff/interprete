// es todo el codigo junto para ir probandolo

enum Operador {
  case LAMBDA, PUNTO, PAREN_IZQ, PAREN_DER, ESPACIO
}

sealed trait Expresion

case class Variable(nombre: String) extends Expresion
case class Abstraccion(variable: Variable, cuerpo: Expresion) extends Expresion
case class Aplicacion(funcion: Expresion, argumento: Expresion) extends Expresion

import scala.annotation.tailrec

def leerEcuacion(ecuacion: String): List[Operador | String] = {
    val ops = ecuacion.split("").toList
    _leerEcuacionRec(ops)
}

private def _leerEcuacionRec(tokens: List[String]): List[Operador | String] = tokens match {
    case Nil => List()
    case x::xs => {
        val op = x match {
            case "λ" => Operador.LAMBDA
            case " " => Operador.ESPACIO
            case "." => Operador.PUNTO
            case "(" => Operador.PAREN_IZQ
            case ")" => Operador.PAREN_DER
            case _ => x 
        }
        op::_leerEcuacionRec(xs)
    }
}

def encontrarAplicacion(ecuacion: List[Operador | String], aplicaciones: Int, index: Int): Int = {
    ecuacion match
      case x :: xs =>
         x match
          case Operador.ESPACIO if aplicaciones == 0 => index
          case Operador.PAREN_IZQ => encontrarAplicacion(xs, aplicaciones + 1, index + 1)
          case Operador.PAREN_DER => encontrarAplicacion(xs, aplicaciones - 1, index + 1)
          case _ => encontrarAplicacion(xs, aplicaciones, index + 1)
}

def interpretarEcuacion(ecuacion: List[Operador | String]): Expresion = {
  _interpretarEcuacion(ecuacion)
}

private def _interpretarEcuacion(ecuacion: List[Operador | String]): Expresion = {
  ecuacion match
    case Nil => Variable("")
    case (s: String) :: xs => Variable(s)
    case Operador.LAMBDA :: (nombre: String) :: Operador.PUNTO :: xs  =>
      val variable = Variable(nombre)
      Abstraccion(variable, cuerpo = _interpretarEcuacion(xs))
    case Operador.PAREN_IZQ :: xs =>
      val indexSeparacion = encontrarAplicacion(xs.init, 0, 1)
      val (lista1, lista2) = xs.splitAt(indexSeparacion)
      Aplicacion(funcion = _interpretarEcuacion(lista1), argumento = _interpretarEcuacion(lista2))
}