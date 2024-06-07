package parser

import model.Operador
import model.Operador.{LAMBDA, PUNTO, PAREN_IZQ, PAREN_DER, ESPACIO}
import model.expresion.{Variable, Abstraccion, Aplicacion, VarLibres}
import model.expresion.Expresion

def encontrarAplicacion(ecuacion: List[Operador | String], aplicaciones: Int, index: Int): Int = {
    ecuacion match {
      case x :: xs =>
        x match {
          case Operador.ESPACIO if aplicaciones == 0 => index
          case Operador.PAREN_IZQ => encontrarAplicacion(xs, aplicaciones + 1, index + 1)
          case Operador.PAREN_DER => encontrarAplicacion(xs, aplicaciones - 1, index + 1)
          case _ => encontrarAplicacion(xs, aplicaciones, index + 1)
        }
    }
}

def parserEcuacion(ecuacion: List[Operador | String]): Expresion = {
  ecuacion match {
    case Nil => Variable("")
    case (s: String) :: xs => Variable(s)
    case Operador.LAMBDA :: (nombre: String) :: Operador.PUNTO :: xs =>
      val variable = Variable(nombre)
      Abstraccion(variable, cuerpo = parserEcuacion(xs))
    case Operador.PAREN_IZQ :: xs =>
      val indexSeparacion = encontrarAplicacion(xs.init, 0, 1)
      val (lista1, lista2) = xs.splitAt(indexSeparacion)
      Aplicacion(funcion = parserEcuacion(lista1), argumento = parserEcuacion(lista2))
  }
}

def parserArbol(expresion: Expresion): List[Operador | String] = {
  expresion match {
    case Variable(v) => List(v)
    case Abstraccion(e1,e2) => Operador.LAMBDA :: e1.nombre :: Operador.PUNTO :: parserArbol(e2)
    case Aplicacion(e1,e2) => Operador.PAREN_IZQ :: parserArbol(e1) ::: List(Operador.ESPACIO) ::: parserArbol(e2) ::: List(Operador.PAREN_DER)
    case VarLibres(variables) => List(s"{${variables.mkString(",")}}")
  }
}
