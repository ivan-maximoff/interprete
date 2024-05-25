package lector

import modelo.Operador

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