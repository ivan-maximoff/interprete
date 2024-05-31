package lector

import modelo.Operador

import scala.annotation.tailrec

def leerEcuacion(ecuacion: String): List[Operador | String] = {
    ecuacion.map(x => stringToToken(x.toString)).toList
}

def stringToToken(string : String): Operador | String = {
    string match {
        case "λ" => Operador.LAMBDA
        case " " => Operador.ESPACIO
        case "." => Operador.PUNTO
        case "(" => Operador.PAREN_IZQ
        case ")" => Operador.PAREN_DER
        case _ => string
    }
}

def leerTokens(tokens: List[Operador | String]): String = {
    tokens.map(x => tokenToString(x)).mkString
}

def tokenToString(token : Operador | String): String = {
    token match {
        case Operador.LAMBDA => "λ"
        case Operador.ESPACIO => " "
        case Operador.PUNTO => "."
        case Operador.PAREN_IZQ => "("
        case Operador.PAREN_DER => ")"
        case s : String => s
    }
}