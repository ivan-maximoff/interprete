package lambda

def leerEcuacion(ecuacion: String): List[Operador | String] = {
    val ops = ecuacion.split("").toList
    _leerEcuacionRec(ops)
}

sealed trait Expresion

case class Variable(nombre: String) extends Expresion

case class Abstraccion(variable: Variable, cuerpo: Expresion) extends Expresion

case class Aplicacion(funcion: Expresion, argumento: Expresion) extends Expresion

enum Operador {
    case LAMBDA, PUNTO, PAREN_IZQ, PAREN_DER, ESPACIO
}

private def _leerEcuacionRec(tokens: List[String]): List[Operador | String] = tokens match {
    case Nil => List()
    case x :: xs => {
        val op = x match {
            case "λ" => Operador.LAMBDA
            case " " => Operador.ESPACIO
            case "." => Operador.PUNTO
            case "(" => Operador.PAREN_IZQ
            case ")" => Operador.PAREN_DER
            case _ => x
        }
        op :: _leerEcuacionRec(xs)
    }
}

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

def interpretarEcuacion(ecuacion: List[Operador | String]): Expresion = {
    _interpretarEcuacion(ecuacion)
}

private def _interpretarEcuacion(ecuacion: List[Operador | String]): Expresion = {
    ecuacion match {
        case Nil => Variable("")
        case (s: String) :: xs => Variable(s)
        case Operador.LAMBDA :: (nombre: String) :: Operador.PUNTO :: xs =>
            val variable = Variable(nombre)
            Abstraccion(variable, cuerpo = _interpretarEcuacion(xs))
        case Operador.PAREN_IZQ :: xs =>
            val indexSeparacion = encontrarAplicacion(xs.init, 0, 1)
            val (lista1, lista2) = xs.splitAt(indexSeparacion)
            Aplicacion(funcion = _interpretarEcuacion(lista1), argumento = _interpretarEcuacion(lista2))
    }
}

def reducir(expresion: Expresion, reductor: Expresion => Expresion): Expresion = {
    reductor(expresion) match {
        case _ if reductor(expresion) == expresion => expresion
        case _ => reducir(reductor(expresion), reductor)
    }
}

def sustitucion(vl: Variable, arbol: Expresion, sustituyente: Expresion): Expresion = {
    arbol match {
        case v: Variable if v == vl => sustituyente
        case Variable(v) => Variable(v)
        case Abstraccion(e1, e2) => Abstraccion(e1, sustitucion(vl, e2, sustituyente))
        case Aplicacion(e1, e2) => Aplicacion(sustitucion(vl, e1, sustituyente), sustitucion(vl, e2, sustituyente))
    }
}
/*
def convertir(v: Variable): Variable = {

}
    case Abstraccion(e1, e2) => {
        e1 match {
            case vl => Abstraccion(convertir(e1), sustitucion(vl, e2))
            case _ => Abstraccion(e1, sustitucion(vl, e2))
        }
    }*/

def callByName(expresion: Expresion): Expresion = {
    expresion match {
        case Variable(v) => expresion
        case Abstraccion(e1, e2) => Abstraccion(e1, callByName(e2))
        case Aplicacion(e1, e2) =>
            e1 match {
                case Abstraccion(vl, e) => sustitucion(vl, e, e2)
                case _ => Aplicacion(callByName(e1), callByName(e2))
            }
    }
}

def callByValue(expresion: Expresion): Expresion = {
    expresion match {
        case Variable(v) => expresion
        case Abstraccion(e1, e2) => Abstraccion(e1, callByValue(e2))
        case Aplicacion(e1, e2) =>
            e1 match {
                case Abstraccion(vl, e) => sustitucion(vl, e, callByValue(e2))
                case _ => Aplicacion(callByValue(e1), callByValue(e2))
            }
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        //leerInput()
        val ecuacion = leerEcuacion("(λx.λx.(y x) z)")
        val expresion = interpretarEcuacion(ecuacion)
        val resultado = reducir(expresion, callByName)
    }
}
/*
