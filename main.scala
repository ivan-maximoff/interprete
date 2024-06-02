package interprete

import lexer.leerEcuacion
import lexer.leerTokens
import parser.parserEcuacion
import parser.parserArbol
import reductor.reducir
import reductor.callByName
import reductor.callByValue
import reductor.freeVariables
import model.expresion.*


object Main {
    def main(args: Array[String]): Unit = {
        leerInput(callByName)
    }
}

def leerInput(reductor : Expresion => Expresion): Unit = {
    val input = scala.io.StdIn.readLine()
    input match {
        case null =>
        case _ if input.split(" ").toList.head.toString == "set" => input.split(" ").toList.tail.head match {
            case "call-by-name" => leerInput(callByName)
            case "call-by-value" => leerInput(callByValue)
            case "free-variables" => leerInput(freeVariables)
            case _ => leerInput(reductor)
        }
        case _ =>
            val ecuacion = leerEcuacion(input)
            val expresion = parserEcuacion(ecuacion)
            val reducido = reducir(expresion, reductor)
            val resultado = leerTokens(parserArbol(reducido))
            println(resultado)
            leerInput(reductor)
    }
}