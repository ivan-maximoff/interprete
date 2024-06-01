package interprete

import lexer.leerEcuacion
import lexer.leerTokens
import parser.parseEcuacion
import parser.interpretarArbol
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
        case _ if input.split(" ")[0] == "set" => input.split(" ")[1] match {
            case "call-by-name" => leerInput(callByName)
            case "call-by-value" => leerInput(callByValue)
            case "free-variables" => leerInput(freeVariables)
            case _ => leerInput(reductor)
        }
        case _ =>
            val ecuacion = leerEcuacion(input)
            val expresion = parseEcuacion(ecuacion)
            val reducido = reducir(expresion, callByName)
            val resultado = leerTokens(interpretarArbol(reducido))
            printf("Resultado = %f \n", resultado)
            leerInput(reductor)
    }
}