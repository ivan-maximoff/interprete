package lambda

import interprete.interpretarEcuacion
import lector.leerEcuacion
import procesador.procesar

import java.util.Scanner
import scala.io.StdIn.readLine

object Main {
    def main(args: Array[String]): Unit = {
        leerInput()
    }
}

def leerInput(): Unit = {
    val input = scala.io.StdIn.readLine()
    input match 
        case null =>
        case _ =>
            val ecuacionParseada = leerEcuacion(input)
            val expresion = interpretarEcuacion(ecuacionParseada)
            val resultado = procesar(expresion)
            printf("Resultado = %f \n", resultado)
            leerInput()
}