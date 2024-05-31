package interprete



object Main {
    def main(args: Array[String]): Unit = {
        //leerInput()
        val ecuacion = leerEcuacion("(λx.λx.(y x) z)")
        val expresion = interpretarEcuacion(ecuacion)
        val resultado = reducir(expresion, callByName)
    }
}
/*
