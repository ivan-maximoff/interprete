package lexer

import model.Operador

def leerEcuacion(ecuacion: String): List[Operador | String] = {
    val tokens = ecuacion.map(x => stringToToken(x.toString)).toList
    unirStrings(tokens)
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

def unirStrings(tokens : List[Operador | String]): List[Operador | String] = {
    tokens.foldRight(List.empty[Operador | String]) {
        case (token : String, (head : String) :: tail) => (token+head) :: tail
        case (token, sig) => token :: sig
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