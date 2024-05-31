package procesador

import modelo.expresion.*

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
def variablesLibres(expresion: Expresion): Set[String] = {
    expresion match {
        case Variable(v) => Set(v)
        case Abstraccion(Variable(vl), e2) => variablesLibres(e2) - vl
        case Aplicacion(e1, e2) => variablesLibres(e1) ++ variablesLibres(e2)
    }
}