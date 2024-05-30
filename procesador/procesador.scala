package procesador

import modelo.expresion.*

def reducir(expresion: Expresion, reductor: Expresion => Expresion): Expresion = {
    reductor(expresion)
}

def sustitucion(vl: Variable, arbol: Expresion, sustituyente: Expresion): Expresion = {
    arbol match {
        case v: Variable => v
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
            callByName(e1)
            e1 match {case Abstraccion(vl, e) => sustitucion(vl, e, e2)}
            callByName(e2)
    }
}

def callByValue(expresion: Expresion): Expresion = {
    expresion match {
        case Variable(v) => expresion
        case Abstraccion(vl, e2) => callByValue(e2)
        case Aplicacion(e1,e2) =>
            callByValue(e1)
            e1 match {
                case Abstraccion(vl, e) =>
                    callByValue(e2)
                    sustitucion(vl, e, e2)
            }
            callByValue(e2) /*?hace falta?*/
    }
}
/*
def variablesLibes(expresion: Expresion): Expresion = {
    expresion match {
        case Variable(v) => expresion
        case Abstraccion(vl, e2) => variablesLibes(e2) - {vl}
        case Aplicacion(e1, e2) => variablesLibes(e1) + variablesLibes(e2)
    }
}*/