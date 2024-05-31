package procesador

import modelo.expresion.*

def reducir(expresion: Expresion, reductor: Expresion => Expresion): Expresion = {
    reductor(expresion)
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
        case _ if _callByName(expresion) == expresion => expresion
        case _ => callByName(_callByName(expresion))
    }
}

def _callByName(expresion: Expresion): Expresion = {
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
        case _ if _callByValue(expresion) == expresion => expresion
        case _ => callByValue(_callByValue(expresion))
    }
}

def _callByValue(expresion: Expresion): Expresion = {
    expresion match {
        case Variable(v) => expresion
        case Abstraccion(e1, e2) => Abstraccion(e1, _callByValue(e2))
        case Aplicacion(e1, e2) =>
            e1 match {
                case Abstraccion(vl, e) => _callByValue(sustitucion(vl, e, _callByValue(e2)))
                case _ => Aplicacion(_callByValue(e1), _callByValue(e2))
            }
    }
}
def variablesLibres(expresion: Expresion) : Expresion = {
    VarLibres(_variablesLibres(expresion))
}

def _variablesLibres(expresion: Expresion): Set[String] = {
    expresion match {
        case Variable(v) => Set(v)
        case Abstraccion(Variable(vl), e2) => _variablesLibres(e2) - vl
        case Aplicacion(e1, e2) => _variablesLibres(e1) ++ _variablesLibres(e2)
    }
}
