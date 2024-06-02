package reductor

import model.expresion.*

def reducir(expresion: Expresion, reductor: Expresion => Expresion): Expresion = {
    reductor(expresion)
}

def convertir(expresion: Expresion, r: Variable): Expresion = {
    expresion match {
        case v : Variable => convertirVar(v, r)
        case Aplicacion(e1, e2) => Aplicacion(convertir(e1,r), convertir(e2,r))
        case Abstraccion(vl, e) if (vl.nombre == r.nombre) => Abstraccion(convertirVar(vl,r), convertir(e,r))
        case Abstraccion(vl, e) => Abstraccion(vl, convertir(e,r))
    }
}

def convertirVar(v: Variable, r: Variable): Variable = {
    v match {
        case _ if v == r => Variable(r.nombre + "*")
        case _ => v
    }
}

def sustitucion(vl: Variable, arbol: Expresion, sus: Expresion): Expresion = {
    arbol match {
        case v: Variable if v == vl => sus
        case Variable(v) => Variable(v)
        case Abstraccion(e1, e) if (e1 == vl) => Abstraccion(e1, e)
        case Abstraccion(e1, e) if (e1 == sus) => Abstraccion(convertirVar(e1,e1), sustitucion(vl, convertir(e, e1), sus))
        case Abstraccion(e1, e) => Abstraccion(e1, sustitucion(vl, e, sus))
        case Aplicacion(e1, e2) => Aplicacion(sustitucion(vl, e1, sus), sustitucion(vl, e2, sus))
    }
}

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
        case Aplicacion(e1, e2) => {
            e1 match 
                case Abstraccion(vl, e) => _callByValue(sustitucion(vl, e, _callByValue(e2)))
                case _ => Aplicacion(_callByValue(e1), _callByValue(e2))
        }
    }
}
def freeVariables(expresion: Expresion) : Expresion = {
    VarLibres(_freeVariables(expresion))
}

def _freeVariables(expresion: Expresion): Set[String] = {
    expresion match {
        case Variable(v) => Set(v)
        case Abstraccion(Variable(vl), e2) => _freeVariables(e2) - vl
        case Aplicacion(e1, e2) => _freeVariables(e1) ++ _freeVariables(e2)
    }
}
