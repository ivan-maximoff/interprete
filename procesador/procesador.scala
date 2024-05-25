package procesador

import modelo.expresion.*

def procesar(expresion: Expresion): Float = {
    expresion match
        case Variable(v) => v
        case Abstraccion(e1, e2) => 
        case Aplicacion(e1, e2) => 
}
