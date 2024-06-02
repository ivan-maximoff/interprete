package model.expresion

sealed trait Expresion

// Secuencia de tokens que puede recibir el Parser
case class Variable(nombre: String) extends Expresion
case class Abstraccion(variable: Variable, cuerpo: Expresion) extends Expresion
case class Aplicacion(funcion: Expresion, argumento: Expresion) extends Expresion
case class VarLibres(variables: Set[String]) extends Expresion
