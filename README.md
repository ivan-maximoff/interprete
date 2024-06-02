Ejemplo que nos dan la presentacion del TP

leerEcuacion("(λx.λy.y (λx.(x x) λx.(x x)))")
interpretarEcuacion(res0)

Aplicacion(Abstraccion(Variable(x),Abstraccion(Variable(y),Variable(y))),Aplicacion(Abstraccion(Variable(x),Aplicacion(Variable(x),Variable(x))),Abstraccion(Variable(x),Aplicacion(Variable(x),Variable(x)))))

![image](https://github.com/ivan-maximoff/interprete/assets/112181357/84163e0f-0e94-43d1-9fc2-80beab12285b)

Pasos:
1) interprete$ scalac **/*.scala
2) interprete$ scala main.scala
