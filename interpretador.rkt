b) 5pts. Escriba un programa en su lenguaje de programación que contenga un
procedimiento que permita calcular el factorial de un número n.
Como la gramática para funciones recursivas debe ser propuesta por el grupo,
incluya dos ejemplos de uso para el factorial de 5 y el factorial de 10.

c) 10pts. Escriba un programa en su lenguaje de programación que contenga un
procedimiento que permita calcular una suma de forma recursiva.
Debe hacer uso de las funciones add1 y sub1 (remitase a la clase
donde se implementó la interfaz con las funciones zero, isZero?, sucessor, predecessor).
Si no se evidencia el uso de add1 y sub1, el ejercicio no será valido.
Incluya un llamado a la función recursiva: "evaluar @sumar (4, 5) finEval "

declarar (
          @add1 = procedimiento(@n) haga (@n + 1) finProc;
          @sub1 = procedimiento(@n) haga (@n - 1) finProc;

          @sumar = funcionRec(@x;@y) haga
          Si (zero? @x) entonces
              @y
           sino
              evaluar sumar (sub1(@x), add1(@y)) finEval
              finSI
          finRec)
{evaluar @sumar (4,5) finEval}

d) 15pts. Escriba un programa en su lenguaje de programación que permita restar y
multiplicar dos números haciendo uso solamente de las primitivas add1 y sub1.
Incluya llamados:  "evaluar @restar (10, 3) finEval  ",  "evaluar @multiplicar (10, 3) finEval" 
