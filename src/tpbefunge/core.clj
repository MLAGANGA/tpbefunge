(ns tpbefunge.core
  (:require [clojure.java.io :as io])
  (:import [java.util Random]))

;; Estado del intrprete
(def grilla (atom []))  ;; Esto va a servir para la grilla donde se va a ejecutar el programa
(def x (atom 0))        ;; Posicion x del puntero
(def y (atom 0))        ;; Posicion y del puntero
(def direccion (atom :right))  ;; Direccion en la que el puntero se mueve
(def pila (atom '()))   ;; Pila para las operaciones
(def en-comillas? (atom false))  ;; Para indicar que estamos dentro de una cadena
(def direcciones {:right [1 0], :left [-1 0], :up [0 -1], :down [0 1]})
(def random (Random.))  ;; Para manejar direcciones aleatorias
(def max-pasos 1000)    ;; Para evitar bucles infinitos (creo que el maze.bf es un caso)



;; Ciclo principal de ejecución, se detiene en @
(defn ejecutar [filename]
  (leer-archivo filename)
  (while (not= (get-in @grilla [@y @x]) \@)
    (paso))
  (println "\nPrograma finalizado."))