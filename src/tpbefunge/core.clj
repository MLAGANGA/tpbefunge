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

(defn leer-archivo [filename]
  (with-open [reader (io/reader filename)]
    (let [lineas (doall (line-seq reader))
          max-ancho (apply max (map count lineas))]
      (reset! grilla (vec (map #(vec (concat % (repeat (- max-ancho (count %)) \space))) lineas))))))

(defn envolver [val max]
  (mod (+ val max) max))  

(defn mover []
  (let [[desplazamiento-x desplazamiento-y] (direcciones @direccion)]
    (swap! x #(envolver (+ % desplazamiento-x) (count (first @grilla)))) 
    (swap! y #(envolver (+ % desplazamiento-y) (count @grilla)))))  

(defn apilar [val]
  (swap! pila conj val))

(defn desapilar []
  (let [val (first @pila)]
    (swap! pila rest)
    (or val 0)))


;; Ciclo principal de ejecucion, termina cuando aparece @
(defn ejecutar [filename]
  (leer-archivo filename)
  (while (not= (get-in @grilla [@y @x]) \@)
    (paso))
  (println "\nPrograma finalizado."))