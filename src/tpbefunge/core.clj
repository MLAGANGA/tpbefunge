(ns befunge-interpreter.core
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
  "Apilar un valor en la pila."
  (swap! pila conj val))

(defn desapilar []
  "Desapilar un valor de la pila; si la pila está vacía, devuelve 0."
  (let [val (first @pila)]
    (swap! pila rest)
    (or val 0)))

(defn procesar-instruccion [instr]
  "Procesa una instrucción Befunge."
  (if @en-comillas?
    (if (= instr \") (swap! en-comillas? not) (apilar (int instr)))
    (case instr
      \> (reset! direccion :right)
      \< (reset! direccion :left)
      \^ (reset! direccion :up)
      \v (reset! direccion :down)
      \+ (apilar (+ (desapilar) (desapilar)))
      \- (let [a (desapilar) b (desapilar)] (apilar (- b a)))
      \* (apilar (* (desapilar) (desapilar)))
      \/ (let [a (desapilar) b (desapilar)] (apilar (if (= a 0) 0 (quot b a))))
      \% (let [a (desapilar) b (desapilar)] (apilar (if (= a 0) 0 (mod b a))))
      \! (apilar (if (zero? (desapilar)) 1 0))
      \` (let [a (desapilar) b (desapilar)] (apilar (if (> b a) 1 0)))
      \? (reset! direccion (rand-nth (keys direcciones)))
      \_ (reset! direccion (if (zero? (desapilar)) :right :left))
      \| (reset! direccion (if (zero? (desapilar)) :down :up))
      \" (swap! en-comillas? not)
      \: (apilar (or (first @pila) 0))
      \\ (let [a (desapilar) b (desapilar)] (apilar a) (apilar b))
      \$ (desapilar)
      \. (let [num (desapilar)] (when num (print (str num " ")))) 
      \, (let [chr (desapilar)] (when chr (print (char chr))))     
      \# (mover) 
      \= (let [a (desapilar) b (desapilar)] (apilar (if (= b a) 1 0)))
      \p (let [y (desapilar) x (desapilar) v (desapilar)]
           (swap! grilla update-in [y x] (constantly (char v))))
      \g (let [y (desapilar) x (desapilar)]
           (when (and (< y (count @grilla)) (< x (count (first @grilla))))
             (apilar (int (get-in @grilla [y x])))))
      \& (apilar (Integer/parseInt (read-line)))
      \~ (apilar (int (first (read-line))))
      (when (Character/isDigit instr)
        (apilar (Character/digit instr 10))))))

(defn paso []
  (procesar-instruccion (get-in @grilla [@y @x]))
  (mover))

(defn ejecutar-con-limite [filename]
  (leer-archivo filename)
  (loop [pasos 0]
    (when (and (not= (get-in @grilla [@y @x]) \@) (< pasos max-pasos))
      (paso)
      (recur (inc pasos))))
  (if (= (get-in @grilla [@y @x]) \@)
    (println "\nPrograma finalizado.")
    (println "\nPrograma detenido después de alcanzar el máximo de pasos para prevenir bucles infinitos.")))

;; Ciclo principal de ejecucion, se detiene en  @
(defn ejecutar [filename]
  (leer-archivo filename)
  (while (not= (get-in @grilla [@y @x]) \@)
    (paso))
  (println "\nPrograma finalizado."))

(defn -main [& args]
  (if (or (empty? args) (not (clojure.string/ends-with? (first args) ".bf")))
    (println "ERROR: ¡Especifica un archivo .bf como entrada!")
    (ejecutar (first args))))
