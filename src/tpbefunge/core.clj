(ns tpbefunge.core
  (:require [clojure.java.io :as io]))

;Constantes
(def direcciones {:right [1 0], :left [-1 0], :up [0 -1], :down [0 1]})
(def max-iter 1000000)    ;; Para evitar bucles infinitos
(def dimensiones {:x 80 :y 25}) ;; Dimensiones de la grilla

;Funciones pila
(defn crear-pila [] '())

(defn apilar [pila val]
  (conj pila val))

(defn desapilar [pila]
  (if (seq pila)
    [(peek pila) (pop pila)]
    [0 pila]))

;Parser archivo y generación de grilla 80x25
(defn leer-archivo [filename]
  (with-open [reader (io/reader filename)]
    (let [max-ancho (:x dimensiones)
          max-alto (:y dimensiones)
          lineas (take max-alto (doall (line-seq reader)))
          grilla-aux (vec (map #(vec (concat (take max-ancho %)
                                             (repeat (- max-ancho (count %)) \space)))
                               lineas))]
      (vec (concat grilla-aux (repeat (- max-alto (count grilla-aux))
                                      (vec (repeat max-ancho \space))))))))

;funcion superficie toroide
(defn envolver [val max]
  (mod (+ val max) max))

;recibe direccion y pc, devuelve vector con el nuevo pc
(defn mover [direccion pc]
  (let [[dx dy] (get direcciones direccion)
        [x y] pc
        limite-ancho (:x dimensiones)
        limite-alto (:y dimensiones)]
    [(envolver (+ x dx) limite-ancho) (envolver (+ y dy) limite-alto)]))

;funcion auxiliar para operaciones binarias. Recibe pila y una función binaria.
;aplica función a los dos primeros valores desapilados, apila el resultado y devuelve la nueva pila.
(defn operacion-binaria [pila f]
  (let [[a p1] (desapilar pila)
        [b p2] (desapilar p1)]
    (apilar p2 (f b a))))

; Inicialización del mapa que contiene el estado del programa Befunge93.
(defn crear-mapa-estado []
  {:pila (crear-pila)
   :grilla [[]]
   :direccion :right
   :en-comillas? false
   :pc [0 0]})

; Procesamiento de instrucciones
(defn procesar-instruccion [mapa-estado instr]
  ;crea variables locales con el nombre de las claves de mapa-estado
  (let [{:keys [pila direccion grilla en-comillas? pc]} mapa-estado]
    (if en-comillas?
      ;si en-comillas es true y la instruccion es ("), se invierte el valor de en-comillas?
      ;si no, se apila el valor entero del caracter ASCII recibido.
      (if (= instr \")
        (assoc mapa-estado :en-comillas? false)
        (assoc mapa-estado :pila (apilar pila (int instr))))
      ;si en-comillas? es false, se trata a la instruccion como un comando.
      (case instr
        \> (assoc mapa-estado :direccion :right)
        \< (assoc mapa-estado :direccion :left)
        \^ (assoc mapa-estado :direccion :up)
        \v (assoc mapa-estado :direccion :down)
        \+ (assoc mapa-estado :pila (operacion-binaria pila +))
        \- (assoc mapa-estado :pila (operacion-binaria pila -))
        \* (assoc mapa-estado :pila (operacion-binaria pila *))
        \/ (let [[a p1] (desapilar pila)
                 [b p2] (desapilar p1)]
             (if(zero? a)
               (throw (ArithmeticException. "Error: intentando dividir por 0")))
             (assoc mapa-estado :pila (apilar p2 (quot b a))))
        \% (let [[a p1] (desapilar pila)
                 [b p2] (desapilar p1)]
             (if(zero? a)
               (throw (ArithmeticException. "Error: intentando dividir por 0")))
             (assoc mapa-estado :pila (apilar p2 (mod b a))))
        \! (let [[v p] (desapilar pila)]
             (assoc mapa-estado :pila (apilar p (if (zero? v) 1 0))))
        \` (let [[a p1] (desapilar pila)
                 [b p2] (desapilar p1)]
             (assoc mapa-estado :pila (apilar p2 (if (> b a) 1 0))))
        \? (assoc mapa-estado :direccion (rand-nth (keys direcciones)))
        \_ (let [[v p] (desapilar pila)]
             (assoc mapa-estado :direccion (if (zero? v) :right :left) :pila p))
        \| (let [[v p] (desapilar pila)]
             (assoc mapa-estado :direccion (if (zero? v) :down :up) :pila p))
        \" (assoc mapa-estado :en-comillas? true)
        \: (assoc mapa-estado :pila (apilar pila (or (peek pila) 0)))
        \\ (let [[a p1] (desapilar pila)
                 [b p2] (desapilar p1)]
             (assoc mapa-estado :pila (apilar (apilar p2 a) b)))
        \$ (assoc mapa-estado :pila (second(desapilar pila)))
        \. (let [[num p] (desapilar pila)]
             (do (print (str num " ")) (assoc mapa-estado :pila p)))
        \, (let [[chr p] (desapilar pila)]
             (do  (if (not= chr 0) (print (char chr)))
                  (assoc mapa-estado :pila p)))
        \# (assoc mapa-estado :pc (mover direccion pc))
        \p (let [[y p1] (desapilar pila)
                 [x p2] (desapilar p1)
                 [v p3] (desapilar p2)]
             (assoc mapa-estado :grilla (assoc-in grilla [(envolver y (:y dimensiones))
                                                          (envolver x (:x dimensiones))] (char v))
                                :pila p3))
        \g (let [[y p1] (desapilar pila)
                 [x p2] (desapilar p1)]
             (assoc mapa-estado :pila (apilar p2 (if(and (> (:x dimensiones) x)
                                                         (> (:y dimensiones) y))
                                                   (int (get-in grilla [y x]))
                                                   0))))
        \& (assoc mapa-estado :pila (apilar pila (Integer. (read-line))))
        \~ (assoc mapa-estado :pila (apilar pila (int (first (read-line)))))
        \space mapa-estado
        (if (Character/isDigit instr)
          (assoc mapa-estado :pila (apilar pila (Character/digit instr 10)))
          ;si no se satisfizo ninguna de las anteriores condiciones, se devuelve el estado tal cual entró
          mapa-estado)))))

;Funcion que se ejecuta con cada instruccion. Ordena el procesamiento y el movimiento del pc. Devuelve mapa actualizado.
(defn paso [mapa-estado instr]
  (let [mapa-actualizado (procesar-instruccion mapa-estado instr)
        nuevo-pc (mover (:direccion mapa-actualizado) (:pc mapa-actualizado))]
    (assoc mapa-actualizado :pc nuevo-pc)))

(defn obtener-instr [mapa-estado]
  (let [[x y] (:pc mapa-estado)]
    (get-in (:grilla mapa-estado) [y x])))

(defn ejecutar [filename]
  (let [mapa-estado (crear-mapa-estado)]
    (loop [iter 0
           estado (assoc mapa-estado :grilla (leer-archivo filename))]
      (let [instr (obtener-instr estado)]
        (if (and (not= instr \@) (< iter max-iter))
          (recur (inc iter) (paso estado instr))
          (println "\nPrograma finalizado."))))))
(defn -main [& args]
  (if (or (empty? args) (not (clojure.string/ends-with? (first args) ".bf")))
    (println "ERROR: ¡Especifica un archivo .bf como entrada!")
    (time (ejecutar (first args)))))