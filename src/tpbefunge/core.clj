(ns tpbefunge.core
  (:require [clojure.java.io :as io])
  (:import [java.util Random]))

;; Parámetros y configuraciones
(def direcciones {:right [1 0], :left [-1 0], :up [0 -1], :down [0 1]})
(def random (Random.))
(def max-pasos 1000)    ;; Para evitar bucles infinitos
(def dimensiones {:x 80 :y 25}) ;; Dimensiones de la grilla

;; Lectura del archivo y generación de la grilla
(defn leer-archivo [filename]
  (with-open [reader (io/reader filename)]
    (let [max-ancho (:x dimensiones)
          max-alto (:y dimensiones)
          lineas (take max-alto (doall (line-seq reader)))
          grilla-aux (vec (map #(vec (concat (take max-ancho %)
                                             (repeat (- max-ancho (count %)) \space)))
                               lineas))]
      (vec (concat grilla-aux
                   (repeat (- max-alto (count grilla-aux))
                           (vec (repeat max-ancho \space))))))))

;; Utilidades
(defn envolver [val max]
  (mod (+ val max) max))

(defn mover [direccion pc]
  (let [[dx dy] (get direcciones direccion)
        [x y] pc
        limite-ancho (:x dimensiones)
        limite-alto (:y dimensiones)]
    [(envolver (+ x dx) limite-ancho) (envolver (+ y dy) limite-alto)]))

;; Operaciones con la pila
(defn crear-pila [] '())

(defn apilar [pila val]
  (conj pila val))

(defn desapilar [pila]
  (if (seq pila)
    [(peek pila) (pop pila)]
    [0 pila]))

(defn operacion-binaria [pila f]
  (let [[a p1] (desapilar pila)
        [b p2] (desapilar p1)]
    (apilar p2 (f b a))))

;; Procesamiento de instrucciones
(defn procesar-instruccion [mapa-estado instr]
  (let [{:keys [pila direccion grilla en-comillas? pc]} mapa-estado]
    (if en-comillas?
      (if (= instr \")
        (assoc mapa-estado :en-comillas? false)
        (assoc mapa-estado :pila (apilar pila (int instr))))
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
              (assoc mapa-estado :pila (apilar p2 (if (zero? a) 0 (quot b a)))))
        \% (let [[a p1] (desapilar pila)
                  [b p2] (desapilar p1)]
              (assoc mapa-estado :pila (apilar p2 (if (zero? a) 0 (mod b a)))))
        \! (let [[v p] (desapilar pila)]
             (assoc mapa-estado :pila (apilar p (if (zero? v) 1 0))))
        \` (let [[a p1] (desapilar pila)
                  [b p2] (desapilar p1)]
             (assoc mapa-estado :pila (apilar p2 (if (> b a) 1 0))))
        \? (assoc mapa-estado :direccion (rand-nth (keys direcciones)))
        \_ (let [[v p] (desapilar pila)]
             (assoc mapa-estado :direccion (if (zero? v) :right :left) :pila p))
        \| (let [[v p] (desapilar pila)]
             (assoc mapa-estado :direccion (if (zero? v) :up :down) :pila p))
        \" (assoc mapa-estado :en-comillas? true)
        \: (assoc mapa-estado :pila (apilar pila (or (peek pila) 0)))
        \\ (let [[a p1] (desapilar pila)
                  [b p2] (desapilar p1)]
             (assoc mapa-estado :pila (apilar (apilar p2 a) b)))
        \$ (assoc mapa-estado :pila (pop pila))
        \. (let [[num p] (desapilar pila)]
             (do (print (str num " ")) (assoc mapa-estado :pila p)))
        \, (let [[chr p] (desapilar pila)]
             (do (print (char chr)) (assoc mapa-estado :pila p)))
        \# (assoc mapa-estado :pc (mover direccion pc))
        \p (let [[v p1] (desapilar pila)
                  [x p2] (desapilar p1)
                  [y p3] (desapilar p2)]
             (assoc mapa-estado :grilla (assoc-in grilla [y x] (char v)) :pila p3))
        \g (let [[y p1] (desapilar pila)
                  [x p2] (desapilar p1)]
             (assoc mapa-estado :pila (apilar p2 (int (get-in grilla [y x])))))
        \& (assoc mapa-estado :pila (apilar pila (Integer. (read-line))))
        \~ (assoc mapa-estado :pila (apilar pila (int (first (read-line)))))
        \space mapa-estado
        (if (Character/isDigit instr)
          (assoc mapa-estado :pila (apilar pila (Character/digit instr 10)))
          mapa-estado)))))

(defn paso [mapa-estado instr]
  (let [mapa-actualizado (procesar-instruccion mapa-estado instr)
        nuevo-pc (mover (:direccion mapa-actualizado) (:pc mapa-actualizado))]
    (assoc mapa-actualizado :pc nuevo-pc)))

;; Inicialización y ejecución
(defn crear-mapa-estado [filename]
  {:pila (crear-pila)
   :grilla (leer-archivo filename)
   :direccion :right
   :en-comillas? false
   :pc [0 0]})

(defn obtener-instr [mapa-estado]
  (let [pc (:pc mapa-estado)]
    (get-in (:grilla mapa-estado) [(second pc) (first pc)])))

(defn ejecutar [filename]
  (let [mapa-estado (atom (crear-mapa-estado filename))]
    (while (not= (obtener-instr @mapa-estado) \@)
      (let [instr (obtener-instr @mapa-estado)]
        (swap! mapa-estado paso instr)))
    (println "\nPrograma finalizado.")))

(defn -main [& args]
  (if (or (empty? args) (not (clojure.string/ends-with? (first args) ".bf")))
    (println "ERROR: ¡Especifica un archivo .bf como entrada!")
    (ejecutar (first args))))
