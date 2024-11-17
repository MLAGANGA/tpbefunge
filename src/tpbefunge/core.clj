(ns tpbefunge.core
  (:require [clojure.java.io :as io])
  (:import [java.util Random]))

;; Estado del intrprete
;(def grilla (atom []))  ;; Esto va a servir para la grilla donde se va a ejecutar el programa
;(def x (atom 0))        ;; Posicion x del puntero
;(def y (atom 0))        ;; Posicion y del puntero
;(def direccion (atom :right))  ;; Direccion en la que el puntero se mueve
;(def pila (atom '()))   ;; Pila para las operaciones
;(def en-comillas? (atom false))  ;; Para indicar que estamos dentro de una cadena
(def direcciones {:right [1 0], :left [-1 0], :up [0 -1], :down [0 1]})
(def random (Random.))  ;; Para manejar direcciones aleatorias
(def max-pasos 1000)    ;; Para evitar bucles infinitos (creo que el maze.bf es un caso)
(def dimensiones {:x 80 :y 25})


(defn leer-archivo [filename]
  (with-open [reader (io/reader filename)]
    (let [max-ancho (:x dimensiones)
          max-alto (:y dimensiones)
          lineas (take max-alto(doall (line-seq reader)))
          ;completo columnas con espacios
          grilla-aux (vec (map
                            #(vec (concat
                                    (take max-ancho %)
                                    (repeat
                                      (- max-ancho (count %))
                                      \space)))
                            lineas))]

      ;completo filas con espacios
      (vec (concat grilla-aux
                   (repeat (- max-alto (count grilla-aux))
                           (vec(repeat
                                 max-ancho
                                 \space))))))))

(defn envolver [val max]
  (mod (+ val max) max))

(defn mover [direccion pc]
      (let [[desplazamiento-x desplazamiento-y] (get direcciones direccion)
            x (get pc 0)
            y (get pc 1)
            limite-ancho (:x dimensiones)
            limite-alto (:y dimensiones)]
           [(envolver (+ x desplazamiento-x) limite-ancho)
            (envolver (+ y desplazamiento-y) limite-alto)]))


(defn crear-pila []
  '())

(defn apilar [pila val]
  "Apilar un valor en la pila."
  (conj pila val))


(defn desapilar [pila]
  "Desapilar un valor de la pila; si la pila está vacía, devuelve 0."
      (if (seq pila)
        [(peek pila) (pop pila)]
        [nil pila]))

(defn operacion-binaria [pila f]
      (let [[a p1] (desapilar pila)
            [b p2] (desapilar p1)]
           (if (and (not= a nil) (not= b nil))
             (apilar p2 (f b a))
             (do
               (println "Error: valor nulo en la pila.") ;hay que manejar mejor esto
               pila))))
;
;(defn cambiar-valor-grilla [grilla x y v]
;  )


(defn procesar-instruccion [mapa-estado instr]
  "Procesa una instrucción Befunge."

  (let [{:keys [pila direccion grilla en-comillas? pc]} mapa-estado]

    (if en-comillas?
      (if (= instr \")
        (assoc mapa-estado :en-comillas? (not en-comillas?))
        (assoc mapa-estado :pila (apilar pila (int instr)))) ; esto repite codigo?

      (case instr
      \> (assoc mapa-estado :direccion :right)
      \< (assoc mapa-estado :direccion :left)
      \^ (assoc mapa-estado :direccion :up)
      \v (assoc mapa-estado :direccion :down)
      \+ (assoc mapa-estado :pila (operacion-binaria pila +))
      \- (assoc mapa-estado :pila (operacion-binaria pila +))
      \* (assoc mapa-estado :pila (operacion-binaria pila +))
      \/ (assoc mapa-estado :pila (if(= (peek pila) 0)
                                     (apilar (pop (pop pila)) 0)
                                     (operacion-binaria pila quot)))
      \% (assoc mapa-estado :pila (if(= (peek pila) 0)
                                  (apilar (pop (pop pila)) 0)
                                  (operacion-binaria pila mod)))
      \! (assoc mapa-estado :pila (if(zero? (peek pila))
                                    (apilar (pop pila) 1)
                                    (apilar (pop pila) 0)))
      \` (let [[a p1] (desapilar pila) [b p2] (desapilar p1)]
         (assoc mapa-estado :pila (apilar p2 (if (> b a) 1 0))))
      \? (assoc mapa-estado :direccion (rand-nth (keys direcciones)))
      \_ (let [[v p] (desapilar pila)]
         (if(zero? v)
            (assoc mapa-estado :direccion :right :pila p)
            (assoc mapa-estado :direccion :left :pila p)))
      \| (let [[v p] (desapilar pila)]
         (if(zero? v)
         (assoc mapa-estado :direccion :up :pila p)
         (assoc mapa-estado :direccion :down :pila p)))
      \" (assoc mapa-estado :en-comillas? (not en-comillas?))
      \: (assoc mapa-estado :pila (apilar pila (or (peek pila) 0)))
      \\ (let [[a p1] (desapilar pila) [b p2] (desapilar p1)]
         (assoc mapa-estado :pila (apilar (apilar p2 a) b)))
      \$ (assoc mapa-estado :pila (pop pila))
      \.       (let [[chr p] (desapilar pila)]
                 (if chr
                   (do
                     (print (str num " "))
                     (assoc mapa-estado :pila p))
                   mapa-estado))
      \, (let [[chr p] (desapilar pila)]
           (if chr
             (do
               (print (char chr))
               (assoc mapa-estado :pila p))
             mapa-estado))
      \# (let [[nuevo-pc] (mover (:direccion mapa-estado) (:pc mapa-estado))]
            (assoc mapa-estado :pc nuevo-pc))
      ;\= (let [a (desapilar pila) b (desapilar pila)] (apilar pila (if (= b a) 1 0)))
      \p (let [[v p1] (desapilar pila) [x p2] (desapilar p1) [y p3] (desapilar p2)  ]
         (swap! grilla update-in [y x] (constantly (char v))))
      \g (let [[y p1] (desapilar pila) [x p2] (desapilar p1)]
         (when (and (< x (:x dimensiones)) (< y (:y dimensiones)))
           (assoc mapa-estado :pila (apilar p2 (int (get-in grilla [(get pc 0) (get pc 1)]))))))
      \& (apilar pila (Integer/parseInt (read-line)))
      \~ (apilar pila (int (first (read-line))))
      \space mapa-estado
      (when (Character/isDigit instr)
        (assoc mapa-estado :pila (apilar pila (Character/digit instr 10)))))))) ;devuelve el mapa si no hay instruccion valida

(defn paso [mapa-estado instr]
      ;crea un mapa actualizado a partir del procesamiento de la instruccion
      ;crea un pc nuevo a partir de mover y devuelve el mapa actualizado con el
      ;nuevo pc.
  (let [mapa-actualizado (procesar-instruccion mapa-estado instr)
        nuevo-pc (mover (:direccion mapa-actualizado) (:pc mapa-actualizado))]
       (assoc mapa-actualizado :pc nuevo-pc)
  ))

(defn crear-mapa-estado [filename]
  {
   :pila (crear-pila)
   :grilla (leer-archivo filename)
   :direccion :right
   :en-comillas? false
   :pc [0 0]
   })


;(defn ejecutar-con-limite [filename]
;  (leer-archivo filename)
;  (loop [pasos 0]
;    (when (and (not= (get-in @grilla [@y @x]) \@) (< pasos max-pasos))
;      (paso)
;      (recur (inc pasos))))
;  (if (= (get-in @grilla [@y @x]) \@)
;    (println "\nPrograma finalizado.")
;    (println "\nPrograma detenido después de alcanzar el máximo de pasos para prevenir bucles infinitos.")))

(defn obtener-instr [mapa-estado]
      (let [pc (:pc mapa-estado)]
           (get-in (:grilla mapa-estado) [(get pc 1) (get pc 0)])))

;; Ciclo principal de ejecucion, se detiene en  @
(defn ejecutar [filename]
  ;(leer-archivo filename)
  (let [mapa-estado (atom(crear-mapa-estado filename))]
  (while (not= (obtener-instr @mapa-estado) \@)
          (let[instr (obtener-instr @mapa-estado)
          mapa-actualizado (paso @mapa-estado instr)]
          (reset! mapa-estado mapa-actualizado)))
  (println "\nPrograma finalizado.")))


(defn -main [& args]
  (if (or (empty? args) (not (clojure.string/ends-with? (first args) ".bf")))
    (println "ERROR: ¡Especifica un archivo .bf como entrada!")
    (ejecutar (first args))))


