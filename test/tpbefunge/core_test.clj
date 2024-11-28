(ns tpbefunge.core-test
  (:require [clojure.test :refer :all]
            [tpbefunge.core :refer :all]
            [clojure.java.io :as io]))

(deftest test-envolver
  (testing "Testeando superficie toroide para un elemento"
    (is (= 4 (envolver 129 25))))
  (testing "Testeando caso borde (envolver x x)")
  (is(= 0 (envolver 25 25)))
  (testing "Testeando caso base (envolver 0 x)")
  (is(= 0 (envolver 0 25))))

(deftest test-instrucciones-direccion-pc
  (testing "Testeando instruccion >"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :direccion :left) \>)]
      (is (= (:direccion mapa) :right))))
  (testing "Testeando instruccion <"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \<)]
      (is (= (:direccion mapa) :left))))
  (testing "Testeando instruccion ^"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \^)]
      (is (= (:direccion mapa) :up))))
  (testing "Testeando instruccion v"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \v)]
      (is (= (:direccion mapa) :down))))
  )

(deftest test-instrucciones-operaciones-aritmeticas
  (testing "Testeando instruccion +"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(1 2)) \+)]
      (is (= (:pila mapa) '(3)))))
  (testing "Testeando instruccion -"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(3 7)) \-)]
      (is (= (:pila mapa) '(4)))))
  (testing "Testeando instruccion *"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(3 7)) \*)]
      (is (= (:pila mapa) '(21)))))
  (testing "Testeando instruccion /"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(3 9)) \/)]
      (is (= (:pila mapa) '(3)))))
  (testing "Testeando instruccion / dividendo igual a 0"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(3 0)) \/)]
      (is (= (:pila mapa) '(0)))))
  (testing "Testeando instruccion / division por 0"
    (is (thrown? ArithmeticException
                 (procesar-instruccion (assoc (crear-mapa-estado) :pila '(0 3)) \/))))
  )

(deftest test-instruccion-modulo
  (testing "Testeando division dos enteros distintos a 0"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(5 8)) \%)]
      (is (= (:pila mapa) '(3)))))
  (testing "Testeando division por 0"
    (is (thrown? ArithmeticException
                 (procesar-instruccion (assoc (crear-mapa-estado) :pila '(0 8)) \%))))
  (testing "Testeando division 0/x"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(7 0)) \%)]
      (is (= (:pila mapa) '(0)))))
  )


(deftest test-condicionales
  (testing "Testeando instruccion !"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(7)) \!)]
      (is(= (:pila mapa) '(0)))))
  (testing "Testeando instruccion !"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(0)) \!)]
      (is(= (:pila mapa) '(1)))))
  (testing "Testeando instruccion `"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(1 2)) \`)]
      (is(= (:pila mapa) '(1)))))
  (testing "Testeando instruccion `"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(4 3)) \`)]
      (is(= (:pila mapa) '(0)))))
  (testing "Testeando instruccion _"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(4)) \_)]
      (is(= (:direccion mapa) :left))))
  (testing "Testeando instruccion _"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(0)) \_)]
      (is(= (:direccion mapa) :right))))
  (testing "Testeando instruccion |"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(0)) \|)]
      (is(= (:direccion mapa) :down))))
  (testing "Testeando instruccion |"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(2)) \|)]
      (is(= (:direccion mapa) :up))))
)

(deftest test-comillas-strings
  (testing "Apertura comillas"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \")]
      (is(= (:en-comillas? mapa) true))))
  (testing "Apertura y cierre comillas"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \")]
      (is(= (:en-comillas? mapa) true))
      (is(= (:en-comillas? (procesar-instruccion mapa \") false)))))
  (testing "Apilar string entero"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \")
          mapa1 (procesar-instruccion mapa \h)  ;h ascii = 104
          mapa2 (procesar-instruccion mapa1 \o) ;o ascii = 111
          mapa3 (procesar-instruccion mapa2 \l) ;l ascii = 108
          mapa4 (procesar-instruccion mapa3 \a) ;a ascii = 97
          mapa-final (procesar-instruccion mapa4 \")]
      (is(= (:en-comillas? mapa) true))
      (is(= (:pila mapa-final) '(97 108 111 104)))
      (is(= (:en-comillas? (procesar-instruccion mapa-final \") false))))))

(deftest test-comandos-especiales
  (testing "Testeando comando : con pila vacia"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \:)]
      (is(= (:pila mapa) '(0)))))
  (testing "Testeando comando : con pila poblada"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \4)
          mapa1 (procesar-instruccion mapa \5)
          mapa2 (procesar-instruccion mapa1 \:)]
      (is(= (:pila mapa2) '(5 5 4)))))
  (testing "Testeando instruccion barra invertida"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \\)]
      (is(= (:pila mapa) '(0 0))))) ;verificado con interprete: \ en pila vacia, apila (0 0)
  (testing "Testeando instruccion barra invertida"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(4 5)) \\)]
      (is(= (:pila mapa) '(5 4)))))
  (testing "Testeando instruccion $"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \$)]
      (is(= (:pila mapa) '()))))
  (testing "Testeando instruccion $"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(4 5)) \$)]
      (is(= (:pila mapa) '(5)))))
  (testing "Testeando instruccion #"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \#)]
      (is(= (:pc mapa) [1 0] )))))

(deftest test-comandos-salida
  (testing "Testeando comando ."
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(4 5)) \.)]
      (is(= (:pila mapa) '(5)))))
  (testing "Testeando comando ,"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(97 101)) \,)]
      (is(= (:pila mapa) '(101))))))

(deftest test-automodificacion
  (testing "Testeando comando g"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(2 2)
                                                                :grilla [[1 2 3]
                                                                         [6 5 7]
                                                                         [9 4 8]]) \g)]
      (is(= (:pila mapa) '(8)))))
  (testing "Testeando comando g fuera de limites"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(120 200)
                                                                :grilla [[1 2 3]
                                                                         [6 5 7]
                                                                         [9 4 8]]) \g)]
      (is(= (:pila mapa) '(0)))))
  (testing "Testeando comando p"
    (let [mapa (procesar-instruccion
                 (assoc
                  (crear-mapa-estado)
                   :pila    '(2 2 97)
                   :grilla  [[1 2 3]
                             [6 5 7]
                             [9 4 8]]) \p)]
      (is (= (:grilla mapa) [[1 2 3]
                             [6 5 7]
                             [9 4 \a]])) ;se carga como caracter ascii el valor 97
      (is (= (:pila mapa '())))))

  (testing "Testeando comando p fuera de limites"
    (let [mapa (procesar-instruccion
                 (assoc
                   (crear-mapa-estado)
                   :pila    '(26 81 97) ;la grilla se considera de 80x25
                            ; y  x  v
                   :grilla  [[1 2 3]
                             [6 5 7]
                             [9 4 8]]) \p)]
      (is (= (:grilla mapa) [[1 2 3]
                             [6 \a 7]
                             [9 4 8]])) ;se carga como caracter ascii el valor 97
      (is (= (:pila mapa '()))))))


(deftest test-noop
  (testing "Testeando caracter espacio"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \space)]
      (is(= mapa (crear-mapa-estado))))))

(deftest test-digitos
  (testing "Testeando apilar digitos"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \5)
          mapa1 (procesar-instruccion mapa \8)]
      (is(= (:pila mapa) '(5)))
      (is(= (:pila mapa1) '(8 5))))))

(deftest test-caracteres-invalidos
  (testing "Testeando caracteres no ASCII"
    (let [mapa (procesar-instruccion (crear-mapa-estado) \í) ;es una i con tilde
          mapa1 (procesar-instruccion mapa \ñ)] ;procesar-instruccion devuelve el mismo mapa
      (is(= mapa (crear-mapa-estado)))
      (is(= mapa1 mapa))))) ;por transitividad mapa1 = (crear-mapa-estado)

(deftest test-mover
  (testing "Mover hacia la derecha"
    (is (= [1 0] (mover :right [0 0]))))
  (testing "Mover hacia abajo"
    (is (= [0 1] (mover :down [0 0]))))
  (testing "Mover hacia la izquierda (con envolvimiento)"
    (is (= [79 0] (mover :left [0 0]))))
  (testing "Mover hacia arriba (con envolvimiento)"
    (is (= [0 24] (mover :up [0 0]))))
  (testing "Mover hacia la derecha (con envolvimiento)"
    (is (= [0 15] (mover :right [79 15]))))
  (testing "Mover hacia abajo (con envolvimiento)"
    (is (= [53 0] (mover :down [53 24])))))

(deftest test-paso
  (testing "Ejecutar un paso con direccion"
    (let [estado {:pila (crear-pila)
                  :grilla [[]]
                  :direccion :right
                  :en-comillas? false
                  :pc [0 0]}
          estado-actualizado (paso estado \>)]
      (is (= :right (:direccion estado-actualizado)))
      (is (= [1 0] (:pc estado-actualizado)))))

  (testing "Ejecutar un paso con suma"
    (let [estado {:pila (-> (crear-pila) (apilar 3) (apilar 5))
                  :grilla [[]]
                  :direccion :right
                  :en-comillas? false
                  :pc [0 0]}
          estado-actualizado (paso estado \+)]
      (is (= '(8) (:pila estado-actualizado)))
      (is (= [1 0] (:pc estado-actualizado)))))

  (testing "Ejecutar un paso con un cambio de direccion"
    (let [estado {:pila (crear-pila)
                  :grilla [[]]
                  :direccion :right
                  :en-comillas? false
                  :pc [0 0]}
          estado-actualizado (paso estado \v)]
      (is (= :down (:direccion estado-actualizado)))
      (is (= [0 1] (:pc estado-actualizado))))))
