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
  (testing "Testeando instruccion /"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(3 9)) \/)]
      (is (= (:pila mapa) '(3)))))
  (testing "Testeando instruccion /"
    (let [mapa (procesar-instruccion (assoc (crear-mapa-estado) :pila '(3 0)) \/)]
      (is (= (:pila mapa) '(0)))))
  (testing "Testeando instruccion /"
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

;(deftest test-condicionales
;  testing "Testeando instruccion !"
;
;
;  )





