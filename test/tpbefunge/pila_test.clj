(ns tpbefunge.pila-test
  (:require [clojure.test :refer :all]
            [tpbefunge.core :refer :all]))


(deftest test-crear-pila
  (testing "Crear una pila vacía"
    (is (= '() (crear-pila)))))

(deftest test-pila-apilar
  (testing "Testeando apilar en pila vacia"
    (let [pila (crear-pila)]
    (apilar pila 1)
    (is (= '(1) pila))))

  (testing "Testeando apilar en pila poblada"
    (let [pila (crear-pila)
          p2 (apilar pila 1)
          p3 (apilar p2 2)]
    (is (= '(2 1) p3))))
)