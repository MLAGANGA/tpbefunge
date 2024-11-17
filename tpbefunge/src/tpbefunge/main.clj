(ns tpbefunge.main
  (:require [tpbefunge.core :refer [ejecutar]])
  (:gen-class))

(defn -main [& args]
  (if (or (empty? args) (not (clojure.string/ends-with? (first args) ".bf")))
    (println "ERROR: Escriba un archivo.bf como entrada a la consola para la ejecucion")
    (ejecutar (first args))))
