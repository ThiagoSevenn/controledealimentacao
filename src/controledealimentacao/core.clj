(ns controledealimentacao.core
  (:require ;; conexao backend
            [controledealimentacao.auxiliar.auxiliares :refer :all]
            ;; frontend
            [controledealimentacao.exibicao.frontend :as frontend]
            [clj-http.client :as client])
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; programa 100%
  (frontend/programa-final)

)
