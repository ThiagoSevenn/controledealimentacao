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

  (req-post	(endereco-para "/transacoes")(ganho "nome" 6002 100))  
  (req-post	(endereco-para "/transacoes")(ganho "nome" 6000 200))
  (req-post	(endereco-para "/transacoes")(perda "nome" 8000 30))
  (req-post	(endereco-para "/transacoes")(perda "nome" 8602 45))
  
  ;; programa 100%
  (frontend/programa (frontend/inicializar))

)
