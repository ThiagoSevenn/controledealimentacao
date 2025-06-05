(ns controledealimentacao.core
  (:require ;; conexao backend
            [controledealimentacao.auxiliar.auxiliares :refer :all]
            ;; frontend
            [controledealimentacao.exibicao.frontend :as frontend])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
    
  
  ;; (req-post	(endereco-para "/transacoes")(ganho "data" "nome" 2000))
  ;; (req-post	(endereco-para "/transacoes")(perda "data" "nome" 3000))
  (frontend/inicializar)
  (frontend/terceira-alternativa)
)
