(ns controledealimentacao.auxiliar.auxiliares
  (:require [clj-http.client :as client]
            [cheshire.core :as json]))

;; "reload" na tela, exibição.
(defn atualizarTela []
  (print "\u001b[2J\u001b[H")
  (flush))

;; leitura de inteiro. codigo 0 --> inicialização
(defn lerInteiro [str codigo]
  (let []
    (if (= 1 codigo)
      (printf "                              ")
      nil)
    (printf str)
    (flush)
    (Integer. (read-line))))

;; leitura de String. codigo 0 --> inicialização
(defn lerString [str codigo]
  (let []
    (if (= 1 codigo)
      (printf "                              ")
      nil)
    (printf str)
    (flush)
    (read-line)))

;; exibição.
(defn enquadrarParaExibicao [str]
  (printf "                              ")
  (println str))

(defn teto-piso []
  (println (apply str (repeat 80 "_"))))

;; Avalia as respostas se compreendem as alternativas dadas.
(defn avaliarResposta [resposta quantidadeAlternativas contador]
  (let []
    (if (= contador resposta)
        "sim"
        (if (= contador quantidadeAlternativas)
            "nao"
            (recur resposta quantidadeAlternativas (inc contador))))))

(defn	conteudo-como-json	[transacao]
		{:content-type	:json
			:body	(json/generate-string	transacao)
			:throw-exceptions	false})

(defn	perda	[nome calorias tempo]
		(conteudo-como-json	{:tipo "perda" :nome nome :tempo tempo :calorias calorias :data "data"}))

(defn	ganho	[nome calorias quantidade]
		(conteudo-como-json	{:tipo "ganho" :nome nome :quantidade quantidade :calorias calorias :data "data"}))

(def porta-padrao 3000)

(defn endereco-para	[rota] 
  (str "http://localhost:"
        porta-padrao rota))

(defn req-post [endereco dado]
	(client/post endereco dado))

(defn req-get [endereco]
	(:body (client/get endereco {:as :json})))

	;; traduzir

(defn traduzir-para-en [entrada]
  (let [url (format "https://api.mymemory.translated.net/get" )
				parametros {:query-params {"q" entrada
																	"langpair" "pt|en"}
										:as :json}
        resposta (client/get url parametros)
				traducao-bruta (:responseData (:body resposta))
				traducao (:translatedText traducao-bruta)]
        traducao))	

(defn exibir-dados-alimentos [alimento]
  (println (format 
            "Refeicao %s: Dia (%s) - %s g de %s (%s kcal)" 
            (:id alimento) 
            (:data alimento)
            (:quantidade alimento)
            (:nome alimento) 
            (:calorias alimento))))

(defn exibir-dados-exercicios [exercicio]
  (println (format 
            "Exercicio %s: Dia (%s) - %s minutos - %s (%s kcal)" 
            (:id exercicio) 
            (:data exercicio)
            (:tempo exercicio)
            (:nome exercicio) 
            (:calorias exercicio))))

(defn exibir-dados-registros [registro]
  (if (contains? registro :quantidade)
    (exibir-dados-alimentos registro)
    (exibir-dados-exercicios registro)))