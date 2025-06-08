(ns controledealimentacao.auxiliar.auxiliares
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as str]))

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

(defn	perda	[nome calorias tempo data]
		(conteudo-como-json	{:tipo "perda" :nome nome :tempo tempo :calorias calorias :data data}))

(defn	ganho	[nome calorias quantidade data]
		(conteudo-como-json	{:tipo "ganho" :nome nome :quantidade quantidade :calorias calorias :data data}))

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

(defn dados-registros [registro]
  {:tipo (:tipo registro) :calorias (:calorias registro)})
;; transforma a data em um tipo int formatado
(defn formatar-data [dataBruta]
    (let[data (reverse (str/split dataBruta #"/"))
         vetorInt (doall (map #(Integer/parseInt %) data))
         vetorMultiplicador [10000 100 1]
         dataInt (apply + (doall (map #(* %1 %2) vetorInt vetorMultiplicador)))]
        dataInt))

(defn alimento-data-int [alimento]
  {:tipo "ganho" :id (:id alimento)
                 :nome (:nome alimento) 
                 :quantidade (:quantidade alimento)
                 :calorias (:calorias alimento)
                 :data (:data alimento)
                 :dataInt (formatar-data (:data alimento))})

(defn exercicio-data-int [exercicio]
  {:tipo "perda" :id (:id exercicio)
                 :nome (:nome exercicio) 
                 :tempo (:tempo exercicio)
                 :calorias (:calorias exercicio)
                 :data (:data exercicio)
                 :dataInt (formatar-data (:data exercicio))})

(defn registro-data-int [registro]
  (if (contains? registro :quantidade)
    (alimento-data-int registro)
    (exercicio-data-int registro)))

(defn ordenar-vetor [vetor]
  (sort-by :dataInt vetor))

(defn exibir-dados-filtro-alimentos [alimento dataInicial dataFinal]
  (let [dataAlimento (formatar-data (:data alimento))
        dataInicialInt (formatar-data dataInicial)
        dataFinalInt (formatar-data dataFinal)]
    (if (and (>= dataAlimento dataInicialInt) 
             (<= dataAlimento dataFinalInt))
      (exibir-dados-alimentos alimento)
      nil)))

(defn exibir-dados-filtro-exercicios [exercicio dataInicial dataFinal]
  (let [dataExercicio (formatar-data (:data exercicio))
        dataInicialInt (formatar-data dataInicial)
        dataFinalInt (formatar-data dataFinal)]
    (if (and (>= dataExercicio dataInicialInt) 
             (<= dataExercicio dataFinalInt))
      (exibir-dados-exercicios exercicio)
      nil)))

(defn exibir-dados-filtro-registros [registro dataInicial dataFinal]
  (let [dataRegistro (formatar-data (:data registro))
        dataInicialInt (formatar-data dataInicial)
        dataFinalInt (formatar-data dataFinal)]
    (if (and (>= dataRegistro dataInicialInt) 
             (<= dataRegistro dataFinalInt))
      (exibir-dados-registros registro)
      nil)))

(defn dados-filtro-registros [registro dataInicial dataFinal]
  (let [dataRegistro (formatar-data (:data registro))
        dataInicialInt (formatar-data dataInicial)
        dataFinalInt (formatar-data dataFinal)]
    (if (and (>= dataRegistro dataInicialInt) 
             (<= dataRegistro dataFinalInt))
      (dados-registros registro)
      nil)))

(defn filtro-tipo [registro]
  (if (= "ganho" (:tipo registro))
    (:calorias registro)
    (if (not= registro nil)
      (- 0 (:calorias registro))
      0)))

(defn saldo-filtrado [registros]
  (apply + (doall (map filtro-tipo registros))))