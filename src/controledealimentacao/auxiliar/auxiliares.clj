(ns controledealimentacao.auxiliar.auxiliares
  (:require [clj-http.client :as client]
            [cheshire.core :as json]))

(defn	conteudo-como-json	[transacao]
		{:content-type	:json
			:body	(json/generate-string	transacao)
			:throw-exceptions	false})

(defn	perda	[data nome valor]
		(conteudo-como-json	{:data data :nome nome :valor	valor	:tipo	"perda"}))

(defn	ganho	[data nome valor]
		(conteudo-como-json	{:data data :nome nome :valor valor	:tipo	"ganho"}))

(def porta-padrao 3001)

(defn endereco-para	[rota] 
  (str "http://localhost:"
        porta-padrao rota))

(defn req-post [endereco dado]
	(client/post endereco dado))

(defn req-get [endereco requisicao]
	(client/get endereco requisicao))	