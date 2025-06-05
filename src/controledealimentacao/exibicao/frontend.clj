(ns controledealimentacao.exibicao.frontend
  (:require [controledealimentacao.auxiliar.auxiliares :refer :all]))

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

;; Avalia as respostas se compreendem as alternativas dadas.
(defn avaliarResposta [resposta quantidadeAlternativas contador]
  (let []
    (if (= contador resposta)
        "sim"
        (if (= contador quantidadeAlternativas)
            "nao"
            (recur resposta quantidadeAlternativas (inc contador))))))

;; Criação de um usuário.
(defn inicializar []
  (atualizarTela)
  (println "Inicializar o aplicativo, porfavor preencher dados...")
  (let [altura (lerInteiro "Altura (cm): " 0)
        peso (lerInteiro "Peso (Kg): " 0)
        idade (lerInteiro "Idade: " 0)
        sexo (lerString "Sexo (M - Masculino | F - Feminino): " 0)
        objeto {:altura altura :peso peso :idade idade :sexo sexo}]
    (req-post (endereco-para "/usuario") (conteudo-como-json objeto))))

;; Interface de exibição
(defn interface [usuario]
  (atualizarTela)
  (let [centralizar (repeat 5 "")]
    (doall(map println centralizar))
    (enquadrarParaExibicao "____________________________________________________________")
    (enquadrarParaExibicao "|                         BEM VINDO                         |")
    (enquadrarParaExibicao "|                            AO                             |")
    (enquadrarParaExibicao "|                    APP ('nome do app')                    |")
    (enquadrarParaExibicao (format "| Altura: %d cm   Peso: %d Kg   Idade: %d anos   Sexo: %s   |" (:altura usuario) (:peso usuario) (:idade usuario) (:sexo usuario)))
    (enquadrarParaExibicao "|___________________________________________________________|")
    (enquadrarParaExibicao "| Escolha a alternativa que deseja.                         |")
    (enquadrarParaExibicao "| 1 - Alterar dados informados ao iniciar o programa.       |")
    (enquadrarParaExibicao "| 2 - Registrar consumo de alimento.                        |")
    (enquadrarParaExibicao "| 3 - Registrar realizacao de atividade fisica.             |")
    (enquadrarParaExibicao "| 4 - Visualizar os consumos e/ou os gastos caloricos.      |")
    (enquadrarParaExibicao "| 5 - Consultar saldo de calorias.                          |")
    (enquadrarParaExibicao "| 0 - Sair.                                                 |")
    (enquadrarParaExibicao "|___________________________________________________________|")))

;; Local onde recebe a interação do usuário com a aplicação e devolve funcionalidades.
(defn programa [usuario]
  (let []
    (interface usuario)
    (let [resposta (lerInteiro " Resposta : " 1)]
      (if (= "sim" (avaliarResposta resposta 5 1))
        (cond 
          (= 1 resposta) (recur (inicializar))
          :else (recur usuario))
        nil))))