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

;; Criação de um usuário.
(defn inicializar []
  (atualizarTela)
  (println "Inicializar o aplicativo, porfavor preencher dados...")
  (let [altura (lerInteiro "Altura (cm): " 0)
        peso (lerInteiro "Peso (Kg): " 0)
        idade (lerInteiro "Idade: " 0)
        sexo (lerString "Sexo (M - Masculino | F - Feminino): " 0)
        usuario {:altura altura :peso peso :idade idade :sexo sexo}]
    (req-post (endereco-para "/usuario") (conteudo-como-json usuario))
    usuario))

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

;; Escolhas do usuário
(defn segunda-alternativa []
  (teto-piso)
  (let [data (lerString "Data: " 0)
        nome (lerString "Qual alimento deseja cadastrar? " 0)
        quantidade (lerInteiro "Qual a quantidade do alimento informado: " 0)
        alimento {:quantidade quantidade :nome nome :data data}]
  (req-post	(endereco-para "/alimentos")(conteudo-como-json alimento))
  (println alimento)
  (if (= "S" (lerString "Deseja cadastrar novamente (S/N)? " 0))
    (recur)
    (teto-piso))))

(defn terceira-alternativa []
  (teto-piso)
  (let [data (lerString "Data: " 0)
        atividade (lerString "Qual a atividade fisica realizada? " 0)
        exercicio {:atividade atividade :data data}]
  (req-post	(endereco-para "/exercicios")(conteudo-como-json exercicio))
  (println exercicio)
  (if (= "S" (lerString "Deseja cadastrar novamente (S/N)? " 0))
    (recur)
    (teto-piso))))

;; consulta com filtro ou sem filtro
(defn quarta-alternativa [])

;; consulta de saldo com filtro ou sem filtro
(defn quinta-alternativa [])

;; Local onde recebe a interação do usuário com a aplicação e devolve funcionalidades.
(defn programa [usuario]
  (let []
    (interface usuario)
    (let [resposta (lerInteiro " Resposta : " 1)]
      (if (= "sim" (avaliarResposta resposta 5 1))
        (cond 
          (= 1 resposta) (recur (inicializar))
          (= 2 resposta) (let [] (segunda-alternativa) (recur usuario))
          :else (recur usuario))
        nil))))