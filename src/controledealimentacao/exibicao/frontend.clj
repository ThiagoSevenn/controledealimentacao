(ns controledealimentacao.exibicao.frontend
  (:require [controledealimentacao.auxiliar.auxiliares :refer :all]))

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
    (enquadrarParaExibicao "| 4 - Consultar os alimentos registrados.                   |")
    (enquadrarParaExibicao "| 5 - Consultar as atividades fisicas registradas.          |")
    (enquadrarParaExibicao "| 6 - Consultar todos os registros.                         |")
    (enquadrarParaExibicao "| 7 - Consultar saldo.                                      |")
    (enquadrarParaExibicao "| 0 - Sair.                                                 |")
    (enquadrarParaExibicao "|___________________________________________________________|")))

;; Escolhas do usuário
;;feito ;;registrar alimento
(defn segunda-alternativa []
  (teto-piso)
  (let [data (lerString "Data (dd/mm/yyyy): " 0)
        nome (lerString "Qual alimento deseja cadastrar? " 0)
        quantidade (lerInteiro "Qual a quantidade do alimento informado (g)? " 0)
        alimento {:quantidade quantidade :nome (traduzir-para-en nome) :data data}]
  (req-post	(endereco-para "/alimentos")(conteudo-como-json alimento))
  (println (format 
            "Refeição registrada: Dia %s - %d g de %s (%s kcal)"
            data quantidade nome
            (:calorias (last (:alimentos (req-get (endereco-para "/alimentos")))))))
  (if (= "S" (lerString "Deseja cadastrar novamente (S/N)? " 0))
    (recur)
    (teto-piso))))

;;falta detalhamento para o usuário escolher o exercicio ;;registrar atividade
(defn terceira-alternativa []
  (teto-piso)
  (let [data (lerString "Data (dd/mm/yyyy): " 0)
        atividade (lerString "Qual a atividade fisica realizada? " 0)
        tempo (lerInteiro "Qual o tempo de duracao do exercicio (min)? " 0)
        exercicio {:atividade (traduzir-para-en atividade) :tempo tempo :data data}]
  (req-post	(endereco-para "/exercicios")(conteudo-como-json exercicio))
  (println (format 
            "Atividade registrada: Dia %s - %d minutos - %s (%s kcal)"
            data tempo atividade 
            (:calorias (last (:exercicios (req-get (endereco-para "/exercicios")))))))
  (if (= "S" (lerString "Deseja cadastrar novamente (S/N)? " 0))
    (recur)
    (teto-piso))))

;; metade - faltam os filtros ;; consulta com filtro ou geral -> alimento
(defn quarta-alternativa []
  (teto-piso)
  (let [filtro (lerString "Voce deseja aplicar filtro por data (S/N)? " 0)]
    (if (= filtro "S")
      (let [dataInicial (lerString "Data inicial (dd/mm/yyyy): " 0)
            dataFinal (lerString "Data final (dd/mm/yyyy): " 0)])
      (let [vetorAlimentos (:alimentos (req-get (endereco-para "/alimentos")))]
          (println "Alimentos:")
          (doall (map exibir-dados-alimentos vetorAlimentos)))
    )
    (if (= "S" (lerString "Deseja consultar novamente (S/N)? " 0))
        (recur)
        (teto-piso))))

;; metade - faltam os filtros ;; consulta com filtro ou geral -> atividade
(defn quinta-alternativa []
  (teto-piso)
  (let [filtro (lerString "Voce deseja aplicar filtro por data (S/N)? " 0)]
    (if (= filtro "S")
      (let [dataInicial (lerString "Data inicial (dd/mm/yyyy): " 0)
            dataFinal (lerString "Data final (dd/mm/yyyy): " 0)])
      (let [vetorExercicios (:exercicios (req-get (endereco-para "/exercicios")))]
          (println "Exercicios:")
          (doall (map exibir-dados-exercicios vetorExercicios)))
    )
    (if (= "S" (lerString "Deseja consultar novamente (S/N)? " 0))
        (recur)
        (teto-piso))))

;; metade - faltam os filtros ;; consulta com filtro ou geral -> registros
(defn sexta-alternativa []
  (teto-piso)
  (let [filtro (lerString "Voce deseja aplicar filtro por data (S/N)? " 0)]
    (if (= filtro "S")
      (let [dataInicial (lerString "Data inicial (dd/mm/yyyy): " 0)
            dataFinal (lerString "Data final (dd/mm/yyyy): " 0)])
      (let [vetorRegistros (:registros (req-get (endereco-para "/registros")))]
          (println "Registros:")
          (doall (map exibir-dados-registros vetorRegistros)))
    )
    (if (= "S" (lerString "Deseja consultar novamente (S/N)? " 0))
        (recur)
        (teto-piso))))

;; metade - faltam os filtros ;; consultar saldos com filtro ou geral
(defn setima-alternativa []
  (teto-piso)
  (let [filtro (lerString "Voce deseja aplicar filtro por data (S/N)? " 0)]
    (if (= filtro "S")
      (let [dataInicial (lerString "Data inicial (dd/mm/yyyy): " 0)
            dataFinal (lerString "Data final (dd/mm/yyyy): " 0)])
      (let [saldoGeral (:saldo (req-get (endereco-para "/saldo")))
            texto (format "Seu saldo geral : %s kcal" saldoGeral)]
          (println texto))
    )
    (if (= "S" (lerString "Deseja consultar novamente (S/N)? " 0))
        (recur)
        (teto-piso))))

;; Local onde recebe a interação do usuário com a aplicação e devolve funcionalidades.
(defn programa [usuario]
  (let []
    (interface usuario)
    (let [resposta (lerInteiro " Resposta : " 1)]
      (if (= "sim" (avaliarResposta resposta 7 1))
        (cond 
          (= 1 resposta) (recur (inicializar))
          (= 2 resposta) (let [] (segunda-alternativa) (recur usuario))
          (= 3 resposta) (let [] (terceira-alternativa) (recur usuario))
          (= 4 resposta) (let [] (quarta-alternativa) (recur usuario))
          (= 5 resposta) (let [] (quinta-alternativa) (recur usuario))
          (= 6 resposta) (let [] (sexta-alternativa) (recur usuario))
          (= 7 resposta) (let [] (setima-alternativa) (recur usuario))
          :else (recur usuario))
        nil))))