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
;; FEITO ;;registrar alimento
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

;; falta detalhamento para o usuário escolher o exercicio ;;registrar atividade
;; formatar para usuario

(defn exibir [atividade]
  (println (format "%d - %s" (:id atividade) (:nome atividade))))

;; escolhido
(defn atividade-escolhida [atividade contador]
  (format "%s" (:nome atividade)))

(defn escolhido []
  (println "\nEscolha uma das atividades informadas: ")
  (doall(map exibir (first (:exercicios (req-get (endereco-para "/exercicios/propostos"))))))
  (let [exercicios (first (:exercicios (req-get (endereco-para "/exercicios/propostos"))))
        escolha (range 1 (+ 1 (lerInteiro "Resposta: " 0)) 1)
        exercicioEscolhido (last (map atividade-escolhida exercicios escolha))]
      exercicioEscolhido)) 

(defn terceira-alternativa []
  (teto-piso)
  (let [data (lerString "Data (dd/mm/yyyy): " 0)
        atividade (lerString "Qual a atividade fisica realizada? " 0)
        tempo (lerInteiro "Qual o tempo de duracao do exercicio (min)? " 0)]
  (req-post	(endereco-para "/exercicios/propostos")(conteudo-como-json {:atividade (traduzir-para-en atividade)}))
  (req-post	(endereco-para "/exercicios")(conteudo-como-json {:atividade (traduzir-para-en (escolhido)) :tempo tempo :data data}))
  (println (format 
            "\nAtividade registrada: Dia %s - %d minutos - %s (%s kcal)"
            data tempo 
            (:nome (last (:exercicios (req-get (endereco-para "/exercicios")))))
            (:calorias (last (:exercicios (req-get (endereco-para "/exercicios")))))))
  (if (= "S" (lerString "Deseja cadastrar novamente (S/N)? " 0))
    (recur)
    (teto-piso))))

;; FEITO ;; consulta com filtro ou geral -> alimento
(defn quarta-alternativa []
  (teto-piso)
  (let [filtro (lerString "Voce deseja aplicar filtro por data (S/N)? " 0)
        vetorAlimentos (ordenar-vetor 
                       (map alimento-data-int (:alimentos (req-get (endereco-para "/alimentos")))))]
    (if (= filtro "S")
      (let [dataInicial (lerString "Data inicial (dd/mm/yyyy): " 0)
            dataFinal (lerString "Data final (dd/mm/yyyy): " 0)
            vetorDataInicial (repeat (count vetorAlimentos) dataInicial)
            vetorDataFinal (repeat (count vetorAlimentos) dataFinal)]
            (println "\nAlimentos:")
            (doall (map exibir-dados-filtro-alimentos vetorAlimentos 
                                                      vetorDataInicial
                                                      vetorDataFinal)))
      (let []
          (println "\nAlimentos:")
          (doall (map exibir-dados-alimentos vetorAlimentos)))
    )
    (if (= "S" (lerString "\nDeseja consultar novamente (S/N)? " 0))
        (recur)
        (teto-piso))))

;; FEITO ;; consulta com filtro ou geral -> atividade
(defn quinta-alternativa []
  (teto-piso)
  (let [filtro (lerString "Voce deseja aplicar filtro por data (S/N)? " 0)
        vetorExercicios (ordenar-vetor 
                       (map exercicio-data-int (:exercicios (req-get (endereco-para "/exercicios")))))]
    (if (= filtro "S")
      (let [dataInicial (lerString "Data inicial (dd/mm/yyyy): " 0)
            dataFinal (lerString "Data final (dd/mm/yyyy): " 0)
            vetorDataInicial (repeat (count vetorExercicios) dataInicial)
            vetorDataFinal (repeat (count vetorExercicios) dataFinal)]
            (println "\nExercicios:")
            (doall (map exibir-dados-filtro-exercicios vetorExercicios 
                                                       vetorDataInicial
                                                       vetorDataFinal)))
      (let []
          (println "\nExercicios:")
          (doall (map exibir-dados-exercicios vetorExercicios)))
    )
    (if (= "S" (lerString "\nDeseja consultar novamente (S/N)? " 0))
        (recur)
        (teto-piso))))

;; FEITO ;; consulta com filtro ou geral -> registros
(defn sexta-alternativa []
  (teto-piso)
  (let [filtro (lerString "Voce deseja aplicar filtro por data (S/N)? " 0)
        vetorRegistros (ordenar-vetor 
                       (map registro-data-int (:registros (req-get (endereco-para "/registros")))))]
    (if (= filtro "S")
      (let [dataInicial (lerString "Data inicial (dd/mm/yyyy): " 0)
            dataFinal (lerString "Data final (dd/mm/yyyy): " 0)
            vetorDataInicial (repeat (count vetorRegistros) dataInicial)
            vetorDataFinal (repeat (count vetorRegistros) dataFinal)]
          (println "\nRegistros:")
          (doall (map exibir-dados-filtro-registros vetorRegistros 
                                                    vetorDataInicial
                                                    vetorDataFinal)))
      (let [vetorRegistros (:registros (req-get (endereco-para "/registros")))]
          (println "\nRegistros:")
          (doall (map exibir-dados-registros vetorRegistros)))
    )
    (if (= "S" (lerString "\nDeseja consultar novamente (S/N)? " 0))
        (recur)
        (teto-piso))))

;; FEITO ;; consultar saldos com filtro ou geral
(defn setima-alternativa []
  (teto-piso)
  (let [filtro (lerString "Voce deseja aplicar filtro por data (S/N)? " 0)]
    (if (= filtro "S")
      (let [vetorRegistros (ordenar-vetor 
                           (map registro-data-int (:registros (req-get (endereco-para "/registros")))))
            dataInicial (lerString "Data inicial (dd/mm/yyyy): " 0)
            dataFinal (lerString "Data final (dd/mm/yyyy): " 0)
            vetorDataInicial (repeat (count vetorRegistros) dataInicial)
            vetorDataFinal (repeat (count vetorRegistros) dataFinal)
            registrosFiltrados (doall (map dados-filtro-registros vetorRegistros 
                                                                  vetorDataInicial
                                                                  vetorDataFinal))
            saldoFiltrado (saldo-filtrado registrosFiltrados)
            texto (format "\nSeu saldo filtrado : %.1f kcal" saldoFiltrado)]
          (println texto))
      (let [saldoGeral (:saldo (req-get (endereco-para "/saldo")))
            texto (format "\nSeu saldo geral : %s kcal" saldoGeral)]
          (println texto))
    )
    (if (= "S" (lerString "\nDeseja consultar novamente (S/N)? " 0))
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

(defn verificar-usuario []
  (empty? (:usuario (req-get (endereco-para "/usuario")))))

(defn programa-final []
  (let [usuario (first (:usuario (req-get (endereco-para "/usuario"))))]
    (if (verificar-usuario)
      (programa (inicializar))
      (programa {:altura (:altura usuario) 
                  :peso (:peso usuario) 
                  :idade (:idade usuario) 
                  :sexo (:sexo usuario)}))))