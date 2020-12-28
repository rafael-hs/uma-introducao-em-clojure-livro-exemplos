(ns exemplos-book.aridadeMultipla)

(def cotacoes
  {:yuan {:cotacao 2.15M :simbolo "¥"}
   :euro {:cotacao 0.28M :simbolo "€"}})

(def transacoes
  [{:valor 33.0M :tipo "despesa" :comentario "Almoço" :moeda "R$" :data "19/11/2016"}
   {:valor 2700.0M :tipo "receita" :comentario "Bico" :moeda "R$" :data "01/12/2016"}
   {:valor 29.0M :tipo "despesa" :comentario "Livro de Clojure" :moeda "R$" :data "03/12/2016"}])

(def transacoes
  [{:valor 33M :tipo "despesa" :comentario "Almoço"
    :moeda "R$" :data "19/11/2016"}
   {:valor 2700M :tipo "receita" :comentario "Bico"
    :moeda "R$" :data "01/12/2016"}
   {:valor 29M :tipo "despesa" :comentario "Livro de Clojure"
    :moeda "R$" :data "03/12/2016"}
   {:valor 45M :tipo "despesa" :comentario "Jogo no Steam"
    :moeda "R$" :data "26/12/2016"}])

(defn transacao-em-outra-moeda
  ([cotacoes moeda transacao]
   (let [{{cotacao :cotacao simbolo :simbolo} moeda} cotacoes]
     (assoc transacao :valor (* cotacao (:valor transacao))
            :moeda simbolo)))
  ([moeda transacao]
   (transacao-em-outra-moeda cotacoes  moeda transacao)))

(def membros-fundadores
  (list "Argentina" "Brasil" "Paraguai" "Uruguai"))
(def membros-plenos (cons "Venezuela" membros-fundadores))

(println membros-plenos)
(rest membros-plenos)

(identical? (rest membros-plenos)
            membros-fundadores)

(defn transacao-em-yuan [transacao]
  (let [{{cotacao :cotacao simbolo :simbolo} :yuan} cotacoes]
    (assoc transacao :valor (* cotacao (:valor transacao))
           :moeda simbolo)))

(transacao-em-yuan (first transacoes))

(def registros (atom ()))

(swap! registros conj {:valor 29M :tipo "despesa"
                       :comentario "Livro de Clojure" :moeda "R$"
                       :data "03/12/2016"})
(conj {:valor 29M :tipo "despesa" :comentario "Livro de Clojure"
       :moeda "R$" :data "03/12/2016"} ())

(swap! registros conj
       {:valor 2700M :tipo "receita" :comentario "Bico"
        :moeda "R$" :data "01/12/2016"})

(println registros)

(defn registrar [transacao]
  (swap! registros conj transacao))

(registrar {:valor 33M :tipo "despesa" :comentario "Almoço"
            :moeda "R$" :data "19/11/2016"})

(registrar {:valor 45M :tipo "despesa" :comentario "Jogo no Steam"
            :moeda "R$" :data "26/12/2016"})

(def transacoes @registros)

;; ------------ recursao
(defn despesa? [transacao]
  (= (:tipo transacao) "despesa"))

(defn saldo-acumulado1 [acumulado transacoes]
;; if-let
  (if-let [transacao (first transacoes)]
;; se _transacao_ existir, continue calculando o saldo
    (saldo-acumulado1 (if (despesa? transacao)
                        (- acumulado (:valor transacao))
                        (+ acumulado (:valor transacao)))
                      (rest transacoes))
;; se não existir, a coleção de transacoes acabou e é hora de
;; retornar o resultado
    acumulado))

(defn calcular [acumulado transacao]
  (let [valor (:valor transacao)]
    (if (despesa? transacao)
      (- acumulado valor)
      (+ acumulado valor))))

(defn saldo-acumulado2 [acumulado transacoes]
  (if-let [transacao (first transacoes)]
    (saldo-acumulado2 (calcular acumulado transacao)
                      (rest transacoes))
    acumulado))

(defn saldo-acumulado3 [acumulado transacoes]
  (if-let [transacao (first transacoes)]
    (do
      (prn "Começou saldo-acumulado. Saldo até agora:" acumulado)
      (saldo-acumulado3 (calcular acumulado transacao)
                        (rest transacoes)))
    acumulado))

(defn saldo-acumulado [acumulado transacoes]
  (if-let [transacao (first transacoes)]
    (do
      (prn "Começou saldo-acumulado. Saldo até agora:" acumulado)
      (saldo-acumulado (calcular acumulado transacao)
                       (rest transacoes)))
    (do
      (prn "Processo encerrado. Saldo final:" acumulado)
      acumulado)))

(saldo-acumulado 0 transacoes)
(saldo-acumulado 0 ())
(saldo-acumulado 0 (take 2 transacoes))

(defn saldo2 [transacoes]
  (saldo-acumulado 0 transacoes))

(saldo2 transacoes)

(defn saldo
;; caso a função receba só um argumento
  ([transacoes]
   (saldo 0 transacoes))
;; caso a função receba dois argumentos
  ([acumulado transacoes]
   (if-let [transacao (first transacoes)]
     (saldo (calcular acumulado transacao) (rest transacoes))
     acumulado)))

(saldo transacoes)

(defn como-transacao [valor]
  {:valor valor})

(def poucas-transacoes
  (map como-transacao (range 10)))

(def muitas-transacoes
  (map como-transacao (range 1000)))

(def incontaveis-transacoes
  (map como-transacao (range 100000)))

(saldo poucas-transacoes)
(saldo muitas-transacoes)
(saldo incontaveis-transacoes)

(defn saldo-tail-recursion
  ([transacoes] (saldo-tail-recursion 0 transacoes))

  ([acumulado transacoes]
   (if (empty? transacoes)
     acumulado
     (saldo-tail-recursion
      (calcular acumulado
                (first transacoes))
      (rest transacoes)))))

(saldo-tail-recursion poucas-transacoes)
(saldo-tail-recursion muitas-transacoes)
(saldo-tail-recursion incontaveis-transacoes)

;; nova versão de saldo, desta vez com recur
(defn saldo-tail-recursion-elemination
  ([transacoes] (saldo-tail-recursion-elemination 0 transacoes))
  ([acumulado transacoes]
   (if (empty? transacoes)
     acumulado
;;    vvvv aqui utilizamos recur em vez de saldo
     (recur (calcular acumulado (first transacoes))
            (rest transacoes)))))

(saldo-tail-recursion-elemination poucas-transacoes)
(saldo-tail-recursion-elemination muitas-transacoes)
(saldo-tail-recursion-elemination incontaveis-transacoes)
