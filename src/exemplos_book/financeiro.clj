(ns exemplos-book.financeiro
  (:require [clojure.string :as s]))

(def transacoes
  [{:valor 33.0 :tipo "despesa" :comentario "Almoço" :data "19/11/2016"}
   {:valor 2700.0 :tipo "receita" :comentario "Bico" :data "01/12/2016"}
   {:valor 29.0 :tipo "despesa" :comentario "Livro de Clojure" :data "03/12/2016"}];; verifica se a transacao é do tipo despesa
(defn despesa? [transacao]
  (= (:tipo transacao) "despesa"))

;; cria resumo de uma transacao
(defn resumo [transacao]
  (select-keys transacao [:valor :tipo :data]))

;; pega só o valor das transacoes
(defn soma-valor [transacao]
  (:valor transacao))

;; verifica se o valor da transacao é maior que 100
(defn valor-grande? [transacao]
  (> (:valor transacao) 100))

;; filtra valores acima de 100
;;(filter valor-grande? transacoes)

;; filtra valores acima de 100 sem a funcao valor-grande?
(filter (fn [transacao]
          (> (:valor transacao) 100))
        transacoes)
;;ou
(filter #(> (:valor %) 100) transacoes)

;; filtra um conjunto de valores de acordo com uma função
(filter despesa? transacoes)

;; mapea um conjunto de valores de acordo com uma função
(map resumo transacoes)

;; pega só os valores das despesas
(map soma-valor (filter despesa? transacoes))

;; soma todos os valores das despesas
(reduce +
        (map soma-valor
             (filter despesa?
                     transacoes)))
;; resumo disso acima
(reduce + (map #(:valor %)
               (filter #(= (:tipo %) "despesa")
                       transacoes)))

(soma-valor (first transacoes))

(-> (first transacoes)
    (soma-valor))

(def transacoes
  [{:valor 33.0M
    :tipo "despesa"
    :comentario "Almoço"
    :moeda "R$"
    :data "19/11/2016"}
   {:valor 2700.0M
    :tipo "receita"
    :comentario "Bico"
    :moeda "R$"
    :data "01/12/2016"}
   {:valor 29.0M
    :tipo "despesa"
    :comentario "Livro de Clojure"
    :moeda "R$"
    :data "03/12/2016"}])

(comment
  (defn valor-sinalizado1 [transacao]
    (if (= (:tipo transacao) " despesa ")
      (str (:moeda transacao) " - " (:valor transacao))
      (str (:moeda transacao) " + " (:valor transacao)))))

(defn valor-sinalizado [transacao]
  (let [moeda (:moeda transacao "R$")
        valor (:valor transacao)]
    (if (= (:tipo transacao) "despesa")
      (str moeda " -" valor)
      (str moeda " +" valor))))

(valor-sinalizado (first transacoes))
(valor-sinalizado (second transacoes))

(def transacao-aleatoria {:valor 9.0})

(valor-sinalizado transacao-aleatoria)

(defn data-valor [transacao]
  (str (:data transacao) " => " (valor-sinalizado transacao)))

(data-valor (first transacoes))

(def cotacoes
  {:yuan {:cotacao 2.15M :simbolo "¥"}
   :euro {:cotacao 0.28M :simbolo "€"}})

;;(defn transacao-em-yuan [transacao]
;;  (assoc transacao :valor (* (:cotacao (:yuan cotacoes))
;;                             (:valor transacao))
;;        :moeda (:simbolo (:yuan cotacoes))))

;(defn transacao-em-yuan [transacao]
;  (assoc transacao :valor (* (get-in cotacoes [:yuan :cotacao])
;                             (:valor transacao))
;         :moeda (get-in cotacoes [:yuan :simbolo])))

(defn transacao-em-yuan [transacao]
  (let [yuan (:yuan cotacoes)]
    (assoc transacao :valor (* (:cotacao yuan (:valor transacao))
                               (:valor transacao))
           :moeda (:simbolo yuan))))

(data-valor (first transacoes))
(data-valor (transacao-em-yuan (first transacoes)))

(transacao-em-yuan (first transacoes))

(defn texto-resumo-em-yuan [transacao]
  (-> (transacao-em-yuan transacao)
      (data-valor)))

;;(map texto-resumo-em-yuan transacoes)

;;(def texto-resumo-em-yuan (comp data-valor transacao-em-yuan))

;;(map texto-resumo-em-yuan transacoes)

(defn transacao-em-yuan2 [transacao]
  (let [{{cotacao :cotacao simbolo :simbolo} :yuan} cotacoes]
    (assoc transacao :valor (* cotacao (:valor transacao))
           :moeda simbolo)))

(transacao-em-yuan2 (first transacoes))

(defn transacao-em-outra-moeda [moeda transacao]
  (let [{{cotacao :cotacao simbolo :simbolo} moeda} cotacoes]
    (assoc transacao :valor (* cotacao (:valor transacao))
           :moeda simbolo)))

(transacao-em-outra-moeda :euro (first transacoes))
(transacao-em-outra-moeda :yuan (first transacoes))
