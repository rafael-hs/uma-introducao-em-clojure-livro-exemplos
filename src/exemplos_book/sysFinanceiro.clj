(ns exemplos-book.sysFinanceiro
  (:gen-class)
  (:require [clojure.string :as s]))

;;data

(def transacoes
  [{:valor 33.0M :tipo "despesa" :comentario "Almoço" :moeda "R$" :data "19/11/2016"}
   {:valor 2700.0M :tipo "receita" :comentario "Bico" :moeda "R$" :data "01/12/2016"}
   {:valor 29.0M :tipo "despesa" :comentario "Livro de Clojure" :moeda "R$" :data "03/12/2016"}])

(def cotacoes
  {:yuan {:cotacao 2.15M :simbolo "¥"}
   :euro {:cotacao 0.28M :simbolo "€"}})

;;definitions
(defn valor-sinalizado [transacao]
  (let [moeda (:moeda transacao "R$")
        valor (:valor transacao)]
    (if (= (:tipo transacao) "despesa")
      (str moeda " -" valor)
      (str moeda " +" valor))))

(defn data-valor [transacao]
  (str (:data transacao) " => " (valor-sinalizado transacao)))

(defn transacao-em-yuan2 [transacao]
  (let [{{cotacao :cotacao simbolo :simbolo} :yuan} cotacoes]
    (assoc transacao :valor (* cotacao (:valor transacao))
           :moeda simbolo)))

(defn transacao-em-outra-moeda [moeda transacao]
  (let [{{cotacao :cotacao simbolo :simbolo} moeda} cotacoes]
    (assoc transacao :valor (* cotacao (:valor transacao))
           :moeda simbolo)))

;; tests

(def transacao-em-euro (partial transacao-em-outra-moeda :euro))
(def transacao-em-yuan (partial transacao-em-outra-moeda :yuan))

(defn texto-resumo-em-yuan [transacao]
  (-> (transacao-em-yuan transacao)
      (data-valor)))

(transacao-em-yuan (first transacoes))

(map transacao-em-yuan transacoes)
(map transacao-em-euro transacoes)

(s/join ", " (map texto-resumo-em-yuan transacoes))

(def juntar-tudo (partial s/join ", "))

(juntar-tudo (map texto-resumo-em-yuan transacoes))
