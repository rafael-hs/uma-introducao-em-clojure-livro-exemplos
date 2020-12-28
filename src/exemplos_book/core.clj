(ns exemplos-book.core
  (:require [exemplos-book.financeiro :as financeiro])
  (:gen-class))

(def transacao {:valor 200 :tipo "receita"})
(def transacao-desnecessaria {:valor 34
                              :tipo "despesa"
                              :rotulos '("desnecessaria" "cartao")})
(def transacoes
  [{:valor 33.0
    :tipo "despesa"
    :comentario "Almoço"
    :moeda "R$"
    :data "19/11/2016"}
   {:valor 2700.0
    :tipo "receita"
    :comentario "Bico"
    :moeda "R$"
    :data "01/12/2016"}
   {:valor 123.0
    :tipo "despesa"
    :comentario "Livro de Clojure"
    :moeda "R$"
    :data "03/12/2016"}])

(filter financeiro/valor-grande? transacoes)

;(:rotulos transacao-desnecessaria)
;(:tipo transacao)

(defn valor-sinalizado [transacao]
  (if (= (:tipo transacao) "despesa")
    (str "-" (:valor transacao))
    (str "+" (:valor transacao))))
