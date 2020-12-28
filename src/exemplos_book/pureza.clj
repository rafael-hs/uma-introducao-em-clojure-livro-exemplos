(ns exemplos-book.pureza
  (:require [clojure.string :as s]))

(def de-para [{:de "a" :para "4"}
              {:de "e" :para "3"}
              {:de "i" :para "1"}
              {:de "o" :para "0"}])

(defn escrita-hacker [texto dicionario]
  (if (empty? dicionario)
    texto
    (let [conversao (first dicionario)]
      (escrita-hacker (s/replace texto
                                 (:de conversao)
                                 (:para conversao))
                      (rest dicionario)))))

(escrita-hacker "alameda" de-para)

(def cotacoes
  {:yuan {:cotacao 2.15M :simbolo "¥"}
   :euro {:cotacao 0.28M :simbolo "€"}})

;;impuro
(defn transacao-em-outra-moeda-inpuro [moeda transacao]
  (let [{{cotacao :contacao simbolo :simbolo} moeda} cotacoes]
    (assoc transacao :valor (* cotacao (:valor transacao))
           :moeda simbolo)))

;; puro
(defn transacao-em-outra-moeda-pura [cotacoes moeda transacao]
  (let [{{cotacao :contacao simbolo :simbolo} moeda} cotacoes]
    (assoc transacao :valor (* cotacao (:valor transacao))
           :moeda simbolo)))
