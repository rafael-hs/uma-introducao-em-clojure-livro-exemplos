(ns exemplos-book.lazyness)

(defn test-da-preguica []
  (prn "não deveria aparecer nada aqui") "nada")

(defn um-oi [a b]
  "oi")

(def transacoes
  [{:valor 33.0M :tipo "despesa" :comentario "Almoço" :moeda "R$" :data "19/11/2016"}
   {:valor 2700.0M :tipo "receita" :comentario "Bico" :moeda "R$" :data "01/12/2016"}
   {:valor 29.0M :tipo "despesa" :comentario "Livro de Clojure" :moeda "R$" :data "03/12/2016"}])

(um-oi (test-da-preguica) (test-da-preguica))

;; rand-nth retorna um valor aleatório dentro de uma coleção
(rand-nth ["despesa" "receita"])
;; "despesa"
;; seu resultado pode ser diferente

;; rand-int retorna um número inteiro aleatório entre 0 e
;; o argumento, sem incluir o argumento nas possibilidades
;; daí multiplicamos por 0.01M para ter um número real com
;; duas casas decimais
(* (rand-int 100001) 0.01M)
;; 243.85M
;; seu resultado pode ser diferente

(defn transacao-aleatoria []
  {:valor (* (rand-int 100001) 0.01M)
   :tipo (rand-nth ["despesa" "receita"])})

(transacao-aleatoria)

(repeatedly 3 transacao-aleatoria)

(class (repeatedly 3 transacao-aleatoria))

(def transacoes-aleatorias (repeatedly transacao-aleatoria))

(take 1 transacoes-aleatorias)
(take 3 transacoes-aleatorias)
(take 5 transacoes-aleatorias)
(= (take 5 transacoes-aleatorias) (take 5 transacoes-aleatorias))
(take 50 transacoes-aleatorias)

(cons (transacao-aleatoria) transacoes)

(defn aleatorias2
;; começando a sequência criando a primeira transação
  ([quantidade]
   (aleatorias2 quantidade 1 (list (transacao-aleatoria))))
;; incrementando a sequência até que a quantidade desejada
;; seja atendida
  ([quantidade quantas-ja-foram transacoes]
   (if (< quantas-ja-foram quantidade)
     (aleatorias2 quantidade (inc quantas-ja-foram)
                  (cons (transacao-aleatoria)
                        transacoes))
     transacoes)))

(aleatorias2 4)
(aleatorias2 900000)

(defn aleatorias3
  ([quantidade]
   (aleatorias3 quantidade 1 (list (transacao-aleatoria))))
  ([quantidade quantas-ja-foram transacoes]
   (if (= quantas-ja-foram quantidade)
     transacoes
;; aplicando a otimização na cauda
     (recur quantidade (inc quantas-ja-foram)
            (cons (transacao-aleatoria) transacoes)))))

(defn aleatorias
  ([quantidade]
   (aleatorias quantidade 1 (list (transacao-aleatoria))))
  ([quantidade quantas-ja-foram transacoes]
 ;; vamos embalar a parte recursiva na macro lazy-seq
   (lazy-seq
    (if (= quantas-ja-foram quantidade)
      transacoes
      (aleatorias quantidade (inc quantas-ja-foram)
                  (cons (transacao-aleatoria)
                        transacoes))))))

(aleatorias 4)
(aleatorias 900000)

(class (aleatorias 4))
(class (aleatorias 900000))

(time (class (aleatorias 4)))
(time (class (aleatorias 900000)))

(defn aleatorias-infi []
  (lazy-seq
   (cons (transacao-aleatoria) (aleatorias-infi))))

(time (class (take 4 (aleatorias-infi))))
(time (class (take 900000 (aleatorias-infi))))

(take 4 (aleatorias-infi))
(take 900000 (aleatorias-infi))
