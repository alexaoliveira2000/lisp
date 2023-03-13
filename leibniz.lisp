;;;; Calcular uma aproximação do PI através da fórmula de Leibniz (1 - 1/3 + 1/5 - 1/7 + 1/9 - ... = PI/4)

;;; Ao manipular a fórmula de Leibniz podemos chegar a um algoritmo recursivo:
;;; PI = S(i=0 --> n) {
;;;    4/(2i + 1)   , se i%2 = 0
;;;   -4/(2i + 1)   , c.c.
;;; }

;;; Este programa foi feito de 2 formas recursivas muito semelhantes:
;;; leibniz - recebe um argumento "n", que corresponde ao limite de iterações (chama a função leibniz-rec, que vai transportar o resultado entre chamadas da função)
;;; leibniz-overflow - igual à função leibniz-rec, mas o resultado não é transportado entre chamadas (daí ocorrer overflow se n for muito grande); é uma função puramente recursiva

;; (leibniz 10) > 3.2323158
;; (leibniz 100) > 3.1514933
;; (leibniz 1000) > 3.1425917

(defun leibniz (n)
  "Função que retorna uma aproximação do PI através da fórmula de Leibniz"
  (cond ((< n 0) nil)
        (t (float (leibniz-rec n 0)))
))

(defun leibniz-rec (n res)
  "Função recursiva que calcula uma aproximação do PI (argumento res)"
  (cond ((< n 0) res)
        ((zerop (mod n 2)) (leibniz-rec (1- n) (+ res (/ 4 (+ (* n 2) 1)))))
        (t (leibniz-rec (1- n) (- res (/ 4 (+ (* n 2) 1)))))
))

(defun leibniz-overflow (n)
  "Função recursiva que calcula uma aproximação do PI através da fórmula de Leibniz"
  (cond ((< n 0) 0)
        ((zerop (mod n 2)) (float (+ (/ 4 (+ (* n 2) 1)) (leibniz-overflow (1- n)))))
        (t (float (+ (- (/ 4 (+ (* n 2) 1))) (leibniz-overflow (1- n)))))
))