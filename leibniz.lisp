;;;; Calcular uma aproxima��o do PI atrav�s da f�rmula de Leibniz (1 - 1/3 + 1/5 - 1/7 + 1/9 - ... = PI/4)

;;; Ao manipular a f�rmula de Leibniz podemos chegar a um algoritmo recursivo:
;;; PI = S(i=0 --> n) {
;;;    4/(2i + 1)   , se i%2 = 0
;;;   -4/(2i + 1)   , c.c.
;;; }

;;; Este programa foi feito de 2 formas recursivas muito semelhantes:
;;; leibniz - recebe um argumento "n", que corresponde ao limite de itera��es (chama a fun��o leibniz-rec, que vai transportar o resultado entre chamadas da fun��o)
;;; leibniz-overflow - igual � fun��o leibniz-rec, mas o resultado n�o � transportado entre chamadas (da� ocorrer overflow se n for muito grande); � uma fun��o puramente recursiva

;; (leibniz 10) > 3.2323158
;; (leibniz 100) > 3.1514933
;; (leibniz 1000) > 3.1425917

(defun leibniz (n)
  "Fun��o que retorna uma aproxima��o do PI atrav�s da f�rmula de Leibniz"
  (cond ((< n 0) nil)
        (t (float (leibniz-rec n 0)))
))

(defun leibniz-rec (n res)
  "Fun��o recursiva que calcula uma aproxima��o do PI (argumento res)"
  (cond ((< n 0) res)
        ((zerop (mod n 2)) (leibniz-rec (1- n) (+ res (/ 4 (+ (* n 2) 1)))))
        (t (leibniz-rec (1- n) (- res (/ 4 (+ (* n 2) 1)))))
))

(defun leibniz-overflow (n)
  "Fun��o recursiva que calcula uma aproxima��o do PI atrav�s da f�rmula de Leibniz"
  (cond ((< n 0) 0)
        ((zerop (mod n 2)) (float (+ (/ 4 (+ (* n 2) 1)) (leibniz-overflow (1- n)))))
        (t (float (+ (- (/ 4 (+ (* n 2) 1))) (leibniz-overflow (1- n)))))
))