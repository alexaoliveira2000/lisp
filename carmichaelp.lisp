;;;; Função lambda de Carmichael (predicado)

;;; Este exercício foi feito com base no site https://brilliant.org/wiki/carmichaels-lambda-function/

;;; De forma sucinta, estes são os passos para validar se um número N é de Carmichael:
;;; 1 - Decompor o número N em fatores primos (função prime-decomposition)
;;; 2 - Compactar a decomposição, multiplicando os números primos iguais (função compact-decomposition)
;;; 3 - Aplicar a função phi de Euler a cada um dos elementos da lista compactada (função euler-phi-list)
;;; 4 - Descobrir o mínimo múltiplo comum dessa lista gerada (função mmc)
;;; 5 - Se (N-1) for divisível pelo mmc calculado - e se N não for primo - então N é um número de Carmichael

(defun carmichaelp (N)
  "Função que diz se um dado número N é um número de Carmichael"
  (cond ((<= N 4) nil)
        ( t (and (/= N (car (prime-decomposition N))) (zerop (mod (- N 1) (mmc (euler-phi-list (compact-decomposition (prime-decomposition N) 1)))))))))

(defun euler-phi-list (L)
  "Função que calcula o valores phi de uma lista e devolve a lista com esses valores"
  (cond ((null L) nil)
        (t (cons (euler-phi (car L)) (euler-phi-list (cdr L))))
))

(defun euler-phi (N)
  "Função que calcula o valor phi de um dado número N"
  (cond ((<= N 0) nil)
        (t (+ 1 (euler-phi-rec N 1)))
))

(defun euler-phi-rec (N I)
  "Função recursiva que conta a quantidade de coprimos até N"
  (cond ((= I N) 0)
        (t (+ (if (relatively-prime N I) 1 0) (euler-phi-rec N (+ I 1))))
))

(defun relatively-prime (X Y)
  "Função que verifica se 2 números são primos entre si"
  (relatively-prime-rec X Y 2)
)

(defun relatively-prime-rec (X Y N)
  "Função recursiva que verifica se 2 números são divisíveis por um número N"
  (cond ((or (< X 2) (< Y 2)) nil)
        ((= X Y) nil)
        ((and (= (mod X N) 0) (= (mod Y N) 0)) nil)
        ((or (and (> X Y) (> (* N N) X)) (and (> Y X) (> (* N N) Y))) t)
        (t (relatively-prime-rec X Y (+ N 1)))
))

(defun prime-decomposition(N)
  "Função que devolve uma lista da decomposição do número N em números primos"
  (prime-decomposition-rec N 2)
)

(defun prime-decomposition-rec(N F)
  "Função recursiva que valida se um número N é divisível por um número F"
  (cond ((<= N 1) nil)
        ((zerop (mod N F)) (cons F (prime-decomposition-rec (/ N F) F)))
        (t (prime-decomposition-rec N (+ F (if (= F 2) 1 2))))
))

(defun mmc (L)
  "Função que diz qual é o mínimo múltiplo comum (mmc) de uma lista L"
  (mmc-rec L (maximum L))
)

(defun mmc-rec (L I)
  "Função recursiva que devolve o número I que divide a lista inteira"
  (cond ((num-divides-list I L) I)
        (t (mmc-rec L (+ I 1)))
))

(defun num-divides-list (N L)
  "Função recursiva que valida se um número N divide todos os elementos de uma lista"
  (cond ((null L) t)
        ((zerop (mod N (car L))) (num-divides-list N (cdr L)))
        (t nil)
))

(defun compact-decomposition (L N)
  "Função recursiva que devolve uma lista compactada com os produtos dos números primos"
  (cond ((null L) nil)
        ((= N (car L)) (compact-decomposition (cdr L) N))
        (t (cons (first-prime-product L) (compact-decomposition (cdr L) (car L))))
))

(defun first-prime-product (L)
  "Função recursiva que calcula o produto do primeiro número primo de uma lista L"
  (cond ((null (car L)) 1)
        ((or (null (cadr L)) (/= (car L) (cadr L))) (car L))
        (t (* (car L) (first-prime-product (cdr L))))
))

(defun maximum (L)
  "Função recursiva que calcula o valor máximo de uma lista L"
 (cond ((null L) nil)
       ((null (cdr L)) (car L))
       ((> (car L) (maximum(cdr L))) (car L))
       (t (maximum(cdr L)))
))