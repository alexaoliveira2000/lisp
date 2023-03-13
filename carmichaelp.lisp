;;;; Fun��o lambda de Carmichael (predicado)

;;; Este exerc�cio foi feito com base no site https://brilliant.org/wiki/carmichaels-lambda-function/

;;; De forma sucinta, estes s�o os passos para validar se um n�mero N � de Carmichael:
;;; 1 - Decompor o n�mero N em fatores primos (fun��o prime-decomposition)
;;; 2 - Compactar a decomposi��o, multiplicando os n�meros primos iguais (fun��o compact-decomposition)
;;; 3 - Aplicar a fun��o phi de Euler a cada um dos elementos da lista compactada (fun��o euler-phi-list)
;;; 4 - Descobrir o m�nimo m�ltiplo comum dessa lista gerada (fun��o mmc)
;;; 5 - Se (N-1) for divis�vel pelo mmc calculado - e se N n�o for primo - ent�o N � um n�mero de Carmichael

(defun carmichaelp (N)
  "Fun��o que diz se um dado n�mero N � um n�mero de Carmichael"
  (cond ((<= N 4) nil)
        ( t (and (/= N (car (prime-decomposition N))) (zerop (mod (- N 1) (mmc (euler-phi-list (compact-decomposition (prime-decomposition N) 1)))))))))

(defun euler-phi-list (L)
  "Fun��o que calcula o valores phi de uma lista e devolve a lista com esses valores"
  (cond ((null L) nil)
        (t (cons (euler-phi (car L)) (euler-phi-list (cdr L))))
))

(defun euler-phi (N)
  "Fun��o que calcula o valor phi de um dado n�mero N"
  (cond ((<= N 0) nil)
        (t (+ 1 (euler-phi-rec N 1)))
))

(defun euler-phi-rec (N I)
  "Fun��o recursiva que conta a quantidade de coprimos at� N"
  (cond ((= I N) 0)
        (t (+ (if (relatively-prime N I) 1 0) (euler-phi-rec N (+ I 1))))
))

(defun relatively-prime (X Y)
  "Fun��o que verifica se 2 n�meros s�o primos entre si"
  (relatively-prime-rec X Y 2)
)

(defun relatively-prime-rec (X Y N)
  "Fun��o recursiva que verifica se 2 n�meros s�o divis�veis por um n�mero N"
  (cond ((or (< X 2) (< Y 2)) nil)
        ((= X Y) nil)
        ((and (= (mod X N) 0) (= (mod Y N) 0)) nil)
        ((or (and (> X Y) (> (* N N) X)) (and (> Y X) (> (* N N) Y))) t)
        (t (relatively-prime-rec X Y (+ N 1)))
))

(defun prime-decomposition(N)
  "Fun��o que devolve uma lista da decomposi��o do n�mero N em n�meros primos"
  (prime-decomposition-rec N 2)
)

(defun prime-decomposition-rec(N F)
  "Fun��o recursiva que valida se um n�mero N � divis�vel por um n�mero F"
  (cond ((<= N 1) nil)
        ((zerop (mod N F)) (cons F (prime-decomposition-rec (/ N F) F)))
        (t (prime-decomposition-rec N (+ F (if (= F 2) 1 2))))
))

(defun mmc (L)
  "Fun��o que diz qual � o m�nimo m�ltiplo comum (mmc) de uma lista L"
  (mmc-rec L (maximum L))
)

(defun mmc-rec (L I)
  "Fun��o recursiva que devolve o n�mero I que divide a lista inteira"
  (cond ((num-divides-list I L) I)
        (t (mmc-rec L (+ I 1)))
))

(defun num-divides-list (N L)
  "Fun��o recursiva que valida se um n�mero N divide todos os elementos de uma lista"
  (cond ((null L) t)
        ((zerop (mod N (car L))) (num-divides-list N (cdr L)))
        (t nil)
))

(defun compact-decomposition (L N)
  "Fun��o recursiva que devolve uma lista compactada com os produtos dos n�meros primos"
  (cond ((null L) nil)
        ((= N (car L)) (compact-decomposition (cdr L) N))
        (t (cons (first-prime-product L) (compact-decomposition (cdr L) (car L))))
))

(defun first-prime-product (L)
  "Fun��o recursiva que calcula o produto do primeiro n�mero primo de uma lista L"
  (cond ((null (car L)) 1)
        ((or (null (cadr L)) (/= (car L) (cadr L))) (car L))
        (t (* (car L) (first-prime-product (cdr L))))
))

(defun maximum (L)
  "Fun��o recursiva que calcula o valor m�ximo de uma lista L"
 (cond ((null L) nil)
       ((null (cdr L)) (car L))
       ((> (car L) (maximum(cdr L))) (car L))
       (t (maximum(cdr L)))
))