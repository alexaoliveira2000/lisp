;;;; F�sica mec�nica como forma de calcular d�gitos do PI (descoberto por G. Galperin)

;;; Este exerc�cio foi feito com base no v�deo https://www.youtube.com/watch?v=HEfHFsfGXjs&ab_channel=3Blue1Brown
;;; Este programa � uma forma de computacionar os n�meros do PI atrav�s de colis�es el�sticas entre 2 corpos.
;;; Quanto maior for a massa do corpo da direita, mais colis�es haver�o, fazendo-nos aproximar do n�mero do PI. Contudo, este � um m�todo bastante dispendioso, pois o programa exige - exponencialmente - um grande processamento de colis�es e mem�ria stack (por�m n�o deixa de ser interessante).

;;; NOTA: O c�lculo de mais que 3 d�gitos do PI (pi-collisions 3), torna-se bastante demoroso...

;; Fun��o principal. O argumento corresponde ao n� de d�gitos do PI que queremos ver (ex. (pi-collisions 1) --> 3)
(defun pi-collisions (N)
  "Fun��o para devolver o n�mero de colis�es el�sticas entre dois objetos"
  (cond ((<= N 0) nil)
        (t (collision 0 (- 1) 1 (expt (expt 10 2) (- N 1)) 0))
))

;; Fun��o que calcula a exist�ncia de colis�o entre os 2 corpos, colis�o entre o corpo e a parede ou a n�o exist�ncia de colis�o
;; Se v1 < 0, o corpo da esquerda bate na parede (+1 colis�o)
;; Se v1 > 0 e v2 > 0 e v2 > v1, os corpos nunca mais v�o bater (fim)
(defun collision (v1 v2 m1 m2 res)
  "Fun��o recursiva para calcular as novas velocidades dos objetos ap�s colis�o, caso esta exista"
  (cond ((< v1 0) (collision (- v1) v2 m1 m2 (+ 1 res)))
        ((and (>= v1 0) (>= v2 0) (> v2 v1)) res)
        (t (collision (velocity v1 v2 m1 m2) (velocity v2 v1 m2 m1) m1 m2 (+ 1 res)))
))

;;;; Fun��o que faz o c�lculo de uma colis�o linear (com base na f�rmula da conserva��o de energia)
(defun velocity (v1 v2 m1 m2)
  "Fun��o gen�rica para calcular a velocidade de um corpo (momento linear ap�s colis�o)"
  (/ (+ (* 2 m2 v2) (* v1 (- m1 m2))) (+ m1 m2))
)