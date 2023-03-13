;;;; Física mecânica como forma de calcular dígitos do PI (descoberto por G. Galperin)

;;; Este exercício foi feito com base no vídeo https://www.youtube.com/watch?v=HEfHFsfGXjs&ab_channel=3Blue1Brown
;;; Este programa é uma forma de computacionar os números do PI através de colisões elásticas entre 2 corpos.
;;; Quanto maior for a massa do corpo da direita, mais colisões haverão, fazendo-nos aproximar do número do PI. Contudo, este é um método bastante dispendioso, pois o programa exige - exponencialmente - um grande processamento de colisões e memória stack (porém não deixa de ser interessante).

;;; NOTA: O cálculo de mais que 3 dígitos do PI (pi-collisions 3), torna-se bastante demoroso...

;; Função principal. O argumento corresponde ao nº de dígitos do PI que queremos ver (ex. (pi-collisions 1) --> 3)
(defun pi-collisions (N)
  "Função para devolver o número de colisões elásticas entre dois objetos"
  (cond ((<= N 0) nil)
        (t (collision 0 (- 1) 1 (expt (expt 10 2) (- N 1)) 0))
))

;; Função que calcula a existência de colisão entre os 2 corpos, colisão entre o corpo e a parede ou a não existência de colisão
;; Se v1 < 0, o corpo da esquerda bate na parede (+1 colisão)
;; Se v1 > 0 e v2 > 0 e v2 > v1, os corpos nunca mais vão bater (fim)
(defun collision (v1 v2 m1 m2 res)
  "Função recursiva para calcular as novas velocidades dos objetos após colisão, caso esta exista"
  (cond ((< v1 0) (collision (- v1) v2 m1 m2 (+ 1 res)))
        ((and (>= v1 0) (>= v2 0) (> v2 v1)) res)
        (t (collision (velocity v1 v2 m1 m2) (velocity v2 v1 m2 m1) m1 m2 (+ 1 res)))
))

;;;; Função que faz o cálculo de uma colisão linear (com base na fórmula da conservação de energia)
(defun velocity (v1 v2 m1 m2)
  "Função genérica para calcular a velocidade de um corpo (momento linear após colisão)"
  (/ (+ (* 2 m2 v2) (* v1 (- m1 m2))) (+ m1 m2))
)