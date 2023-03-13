;;;; Algoritmo para resolver o jogo Sudoku

;;; (acho que não é preciso dizer em que consiste o jogo)

;; Este algoritmo é bastante simples. Para cada espaço vazio (denominado aqui como 0), o algoritmo tenta inserir um numero que ainda não exista na mesma linha, coluna e caixa. Se não conseguir (ou seja, se chegar até n = 10), significa que a solução que está a seguir vai dar a um beco sem saída, logo, é preciso arranjar outra solução (retorna NIL). Um certo jogo pode ter mais que uma solução, mas este algoritmo retorna sempre uma.

;; Se não houver solução, o algoritmo retorna NIL

;; UTILIZAÇÂO: Basta chamar a função (solve) com qualquer tabuleiro como argumento
;; (solve (grid-1))
;; (solve (grid-2))
;; (solve (grid-3))


(defun solve (grid &optional (x 0) (y 0) (n 1))
  "Resolve uma dada grelha de sudoku. NIL se não existir solução"
  (cond ((= x 9) (solvedp grid)) ; Se passar por todas as grelhas, valida se toda a grelha foi preenchida (devolve a grelha caso esteja e NIL caso não)
        ((= y 9) (solve grid (1+ x))) ; Ao chegar à última coluna, passa para a próxima linha
        ((/= (nth y (nth x grid)) 0) (solve grid x (1+ y))) ; Se a grelha na casa (x, y) não tiver valor 0, passa para a próxima casa (já está preenchido)
        ((= n 10) nil) ; Se a casa atual tiver valor 0 (condição anterior), mas se não for possível inserir qualquer numero na casa, o caminho que estamos a seguir não tem solução (retorna NIL)
        ((and (possiblep x y n grid) (solve (insert-number n x y grid)))) ; Se for possivel inserir um numero na posiçao (x, y), o algoritmo vai tentar resolver o resto do algoritmo com esta casa preenchida (nova grelha)
        (t (solve grid x y (1+ n))) ; Caso a casa atual esteja a 0, mas o numero n já existe na mesma linha/coluna/caixa, testamos o proximo numero (n + 1)
))

(defun grid-1 ()
  "Grelha 1 de sudoku que vai ser resolvida"
  '((5 3 0 0 7 0 0 0 0)
    (6 0 0 1 9 5 0 0 0)
    (0 9 8 0 0 0 0 6 0)
    (8 0 0 0 6 0 0 0 3)
    (4 0 0 8 0 3 0 0 1)
    (7 0 0 0 2 0 0 0 6)
    (0 6 0 0 0 0 2 8 0)
    (0 0 0 4 1 9 0 0 5)
    (0 0 0 0 8 0 0 7 9))
)

(defun grid-2 ()
  "Grelha 2 de sudoku que vai ser resolvida"
  '((0 6 0 1 0 4 0 5 0)
    (0 0 8 3 0 5 6 0 0)
    (2 0 0 0 0 0 0 0 1)
    (8 0 0 4 0 7 0 0 6)
    (0 0 6 0 0 0 3 0 0)
    (7 0 0 9 0 1 0 0 4)
    (5 0 0 0 0 0 0 0 2)
    (0 0 7 2 0 6 9 0 0)
    (0 4 0 5 0 8 0 7 0))
)

(defun grid-3 ()
  "Grelha 3 de sudoku que vai ser resolvida"
  '((6 0 9 0 0 0 7 0 0)
    (3 0 5 4 0 0 0 8 0)
    (0 8 0 0 0 6 0 0 3)
    (0 0 0 0 0 1 0 0 5)
    (0 0 0 0 6 0 0 9 2)
    (8 7 0 5 0 0 0 0 0)
    (0 0 0 8 7 3 0 0 9)
    (0 0 0 9 0 0 2 3 8)
    (0 0 0 0 4 0 0 5 0))
)

(defun possiblep (x y n grid)
  "Valida se é possível inserir um numero n na posiçao (x, y) numa dada grelha"
  (let ((x0 (* (floor x 3) 3))
        (y0 (* (floor y 3) 3)))
    (cond ((or (exists-rowp n x grid) (exists-columnp n y grid) (exists-boxp n x0 y0 grid)) nil)
          (t t)
          )
    )
)

(defun exists-rowp (n x grid &optional (i 0))
  "Valida se um numero n existe na linha x"
  (cond ((= i 9) nil)
        ((= (nth i (nth x grid)) n) t)
        (t (exists-rowp n x grid (1+ i)))
))

(defun exists-columnp (n y grid &optional (i 0))
  "Valida se um numero n existe na coluna y"
  (cond ((= i 9) nil)
        ((= (nth y (nth i grid)) n) t)
        (t (exists-columnp n y grid (1+ i)))
))

(defun exists-boxp (n x0 y0 grid &optional (i 0) (j 0))
  "Valida se um numero n existe na caixa [x0 y0]"
  (cond ((= i 3) nil)
        ((= j 3) (exists-boxp n x0 y0 grid (1+ i)))
        ((= (nth (+ y0 j) (nth (+ x0 i) grid)) n) t)
        (t (exists-boxp n x0 y0 grid i (1+ j)))
))

(defun insert-number (n x y grid)
  "Insere um numero n na linha x e coluna y"
  (cond ((< x 0) nil)
        ((= x 0) (cons (insert-row n y (car grid)) (cdr grid)))
        (t (cons (car grid) (insert-number n (1- x) y (cdr grid))))
))

(defun insert-row (n y row)
  "Insere um numero n na coluna y numa dada linha"
  (cond ((< y 0) nil)
        ((= y 0) (cons n (cdr row)))
        (t (cons (car row) (insert-row n (1- y) (cdr row))))
))

(defun solvedp (grid &optional (x 0) (y 0))
  "Se o tabuleiro estiver resolvido retorna o tabuleiro. Caso contrario, retorna NIL"
  (cond ((= x 9) grid)
        ((= y 9) (solvedp grid (1+ x)))
        ((= (nth y (nth x grid)) 0) nil)
        (t (solvedp grid x (1+ y)))
))