;;;; Códigos Hamming - encriptação e desencriptação

;;; Os códigos Hamming previnem erros de 1 bit na transmissão de mensagens (i.e. código QR danificado, ou ruído). Com este método, numa mensagem com k bits, apenas precisamos de guardar 2^p >= (k + p + 1) bits de paridade/controlo (sendo p o nº de bits de paridade). Quanto maior for a mensagem, menos bits de paridade necessitamos (daí a eficiência de Hamming)

;; Este programa é dividido em 2 funções: encode e decode
;; 1º - Codificar uma mensagem: (encode "ola") > (0 1 1 1 1 0 0 1 0 1 1 0 1 0 0 0 1 0 0 0)
;; 2º - Alterar, manualmente, 1 bit aleatório do código para simular um erro na transmissão (ex. (1 1 1 1 1 0 0 1 0 1 1 0 1 0 0 0 1 0 0 0))
;; 3º - Descodificar a mensagem: (decode '(1 1 1 1 1 0 0 1 0 1 1 0 1 0 0 0 1 0 0 0)) > "ola"
;; NOTA: Cada caracter ocupa 5 bits (15 bits na palavra "ola"), mas a mensagem contém 20 bits. Ou seja, neste código, utilizámos apenas 5 bits de paridade para corrigir qualquer erro nos 20 bits - também denominado por Hamming(20,15). Continuaríamos a utilizar apenas 5 bits de paridade até mensagens de 56 bits

(defun encode (word)
  "Função que devolve uma lista de bits (mensagem codificada)"
  (encode-bits (matrix-to-list (encode-word word)) (encode-bits-cons (matrix-to-list (encode-word word)) (parity-bits (matrix-to-list (encode-word word)) 0)) (parity-bits (matrix-to-list (encode-word word)) 0))
)

(defun decode (bits)
  "Função que recebe uma lista em bits, corrige bits em caso de erro e devolve a palavra"
  (join-string-list (decode-word (matrix (reverse (remove-parity-bits (replace-bit (reverse bits) (1- (binary-to-decimal (decode-hamming-rec bits (1- (ceiling (log (1+ (length bits)) 2))))))) 0 0)) 0 5)))
)

(defun decode-hamming-rec (bits p)
  "Função recursiva que devolve uma lista em binário com a posição do erro (se existir)"
  (cond ((< p 0) nil)
        (t (cons (parity-bit (reverse bits) p 0 t) (decode-hamming-rec bits (1- p))))
))

(defun remove-parity-bits (bits p i)
  "Função recursiva que devolve uma lista sem os bits de paridade (mensagem real)"
  (cond ((null bits) nil)
        ((= (expt 2 p) (1+ i)) (remove-parity-bits (cdr bits) (1+ p) (1+ i)))
        (t (cons (car bits) (remove-parity-bits (cdr bits) p (1+ i))))
))

(defun replace-bit (bits n)
  "Função recursiva que dá toggle num determinado bit (0 para 1 ou vice-versa)"
  (cond ((or (null bits) (< n 0)) bits)
        ((= n 0) (cons (cond ((= (car bits) 0) 1) (t 0)) (cdr bits)))
        (t (cons (car bits) (replace-bit (cdr bits) (1- n))))
))

(defun binary-to-decimal (L)
  "Função recursiva que recebe uma lista em binário e devolve o número em decimal"
  (cond ((null L) 0)
        (t (+ (* (car L) (expt 2 (- (length L) 1))) (binary-to-decimal (cdr L))))
))

(defun matrix-to-list (L)
  "Função recursiva que passa de uma lista de 2 dimensões para 1 dimensão"
  (cond ((atom L) (list L))
        (t (append (matrix-to-list (car L)) (if (cdr L) (matrix-to-list (cdr L)))))
))

(defun parity-bits (bits p)
  "Função recursiva que devolve o número de bits de paridade (controlo) que são precisos para a mensagem"
  (cond ((>= (expt 2 p) (+ (length bits) p 1)) p)
        (t (parity-bits bits (1+ p)))
))

(defun encode-bits (bits L p)
  "Função recursiva que devolve a codificação completa da mensagem, incluindo os bits de paridade"
  (cond ((zerop p) nil)
        ((or (null bits) (= (+ (length bits) p) (expt 2 (1- p)))) (cons (parity-bit (reverse L) (1- p) 0 t) (encode-bits bits L (1- p))))
        (t (cons (car bits) (encode-bits (cdr bits) L p)))
))

(defun parity-bit (bits p n removep)
  "Função recursiva que retorna a paridade para os elementos das casas 2^n"
  (cond ((null bits) 0)
        ((and removep (< n (1- (expt 2 p)))) (parity-bit (cdr bits) p (1+ n) t))
        (removep (parity-bit bits p 0 nil))
        ((= n (* (expt 2 p) 2)) (parity-bit bits p 0 nil))
        ((>= n (expt 2 p)) (parity-bit (cdr bits) p (1+ n) nil))
        (t (logxor (car bits) (parity-bit (cdr bits) p (1+ n) nil)))
))

(defun matrix (L N MAX)
  "Função recursiva que recebe uma lista de bits, e transforma num conjunto de listas de MAX dimensão"
  (cond ((null L) nil)
        ((= N 0) (cons (matrix-letter L 0 MAX) (matrix (cdr L) 1 MAX)))
        ((= N MAX) (matrix L 0 MAX))
        (t (matrix (cdr L) (+ N 1) MAX))
))

(defun matrix-letter (L N MAX)
  "Função recursiva que devolve uma lista de MAX dimensão"
  (cond ((or (null L) (= N MAX)) nil)
        (t (cons (car L) (matrix-letter (cdr L) (+ N 1) MAX)))
))

(defun join-string-list (string-list)
    "Função que junta uma lista de strings"
    (format nil "~{~A~^~}" string-list)
)

(defun encode-bits-cons (bits p)
  "Função recursiva que retorna uma lista da mensagem codificada completa, mas com as casas de controlo a 0"
  (cond ((zerop p) nil)
        ((or (null bits) (= (+ (length bits) p) (expt 2 (1- p)))) (cons 0 (encode-bits-cons bits (1- p))))
        (t (cons (car bits) (encode-bits-cons (cdr bits) p)))
))

(defun encode-word (word)
  "Função recursiva que transforma uma dada palavra em uma lista da palavra em binário"
  (cond ((= (length word) 0) nil)
        ((= (length word) 1) (cons (encode-letter (subseq word 0 1)) nil))
        (t (cons (encode-letter (subseq word 0 1)) (encode-word (subseq word 1 (length word)))))
))

(defun encode-letter (letter)
  "Função que transforma uma dada letra em uma lista de binário"
  (cond ((equal letter "a") '(0 0 0 0 0))
        ((equal letter "b") '(0 0 0 0 1))
        ((equal letter "c") '(0 0 0 1 0))
        ((equal letter "d") '(0 0 0 1 1))
        ((equal letter "e") '(0 0 1 0 0))
        ((equal letter "f") '(0 0 1 0 1))
        ((equal letter "g") '(0 0 1 1 0))
        ((equal letter "h") '(0 0 1 1 1))
        ((equal letter "i") '(0 1 0 0 0))
        ((equal letter "j") '(0 1 0 0 1))
        ((equal letter "k") '(0 1 0 1 0))
        ((equal letter "l") '(0 1 0 1 1))
        ((equal letter "m") '(0 1 1 0 0))
        ((equal letter "n") '(0 1 1 0 1))
        ((equal letter "o") '(0 1 1 1 0))
        ((equal letter "p") '(0 1 1 1 1))
        ((equal letter "q") '(1 0 0 0 0))
        ((equal letter "r") '(1 0 0 0 1))
        ((equal letter "s") '(1 0 0 1 0))
        ((equal letter "t") '(1 0 0 1 1))
        ((equal letter "u") '(1 0 1 0 0))
        ((equal letter "v") '(1 0 1 0 1))
        ((equal letter "w") '(1 0 1 1 0))
        ((equal letter "x") '(1 0 1 1 1))
        ((equal letter "y") '(1 1 0 0 0))
        ((equal letter "z") '(1 1 0 0 1))
        ((equal letter " ") '(1 1 0 1 0))
        (t '(1 1 1 1 1))
))

(defun decode-word (matrix)
  "Função recursiva que transforma uma dada lista em binário numa palavra"
  (cond ((null matrix) nil)
        ((null (cdr matrix)) (cons (decode-letter (car matrix)) nil))
        (t (cons (decode-letter (car matrix)) (decode-word (cdr matrix))))
))

(defun decode-letter (letter)
  "Função que transforma uma dada lista de binário em uma letra"
  (cond ((equal letter '(0 0 0 0 0)) "a")
        ((equal letter '(0 0 0 0 1)) "b")
        ((equal letter '(0 0 0 1 0)) "c")
        ((equal letter '(0 0 0 1 1)) "d")
        ((equal letter '(0 0 1 0 0)) "e")
        ((equal letter '(0 0 1 0 1)) "f")
        ((equal letter '(0 0 1 1 0)) "g")
        ((equal letter '(0 0 1 1 1)) "h")
        ((equal letter '(0 1 0 0 0)) "i")
        ((equal letter '(0 1 0 0 1)) "j")
        ((equal letter '(0 1 0 1 0)) "k")
        ((equal letter '(0 1 0 1 1)) "l")
        ((equal letter '(0 1 1 0 0)) "m")
        ((equal letter '(0 1 1 0 1)) "n")
        ((equal letter '(0 1 1 1 0)) "o")
        ((equal letter '(0 1 1 1 1)) "p")
        ((equal letter '(1 0 0 0 0)) "q")
        ((equal letter '(1 0 0 0 1)) "r")
        ((equal letter '(1 0 0 1 0)) "s")
        ((equal letter '(1 0 0 1 1)) "t")
        ((equal letter '(1 0 1 0 0)) "u")
        ((equal letter '(1 0 1 0 1)) "v")
        ((equal letter '(1 0 1 1 0)) "w")
        ((equal letter '(1 0 1 1 1)) "x")
        ((equal letter '(1 1 0 0 0)) "y")
        ((equal letter '(1 1 0 0 1)) "z")
        ((equal letter '(1 1 0 1 0)) " ")
        (t "?")
))