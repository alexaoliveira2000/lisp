;;;; Algoritmo RSA - Encripta��o e Desencripta��o

;;; Imaginemos que temos a seguinte situa��o: queremos comunicar com algu�m � dist�ncia; n�o queremos que algu�m, pelo caminho, saiba a informa��o que estamos a passar � outra pessoa.
;;; Ser� que isto � poss�vel, sem as 2 pessoas terem combinado alguma esp�cie de c�digo anteriormente (critptografia sim�trica)?
;;; A resposta � sim, atrav�s do algoritmo RSA (assim�trico), utilizado at� hoje em praticamente todas as redes de comunica��o.
;;; O algoritmo RSA, de forma simples, gera 2 chaves - uma p�blica (todos t�m acesso a esta chave) e uma privada (apenas eu tenho acesso a esta chave). � importante saber que estas chaves est�o "ligadas matematicamente", pois isto � crucial para a desencripta��o. Quando algu�m quer comunicar comigo, podem usar a minha chave p�blica para encriptar a sua informa��o. Para eu desencriptar a informa��o que me enviaram, uso a minha chave privada (da� mais ningu�m conseguir desencriptar). As chaves para encriptar e desencriptar s�o diferentes, da� ser chamada de criptografia assim�trica.

;; UTILIZA��O:

;; 1- (generate-rsa-keys) - esta fun��o devolve uma lista com 2 chaves - p�blica e privada - ligadas matematicamente e geradas aleatoriamente.
;; ex.: (generate-rsa-keys) > ((519 943) (919 943))
;; chave p�blica: (519 943) | chave privada: (919 943)
; Nota: � poss�vel alterar o limite dos n�meros primos gerados nesta fun��o (default � 100). Na realidade, os n�meros gerados s�o de escalas enormes.

;; 2- Para enviar a mensagem "ola tudo bem?" � pessoa com a chave p�blica (519 943), utilizo a fun��o (rsa-encrypt "ola tudo bem?" '(519 943)), que me devolve a mensagem encriptada: (141 675 349 596 617 643 320 141 596 55 335 126 427)

;; 3- Ao chegar esta mensagem ao destinat�rio, este usa a chave privada dele (919 943) para desencriptar, atrav�s da fun��o (rsa-decrypt '(141 675 349 596 617 643 320 141 596 55 335 126 427) '(919 943)), que devolve: "ola tudo bem?"

(defun rsa-encrypt (text public-key)
  "Encriptar um texto em uma lista de numeros (codigo) atraves de uma dada chave publica"
    (mapcar #'(lambda (number) (mod (expt number (car public-key)) (cadr public-key))) (encode-text text))
)

(defun rsa-decrypt (code private-key)
  "Desencriptar uma lista de numeros (codigo) em texto atraves de uma dada chave privada"
  (join-string-list (decode-text (mapcar #'(lambda (number) (mod (expt number (car private-key)) (cadr private-key))) code)))
)

(defun generate-rsa-keys (&optional (limit 100))
  "Devolve uma lista com uma chave publica e privada"
  (let* ((primes (pick-primes limit))
         (n (* (car primes) (cadr primes)))
         (phi (* (1- (car primes)) (1- (cadr primes))))
         (e (encrypt phi))
         (d (decrypt phi e n))
         )
    (list (list e n) (list d n))
    )
)

(defun decrypt (phi encrypt-value limit)
  "Devolve um numero para a chave privada"
  (let ((number (1+ (random (1- limit)))))
    (cond ((equal (mod (* number encrypt-value) phi) 1) number)
          (t (decrypt phi encrypt-value limit))
          )
    )
)

(defun encrypt (phi)
  "Devolve um numero para a chave publica"
  (let ((number (1+ (random (1- phi)))))
    (cond ((relatively-primep number phi) number)
          (t (encrypt phi))
          )
    )
)

(defun relatively-primep (X Y)
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

(defun pick-primes (limit &optional primes)
  "Fun��o que devolve uma lista com 2 n�meros primos (aleat�rios) para gerar a chave RSA"
  (cond ((= (length primes) 2) primes)
        (t (let* ((number (random limit))
                  (primep (= (length (prime-decomposition number)) 1))
                  (primes (cond (primep (cons number primes)) (t primes)))
                 )
             (pick-primes limit primes)
               ))
))

(defun prime-decomposition(N)
  "Fun��o que devolve uma lista da decomposi��o do n�mero N em n�meros primos"
  (prime-decomposition-rec N 2)
)

(defun prime-decomposition-rec(N F)
  "Fun��o recursiva que valida se um n�mero N � divis�vel por um n�mero F"
  (cond ((<= N 1) nil)
        ((zerop (mod N F)) (cons F (prime-decomposition-rec (/ N F) F)))
        (t (prime-decomposition-rec N (+ F (cond ((= F 2) 1) (t 2)))))
))

(defun encode-text (text)
  "Fun��o recursiva que transforma uma dada palavra em uma lista da palavra em bin�rio"
  (cond ((= (length text) 0) nil)
        ((= (length text) 1) (cons (encode-letter (subseq text 0 1)) nil))
        (t (cons (encode-letter (subseq text 0 1)) (encode-text (subseq text 1 (length text)))))
))

(defun encode-letter (letter)
  "Fun��o que transforma uma dada letra em um numero"
  (cond ((equal letter "a") 2)
        ((equal letter "b") 3)
        ((equal letter "c") 4)
        ((equal letter "d") 5)
        ((equal letter "e") 6)
        ((equal letter "f") 7)
        ((equal letter "g") 8)
        ((equal letter "h") 9)
        ((equal letter "i") 10)
        ((equal letter "j") 11)
        ((equal letter "k") 12)
        ((equal letter "l") 13)
        ((equal letter "m") 14)
        ((equal letter "n") 15)
        ((equal letter "o") 16)
        ((equal letter "p") 17)
        ((equal letter "q") 18)
        ((equal letter "r") 19)
        ((equal letter "s") 20)
        ((equal letter "t") 21)
        ((equal letter "u") 22)
        ((equal letter "v") 23)
        ((equal letter "w") 24)
        ((equal letter "x") 25)
        ((equal letter "y") 26)
        ((equal letter "z") 27)
        ((equal letter " ") 28)
        (t 29)
))

(defun decode-text (text)
  "Fun��o recursiva que transforma uma dada lista em bin�rio numa palavra"
  (cond ((null text) nil)
        ((null (cdr text)) (cons (decode-letter (car text)) nil))
        (t (cons (decode-letter (car text)) (decode-text (cdr text))))
))

(defun decode-letter (letter)
  "Fun��o que transforma um dado c�digo em uma letra"
  (cond ((equal letter 2) "a")
        ((equal letter 3) "b")
        ((equal letter 4) "c")
        ((equal letter 5) "d")
        ((equal letter 6) "e")
        ((equal letter 7) "f")
        ((equal letter 8) "g")
        ((equal letter 9) "h")
        ((equal letter 10) "i")
        ((equal letter 11) "j")
        ((equal letter 12) "k")
        ((equal letter 13) "l")
        ((equal letter 14) "m")
        ((equal letter 15) "n")
        ((equal letter 16) "o")
        ((equal letter 17) "p")
        ((equal letter 18) "q")
        ((equal letter 19) "r")
        ((equal letter 20) "s")
        ((equal letter 21) "t")
        ((equal letter 22) "u")
        ((equal letter 23) "v")
        ((equal letter 24) "w")
        ((equal letter 25) "x")
        ((equal letter 26) "y")
        ((equal letter 27) "z")
        ((equal letter 28) " ")
        (t "?")
))

(defun join-string-list (string-list)
    "Fun��o que junta uma lista de strings"
    (format nil "~{~A~^~}" string-list)
)