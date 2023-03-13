;;;; Algoritmo RSA - Encriptação e Desencriptação

;;; Imaginemos que temos a seguinte situação: queremos comunicar com alguém à distância; não queremos que alguém, pelo caminho, saiba a informação que estamos a passar à outra pessoa.
;;; Será que isto é possível, sem as 2 pessoas terem combinado alguma espécie de código anteriormente (critptografia simétrica)?
;;; A resposta é sim, através do algoritmo RSA (assimétrico), utilizado até hoje em praticamente todas as redes de comunicação.
;;; O algoritmo RSA, de forma simples, gera 2 chaves - uma pública (todos têm acesso a esta chave) e uma privada (apenas eu tenho acesso a esta chave). É importante saber que estas chaves estão "ligadas matematicamente", pois isto é crucial para a desencriptação. Quando alguém quer comunicar comigo, podem usar a minha chave pública para encriptar a sua informação. Para eu desencriptar a informação que me enviaram, uso a minha chave privada (daí mais ninguém conseguir desencriptar). As chaves para encriptar e desencriptar são diferentes, daí ser chamada de criptografia assimétrica.

;; UTILIZAÇÂO:

;; 1- (generate-rsa-keys) - esta função devolve uma lista com 2 chaves - pública e privada - ligadas matematicamente e geradas aleatoriamente.
;; ex.: (generate-rsa-keys) > ((519 943) (919 943))
;; chave pública: (519 943) | chave privada: (919 943)
; Nota: é possível alterar o limite dos números primos gerados nesta função (default é 100). Na realidade, os números gerados são de escalas enormes.

;; 2- Para enviar a mensagem "ola tudo bem?" à pessoa com a chave pública (519 943), utilizo a função (rsa-encrypt "ola tudo bem?" '(519 943)), que me devolve a mensagem encriptada: (141 675 349 596 617 643 320 141 596 55 335 126 427)

;; 3- Ao chegar esta mensagem ao destinatário, este usa a chave privada dele (919 943) para desencriptar, através da função (rsa-decrypt '(141 675 349 596 617 643 320 141 596 55 335 126 427) '(919 943)), que devolve: "ola tudo bem?"

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

(defun pick-primes (limit &optional primes)
  "Função que devolve uma lista com 2 números primos (aleatórios) para gerar a chave RSA"
  (cond ((= (length primes) 2) primes)
        (t (let* ((number (random limit))
                  (primep (= (length (prime-decomposition number)) 1))
                  (primes (cond (primep (cons number primes)) (t primes)))
                 )
             (pick-primes limit primes)
               ))
))

(defun prime-decomposition(N)
  "Função que devolve uma lista da decomposição do número N em números primos"
  (prime-decomposition-rec N 2)
)

(defun prime-decomposition-rec(N F)
  "Função recursiva que valida se um número N é divisível por um número F"
  (cond ((<= N 1) nil)
        ((zerop (mod N F)) (cons F (prime-decomposition-rec (/ N F) F)))
        (t (prime-decomposition-rec N (+ F (cond ((= F 2) 1) (t 2)))))
))

(defun encode-text (text)
  "Função recursiva que transforma uma dada palavra em uma lista da palavra em binário"
  (cond ((= (length text) 0) nil)
        ((= (length text) 1) (cons (encode-letter (subseq text 0 1)) nil))
        (t (cons (encode-letter (subseq text 0 1)) (encode-text (subseq text 1 (length text)))))
))

(defun encode-letter (letter)
  "Função que transforma uma dada letra em um numero"
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
  "Função recursiva que transforma uma dada lista em binário numa palavra"
  (cond ((null text) nil)
        ((null (cdr text)) (cons (decode-letter (car text)) nil))
        (t (cons (decode-letter (car text)) (decode-text (cdr text))))
))

(defun decode-letter (letter)
  "Função que transforma um dado código em uma letra"
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
    "Função que junta uma lista de strings"
    (format nil "~{~A~^~}" string-list)
)