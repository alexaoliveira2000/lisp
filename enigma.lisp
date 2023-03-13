;;;; Máquina alemã Enigma (WW2) - Encriptação e desencriptação

;;; A máquina Enigma não passa de um simples conjunto de cifras de Cesar seguidas (ou seja, é possível mapear 2 letras a partir da mesma cifra), mas com a particularidade de que a cada letra escrita, o mapeamento das letras é completamente alterado (dificultando a desencriptação se não soubermos o estado inicial da máquina)

;;; A máquina é composta por 3 camadas de encriptação:
;;; painel de tomadas (plugboard) - inicialmente, algumas letras podem ser trocadas por outras (ex. A passa a P, e P passa a A)
;;; rotores (rotors) - camada principal da máquina; alteram uma letra por outra (cifra de Cesar);, quantos mais rotores, mais combinações possíveis existem (também é possível alterar a ordem dos rotores, dificultando ainda mais a desencriptação)
;;; refletor (reflector) - tem uma correspondência direta com outra letra (ex. A --> G, G --> A), para todo o alfabeto (de forma a fazermos a ligação entre uma palavra encriptada e outra desencriptada)

;;; NOTA - como a função do painel de tomadas e do refletor é muito semelhante à dos rotores, podemos considerar estes como rotores

;; UTILIZAÇÂO:
;; A única função que utilizamos é a "enigma" (interface). Por exemplo, ao chamar a função (enigma "inteligencia" '(2 3 1) '(D K F)), estamos a:
;     encriptar a palavra "inteligencia" (output JPOWZJBADKDL)
;     estamos com o rotor 2 em 1º lugar (esquerda), rotor 3 em 2º lugar (centro) e rotor 1 em 3º lugar (direita)
;     as letras iniciais são "D" para o rotor 2, "K" para o rotor 3 e "F" para o rotor 1
; Para desencriptar, é preciso utilizar exatamente a mesma configuração: (enigma "JPOWZJBADKDL" '(2 3 1) '(D K F)) > "INTELIGENCIA"

; como podemos ver, as configurações iniciais são: (3! * 26^3 = 105456), com apenas 3 rotores e sem contar com o painel de tomadas, refletor e quantidade de notches por rotor! (se não soubermos a configuração para desencriptar, boa sorte)

; função principal, utilizada para encriptar e desencriptar frases, sabendo uma determinada ordenação e orientação dos rotores
(defun enigma (word arrangement orientation)
  "Retorna a cifra da palavra passada como argumento"
  (join-string-list (enigma-rec word (setup-machine (setup-rotors arrangement orientation))))
)

(defun enigma-rec (word machine)
  "Retorna a cifra da palavra (letra a letra) de acordo com uma configuração da máquina (recursivo)"
  (cond ((= (length word) 0) nil)
        (t (cons (code-letter (remove-notches machine) (read (make-string-input-stream (subseq word 0 1)))) (enigma-rec (subseq word 1 (length word)) (setup-machine (reverse (turn-rotors (reverse (cdr (reverse (cdr machine)))) t))))))
))

(defun code-letter (rotors letter)
  "Retorna a cifra de uma letra de acordo com uma configuração"
  (car (nth (coded-index (coded-index (letter-index (reverse-rotor (car rotors)) letter) rotors) (cdr (reverse-rotors (reverse rotors)))) (car rotors)))
)

(defun letter-index (rotor letter)
  "Retorna a posição da letra num rotor (recursivo)"
  (cond ((null rotor) nil)
        ((and (listp (cadr (car rotor))) (equal (caadr (car rotor)) letter)) 0)
        ((equal (cadr (car rotor)) letter) 0)
        (t (1+ (letter-index (cdr rotor) letter)))
))

(defun coded-index (index rotors)
  "Retorna a posição da letra cifrada (recursivo)"
  (cond ((null rotors) index)
        (t (coded-index (letter-index (reverse-rotor (car rotors)) (cadr (nth index (car rotors)))) (cdr rotors)))
))

(defun setup-machine (setup)
  "Junta a configuração dos rotores (setup) com o painel de tomadas (plugboard) e o refletor (reflector)"
  (cons (reverse-rotor (map-keys (plugboard))) (reverse (cons (map-keys (reflector)) setup)))
)

; Cada rotor tem "notches" - letras específicas, identificadas aqui como listas - que fazem rodar o próximo rotor (i.e. no 1º rotor, ao chegar à letra G ou H, o próximo rotor é rodado)
(defun rotors ()
  "Lista com todos os rotores disponíveis"
  '((E K M F L (G) D Q V Z N T O W Y (H) X U S P A I B R C J)
    (A J D (K) S I R (U) X B L H W T M C Q G Z N P Y F V O E)
    (B D F H J L C (P) R T X V Z N Y E I W G A (K) M U S Q O)
))

(defun keys ()
  "Lista com todas as chaves (abecedário)"
  '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
)

(defun reflector ()
  "Lista com a componente do refletor (mapeamento direto entre 2 letras)"
  '(Y R U H Q S L D P X N G O K M I E B F Z C W V J A T)
)

(defun plugboard ()
  "Lista com a componente do painel de tomadas (troca de 2 letras)"
  '(R B C D E F K H I J G L M N X P Q A S T U V W O Y Z)
)

(defun map-keys (rotor)
  "Lista com o mapeamento inicial de um rotor (letra do abecedário --> letra do rotor) (recursivo)"
  (cond ((null rotor) nil)
        (t (cons (cons (nth (- 26 (length rotor)) (keys)) (list (car rotor))) (map-keys (cdr rotor))))
))

(defun turn-rotors (rotors turn-next)
  "Lista com todos os rotores rodados (a cada letra, esta função é chamada) (recursivo)"
  (cond ((null rotors) nil)
        ((not turn-next) rotors)
        ((listp (cadr (caar rotors))) (cons (turn-rotor (car rotors)) (turn-rotors (cdr rotors) t)))
        (t (cons (turn-rotor (car rotors)) (cdr rotors)))
))

(defun turn-rotor (rotor)
  "Roda uma vez um rotor"
  (append (cdr rotor) (list (car rotor)))
)

(defun rotor-orientation (rotor letter)
  "Retorna um rotor com a orientação pertendida (recursivo)"
  (cond ((and (listp (cadr (car rotor))) (equal (caadr (car rotor)) letter)) rotor)
        ((equal (cadr (car rotor)) letter) rotor)
        (t (rotor-orientation (turn-rotor rotor) letter))
))

; ex. (setup-rotors '(2 3 1) '(D K P)) > 2º rotor com a letra D, seguido do 3º rotor com a letra K, etc.
(defun setup-rotors (arrangement orientation)
  "Retorna os rotores ordenados e orientados para começar a tradução (recursivo)"
  (cond ((null arrangement) nil)
        (t (cons (rotor-orientation (map-keys (nth (1- (car arrangement)) (rotors))) (car orientation)) (setup-rotors (cdr arrangement) (cdr orientation))))
))

(defun join-string-list (string-list)
    "Função que junta uma lista de strings"
    (format nil "~{~A~^~}" string-list)
)

(defun reverse-rotors (rotors)
  "Inverte o mapeamento de todos os rotores (recursivo)"
  (cond ((null rotors) nil)
        (t (cons (reverse-rotor (car rotors)) (reverse-rotors (cdr rotors))))
))

(defun reverse-rotor (rotor)
  "Inverte o mapeamento de um rotor (recursivo)"
  (cond ((null rotor) nil)
        (t (cons (list (cadr (car rotor)) (caar rotor)) (reverse-rotor (cdr rotor))))
))

(defun remove-notches (rotors)
  "Retorna todos os rotores sem os notches (recursivo)"
  (cond ((null rotors) nil)
        (t (cons (remove-notch (car rotors)) (remove-notches (cdr rotors))))
))

(defun remove-notch (rotor)
  "Retorna um rotor sem notches (recursivo)"
  (cond ((null rotor) nil)
        ((listp (caar rotor)) (cons (list (caaar rotor) (cadr (car rotor))) (remove-notch (cdr rotor))))
        ((listp (cadr (car rotor))) (cons (list (caar rotor) (caadr (car rotor))) (remove-notch (cdr rotor))))
        (t (cons (car rotor) (remove-notch (cdr rotor))))
))