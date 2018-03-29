;converter string numa lista de números         
;(define converte-lista
;  (lambda (seq-texto)
;      (letrec ((tirar-spaces
;                (lambda (lista)
;      (if (null? lista)
;        '()
;        (if (equal? (car lista) #\space)
;            (tirar-spaces (cdr lista))
;            (cons (car lista) (tirar-spaces (cdr lista)))))))
;               
;               (numeros  ;passar de carateres para numeros
;                (lambda (lista)
;                  (map (lambda (x) (- (char->integer x) (char->integer #\0 ))) lista))))
;        (list->vector (numeros (tirar-spaces (string->list seq-texto)))))))


;faz vetor
(define vetor
  (lambda (string i acum)
    (if (not (= i (string-length string)))
        (if (char-whitespace? (string-ref string i))
            (vetor string (add1 i) (add1 acum))
            (vetor string (add1 i) acum))
        (vector-fill! (make-vector (add1 acum)) 0))
    ))

(define fillvetor
  (lambda (vetor palavras i ulti vi)
    (if (not (= i (sub1 (string-length palavras))))
        (if (char-whitespace? (string-ref palavras i))
            (begin (vector-set! vetor vi (substring palavras ulti i)) ;preencher vetor
                   (fillvetor vetor palavras (add1 i) (add1 i) (add1 vi))) ;chamada recursiva
            (fillvetor vetor palavras (add1 i) ulti vi))
        (if (not (char-whitespace? (string-ref palavras i)))
            (if (= ulti i)
                (vector-set! vetor vi (string (string-ref palavras i)))
                (vector-set! vetor vi (substring palavras ulti (add1 i))))))
    vetor))

(define transformar-em-numeros
  (lambda (vetor)
    (list->vector (map string->number (vector->list vetor)))))


;verificar se e aritmética

(define aritmetica?
  (lambda (vetor)
    (letrec ((aux
              (lambda (i razao1)
                (if (= i (sub1 (vector-length vetor)))
                    #t
                    (if (= (- (vector-ref vetor (add1 i)) (vector-ref vetor i)) razao1)
                        (aux (add1 i) razao1)
                        #f)))))
      (aux 0 (- (vector-ref vetor 1) (vector-ref vetor 0))))))


;verificar se e geometrica
(define geometrica?
  (lambda (vetor)
    (letrec ((aux
              (lambda (i razao1)
                (if (= i (sub1 (vector-length vetor)))
                    #t
                    (if (= (/ (vector-ref vetor (add1 i)) (vector-ref vetor i)) razao1)
                        (aux (add1 i) razao1)
                        #f)))))
      (aux 0 (/ (vector-ref vetor 1) (vector-ref vetor 0))))))


; termo aritmética
(define termo-aritmetica
  (lambda (sequencia)
    (display "a(n)= ")
    (display (vector-ref sequencia 0))
    (display " + (n-1)*")
    (display (- (vector-ref sequencia 1) (vector-ref sequencia 0)))
    ))

; termo geométrica
(define termo-geometrica
  (lambda (sequencia)
    (display "a(n)= ")
    (display (vector-ref sequencia 0))
    (display "* ")
    (display (/ (vector-ref sequencia 1) (vector-ref sequencia 0)))
    (display "^(n-1)")
    ))

(define razao
  (lambda (sequencia)
    (cond ((aritmetica? sequencia) (display "Razao: ")
                                   (display (- (vector-ref sequencia 1) (vector-ref sequencia 0)))
                                   (newline))
          ((geometrica? sequencia) (display "Razao: ")
                                   (display (/ (vector-ref sequencia 1) (vector-ref sequencia 0)))
                                   (newline))
          (else (display "Esta sucessao nao e nem geometrica nem aritmetica, infelizmente o algoritmo ainda nao chegou ao nivel de compreender outras sucessoes. Enquanto o algoritmo esta em evolucao, da uma olhada a este link e talvez consigas identificar o tipo de sucessao que tens a tua frente: https://www.mat.uc.pt/~jaimecs/ama/Supl-3a200411313923.pdf")))))


(define pertence-aritmetica?
  (lambda (sequencia n)
    (if (number? n)
    (if (zero? (remainder (- n (vector-ref sequencia 0)) (- (vector-ref sequencia 1) (vector-ref sequencia 0))))
        (begin (display "É o elemento numero ")
               (display (add1 (/ (- n (vector-ref sequencia 0)) (- (vector-ref sequencia 1) (vector-ref sequencia 0)))))
               (display " da sucessao"))
        (display "Nao pertence a sucessao"))
(display "Erro. Tem de ser um numero."))))

(define pertence-geometrica?
  (lambda (sequencia n)
    (if (number? n)
    (if  (integer? (/ (log (/ n (vector-ref sequencia 0))) (log (/ (vector-ref sequencia 1) (vector-ref sequencia 0)))))
        (begin (display "É o elemento numero ")
               (display (add1 (/ (log (/ n (vector-ref sequencia 0))) (log (/ (vector-ref sequencia 1) (vector-ref sequencia 0))))))
               (display " da sucessao"))
        (display "Nao pertence a sucessao"))
    (display "Erro. Tem de ser um numero."))))
        
        
     
     
;verificacao
(define procedimento-verificacao
  (lambda (seq-texto i)
    (if (= i (string-length seq-texto))
        (corpo-principal (transformar-em-numeros (fillvetor (vetor seq-texto 0 0) seq-texto 0 0 0)))
        (if (or (char-numeric? (string-ref seq-texto i)) (equal? (string-ref seq-texto i) #\space))
            (procedimento-verificacao seq-texto (add1 i))
            (begin 
              (display "O input tem de ser uma sequencia de numeros inteiros." )
              (newline)
              (procedimento-verificacao (read-line) 0))))))


;;;;;;;;;;;;;;;;;;;
;CORPO PRINCIPAL
;
;(define corpo-principal
;  (lambda ()
;    (newline)
;    (display "Qual a sucessao? (deixa espacos entre os numeros) ")
;    (newline)
;    (let* ((sucessao (read-line)))
;      (newline)
;      (if (not (equal? (procedimento-verificacao sucessao 0) #t))
;          (begin  (newline)
;                  (display "Nova tentativa:")
;                  (newline)
;                  (corpo-principal))
;          (begin (let* ((sequencia (transformar-em-numeros (fillvetor (vetor sucessao 0 0) sucessao 0 0 0))))
;                   
;                   (display "Que pretendes fazer?")
;                   (newline)
;                   (display "Tipo de sucessão: 1")
;                   (newline)
;                   (display "Termo geral: 2")
;                   (newline)
;                   (display "Soma dos termos: 3")
;                   (newline)
;                   (display "Razao: 4")
;                   (newline)
;                   (case (read)
;                     ((1) (cond ((aritmetica? sequencia) (display "Aritmetica"))
;                                ((geometrica? sequencia) (display "Geometrica"))
;                                (else (display "Esta sucessao nao e nem geometrica nem aritmetica, infelizmente o algoritmo ainda nao chegou ao nivel de compreender outras sucessoes. Enquanto o algoritmo esta em evolucao, da uma olhada a este link e talvez consigas identificar o tipo de sucessao que tens a tua frente: https://www.mat.uc.pt/~jaimecs/ama/Supl-3a200411313923.pdf"))))
;                     ((2) (cond ((aritmetica? sequencia) (display (termo-aritmetica sequencia)))
;                                ((geometrica? sequencia) (display (termo-geometrica sequencia)))
;                                (else (display "Esta sucessao nao e nem geometrica nem aritmetica, infelizmente o algoritmo ainda nao chegou ao nivel de compreender outras sucessoes. Enquanto o algoritmo esta em evolucao, da uma olhada a este link e talvez consigas identificar o tipo de sucessao que tens a tua frente: https://www.mat.uc.pt/~jaimecs/ama/Supl-3a200411313923.pdf"))))
;                     ((4) (razao sequencia))
;                     )
;                   (corpo-principal)))))))
(define chamada
  (lambda ()
    (procedimento-verificacao (read-line) 0)))
(define lol
  (lambda (a)
    (let* ((b (read-string 1)))
    (procedimento-verificacao b 0))))



(display "*************************************************************************************************************************************************************************")
(newline)
(display "****************************************************************** SUCESSOES GEOMETRICAS E ARITMETICAS ******************************************************************")
(newline)
(display "*************************************************************************************************************************************************************************")
(newline)
(newline)
(display "Qual a sucessao? (deixa espacos entre os numeros, mas nao deixes espacos nem no início, nem no final da sucessao) ")
(newline)
(newline)

(define corpo-principal 
  (lambda (sequencia)
    (newline)
                   (display "Que pretendes saber?")
                   (newline)
                   (display "Tipo de sucessão: 1")
                   (newline)
                   (display "Termo geral: 2")
                   (newline)
                   (display "Pertence a sucessao?: 3")
                   (newline)
                   (display "Razao: 4")
                   (newline)
                   (display "Nova sequencia: 5")
                   (newline)
                   (case (read)
                     ((1) (cond ((aritmetica? sequencia) (display "Aritmetica") (newline))
                                ((geometrica? sequencia) (display "Geometrica") (newline))
                                (else (display "Esta sucessao nao e nem geometrica nem aritmetica, infelizmente o algoritmo ainda nao chegou ao nivel de compreender outras sucessoes. Enquanto o algoritmo esta em evolucao, da uma olhada a este link e talvez consigas identificar o tipo de sucessao que tens a tua frente: https://www.mat.uc.pt/~jaimecs/ama/Supl-3a200411313923.pdf") (newline))))
                     ((2) (cond ((aritmetica? sequencia) (termo-aritmetica sequencia) (newline))
                                ((geometrica? sequencia) (termo-geometrica sequencia) (newline))
                                (else (display "Esta sucessao nao e nem geometrica nem aritmetica, infelizmente o algoritmo ainda nao chegou ao nivel de compreender outras sucessoes. Enquanto o algoritmo esta em evolucao, da uma olhada a este link e talvez consigas identificar o tipo de sucessao que tens a tua frente: https://www.mat.uc.pt/~jaimecs/ama/Supl-3a200411313923.pdf") (newline))))
                      ((3) (cond ((aritmetica? sequencia) (display "Qual e o numero a analisar?") (newline) (pertence-aritmetica? sequencia (read)) (newline))
                                ((geometrica? sequencia) (display "Qual e o numero a analisar?") (newline) (pertence-geometrica? sequencia (read)) (newline))
                                (else (display "Esta sucessao nao e nem geometrica nem aritmetica, infelizmente o algoritmo ainda nao chegou ao nivel de compreender outras sucessoes. Enquanto o algoritmo esta em evolucao, da uma olhada a este link e talvez consigas identificar o tipo de sucessao que tens a tua frente: https://www.mat.uc.pt/~jaimecs/ama/Supl-3a200411313923.pdf") (newline))))
                     ((4) (razao sequencia) (newline))
                    ((5) ((newline) (lol 1))))
                   (corpo-principal sequencia)))


(chamada)
