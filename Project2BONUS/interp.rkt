#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      ;;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
      (const-exp (num) (num-val num))
      
      ;;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
      (var-exp (var) (apply-env env var))
      
      ;;\commentbox{\diffspec}
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      ;;\commentbox{\zerotestspec}
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
      ;;\commentbox{\ma{\theifspec}}
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      ;;\commentbox{\ma{\theletspecsplit}}
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      
      (rope-exp (ropes)
                (rope-val (parser (extract-useless ropes) 0))
                )

      (fetch-exp (number ropes)
               (char-val (fetch-i-char (expval->rope (value-of ropes env)) (expval->num (value-of number env))))
                )
      (substr-exp (ropes start end)
                  (mystr-val (rope->mystr (substr (expval->rope (value-of ropes env)) (expval->num (value-of start env)) (expval->num (value-of end env)) )))
                  )

      (concat-exp (ropes1 ropes2)
                  (mystr-val (rope->mystr (concatanate (expval->rope (value-of ropes1 env)) (expval->rope (value-of ropes2 env)) (length (expval->rope (value-of ropes1 env)))))))
      
      ))
  )

(define rope->mystr (lambda (rope)
                      (if (null? rope) '()
                          (cons (cadar rope) (rope->mystr (cdr rope)))

                          )

                      ))

(define substr (lambda (rope start end)
                      (if (null? rope) '()

                         (if (and (>= (caar rope) start) (< (caar rope) end))
                             (cons (car rope) (substr (cdr rope) start end))
                             (substr (cdr rope) start end)
                             )
                          )

                      ))

(define concatanate (lambda (rope1 rope2 rope1-size)
                      (if (null? rope2) rope1
                            (concatanate (append rope1 (cons (list (+  (caar rope2)  rope1-size) (cadar rope2)) '())) (cdr rope2) rope1-size)
                            )
                      
                      ))
                       
(define extract-useless (lambda (str)
                          (if (or (null? str) (eq? (string-length str) 2)) '()
                              (substring str 1 (- (string-length str) 1))
                          )))

(define parser (lambda (str i)
                       (if (eq? i (string-length str)) '()
                           (cons (list i (string-ref str i)) (parser str (+ i 1)))
                           )

                       ))

(define fetch-i-char (lambda (rope  i)
                       (if (null? rope) '()
                           (if (eq? (caar rope) i) (cadar rope)
                               (fetch-i-char (cdr rope) i))))
                       )

