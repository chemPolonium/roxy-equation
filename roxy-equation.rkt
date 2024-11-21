#lang racket

(define (sexp->latex sexp)
  (apply string-append (flatten (tex-preprocess sexp))))

(define (tex-preprocess sexp)
  (match sexp
    [(? string? sexp) sexp]
    [(list-rest 'sup res) (tex-sup res)]
    [(list-rest 'sub res) (tex-sub res)]
    [(list-rest 'frac res) (tex-frac res)]
    [(list-rest 'bmat res) (tex-bmatrix res)]
    [(list-rest 'sum res) (tex-sum res)]
    [(list-rest 'brak res) (tex-bracket res)]
    [(list-rest 'sbrk res) (tex-squarebracket res)]
    [(list-rest 'brac res) (tex-brace res)]
    [(list-rest 'arr res) (tex-array res)]
    [(list-rest 'lbrc res) (tex-left-brace res)]
    [(? pair? sexp) (map tex-preprocess sexp)]
    ['coma ","]
    [(? greek? sexp) (list "\\" (symbol->string sexp) " ")]
    [(? tex-sym-assoc sexp) (list "\\" (cdr (tex-sym-assoc sexp)) " ")]
    [(? symbol? sexp) (symbol->string sexp)]
    [(? number? sexp) (number->string sexp)]
    [_ (error "no matched")]))

(define (greek? x)
  (memq x (list 'alpha 'beta 'gamma 'delta 'epsilon 'zeta 'eta 'theta
                'iota 'kappa 'lambda 'mu 'nu 'xi 'omicron 'pi
                'rho 'sigma 'tau 'upsilon 'phi 'chi 'psi 'omega
                'Alpha 'Beta 'Gamma 'Delta 'Epsilon 'Zeta 'Eta 'Theta
                'Iota 'Kappa 'Lambda 'Mu 'Nu 'Xi 'Omicron 'Pi
                'Rho 'Sigma 'Tar 'Upsilon 'Phi 'Chi 'Psi 'Omega)))

(define (tex-sym-assoc x)
  (assq x '((prod "times")
            (div "div")
            (pm "pm")
            (mp "mp")
            (tril "triangleleft")
            (trir "triangleright")
            (cdot "cdot")
            (setm "setminus")
            (star "star")
            (ast "ast")
            (cup "cup")
            (cap "cap")
            (scup "sqcup")
            (scap "sqcap")
            (vee "vee")
            (wedg "wedge")
            (circ "circ")
            (bllt "bullet")
            (oadd "oplus")
            (osub "ominus")
            (odot "odot")
            (osls "oslash"))))

(define (tex-sup sexp)
  (list "^{" (tex-preprocess sexp) "}"))

(define (tex-sub sexp)
  (list "_{" (tex-preprocess sexp) "}"))

(define (tex-frac sexp)
  (if (not (= 2 (length sexp)))
      (error "FRAC: invalid input")
      (list "\\frac{" (tex-preprocess (first sexp)) "}{" (tex-preprocess (second sexp)) "}")))

(define (tex-array-content sexp)
  (add-between (map (lambda (s)
                      ; s is each line
                      ; (map parse s) is to parse every single element
                      (add-between (map tex-preprocess s) " & "))
                    sexp)
               " \\\\\n"))

(define (tex-bmatrix sexp)
  (if (not (apply = (map length sexp)))
      (error "BMATRIX: invalid input")
      (list "\\begin{bmatrix}\n"
            (tex-array-content sexp)
            "\n\\end{bmatrix}")))

(define (tex-array sexp)
  (if (not (apply = (map length (rest sexp))))
      (error "ARRAY: invalid input")
      (list "\\begin{array}{"
            (first sexp)
            "}\n"
            (tex-array-content (rest sexp))
            "\n\\end{array} ")))

(define (tex-sum sexp)
  (define n (length sexp))
  (cond [(= n 3)
         (list "\\sum_{" (tex-preprocess (first sexp)) "}^{"
               (tex-preprocess (second sexp)) "}" (tex-preprocess (third sexp)))]
        [(= n 1)
         (list "{\\sum}" (tex-preprocess first sexp))]
        [(= n 2)
         (list "\\sum_{" (tex-preprocess (first sexp)) "}" (tex-preprocess (second sexp)))]
        [else
         (error "SUM: invalid input")]))

(define (tex-bracket sexp)
  (list "\\left( " (tex-preprocess sexp) "\\right) "))

(define (tex-squarebracket sexp)
  (list "\\left[ " (tex-preprocess sexp) "\\right] "))

(define (tex-brace sexp)
  (list "\\left\\{ " (tex-preprocess sexp) "\\right\\} "))

(define (tex-left-brace sexp)
  (list "\\left\\{ " (tex-preprocess sexp) "\\right."))

(displayln (sexp->latex `("a" + "b" - alpha (sup "c"))))
(displayln (sexp->latex `("a" "b" (frac ("a" "b") ("c" "d")))))
(define abc `(a + b - c))
(displayln (sexp->latex `(a + b - (c) + ,abc + "a" - (frac 3 gamma))))
(displayln (sexp->latex `(a + b + (bmat (1 2 3) (4 5 6)))))
(displayln (sexp->latex `(a + b + (sum (i = 1) (6) i))))
(displayln (sexp->latex `(a prod b oadd c)))
(displayln (sexp->latex `(a + (brak b + c) - (sbrk (d - e)) prod (brac f))))
(displayln (sexp->latex `(lbrc (arr "ccc"
                                    ((a + b) = 1)
                                    (b coma (a > 3))
                                    (c coma "otherwise")
                                    (d = 5)))))
