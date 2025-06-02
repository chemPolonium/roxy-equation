#lang racket

; 洛琪希：一个简单的 S-表达式到 LaTeX 的转换器
; 这个转换器支持基本的数学符号、希腊字母、矩阵、数组等。
; 它使用 Racket 的数据结构和函数来处理 S-表达式，并生成相应的 LaTeX 代码。

; (define (alatex sexp)
;   (apply string-append (flatten (latex sexp))))

(define (latex sexp)
  (if (pair? sexp)
      (string-join
       (cond
         [(tex-binary? (car sexp))
          (tex-binary (car sexp) (map latex (cdr sexp)))]
         [(tex-paren? (car sexp))
          (tex-paren (car sexp) (latex (cadr sexp)))]
         [(tex-func1? (car sexp))
          (tex-func1 (car sexp) (latex (cadr sexp)))]
         [(tex-func2? (car sexp))
          (tex-func2 (car sexp) (latex (cadr sexp)) (latex (caddr sexp)))]
         [(tex-matrice? (car sexp))
          (tex-matrice (car sexp) (map (lambda (r) (map latex r)) (cdr sexp)))]
         [(eq? (car sexp) 'sum) (apply tex-sum (map latex (cdr sexp)))]
         [else (list (string-join (map latex sexp)))])
       "")
      (cond
        [(string? sexp) sexp]
        [(number? sexp) (number->string sexp)]
        [(tex-greek? sexp) (tex-greek sexp)]
        [(tex-symbol? sexp) (tex-symbol sexp)]
        [(symbol? sexp) (symbol->string sexp)])))

;;; Greek Alphabet

(define tex-greek-hash
  (hasheq
   'alpha    "\\alpha"
   'beta     "\\beta"
   'gamma    "\\gamma"
   'delta    "\\delta"
   'epsilon  "\\epsilon"
   'zeta     "\\zeta"
   'eta      "\\eta"
   'theta    "\\theta"
   'iota     "\\iota"
   'kappa    "\\kappa"
   'lambda   "\\lambda"
   'mu       "\\mu"
   'nu       "\\nu"
   'xi       "\\xi"
   'omicron  "\\omicron"
   'pi       "\\pi"
   'rho      "\\rho"
   'sigma    "\\sigma"
   'tau      "\\tau"
   'upsilon  "\\upsilon"
   'phi      "\\phi"
   'chi      "\\chi"
   'psi      "\\psi"
   'omega    "\\omega"
   'Alpha    "\\Alpha"
   'Beta     "\\Beta"
   'Gamma    "\\Gamma"
   'Delta    "\\Delta"
   'Epsilon  "\\Epsilon"
   'Zeta     "\\Zeta"
   'Eta      "\\Eta"
   'Theta    "\\Theta"
   'Iota     "\\Iota"
   'Kappa    "\\Kappa"
   'Lambda   "\\Lambda"
   'Mu       "\\Mu"
   'Nu       "\\Nu"
   'Xi       "\\Xi"
   'Omicron  "\\Omicron"
   'Pi       "\\Pi"
   'Rho      "\\Rho"
   'Sigma    "\\Sigma"
   'Tau      "\\Tau"
   'Upsilon  "\\Upsilon"
   'Phi      "\\Phi"
   'Chi      "\\Chi"
   'Psi      "\\Psi"
   'Omega    "\\Omega"))

(define (tex-greek? sexp)
  (hash-has-key? tex-greek-hash sexp))

(define (tex-greek sexp)
  (hash-ref tex-greek-hash sexp))

;;; Set and Logic Notations

(define tex-symbol-hash
  (hasheq
   'exists        "\\exists"
   'nexists       "\\nexists"
   'forall        "\\forall"
   'in            "\\in"
   'notin         "\\notin"
   'ni            "\\ni"
   'rightarrow    "\\rightarrow"
   'to            "\\to"
   'leftarrow     "\\leftarrow"
   'gets          "\\gets"
   'greater-than  "\\gt"
   'less-than     "\\lt"
   ; some functions are also treated as symbols
   'sin           "\\sin"
   'cos           "\\cos"
   'tan           "\\tan"
   'sec           "\\sec"
   'csc           "\\csc"
   'cot           "\\cot"
   'arcsin        "\\arcsin"
   'arccos        "\\arccos"
   'arctan        "\\arctan"
   'ln            "\\ln"))

(define (tex-symbol? sexp)
  (hash-has-key? tex-symbol-hash sexp))

(define (tex-symbol sexp)
  (hash-ref tex-symbol-hash sexp))

;;; Functions with one argument
; These functions are used for operations like square root, etc.

(define tex-func1-hash
  (hasheq
   'dot   '("\\dot{"        "}")
   'bar   '("\\bar{"        "}")
   'hat   '("\\hat{"        "}")
   'breve '("\\breve{"      "}")
   'sqrt  '("\\sqrt{"       "}")
   'overline    '("\\overline{"   "}")
   'underline   '("\\underline{"  "}")
   'overbrace   '("\\overbrace{"  "}")
   'underbrace  '("\\underbrace{" "}")
   'wideparen   '("\\wideparen{"  "}")
   'underparen  '("\\underparen{"  "}")
   'overrightarrow       '("\\overrightarrow{"      "}")
   'overleftarrow        '("\\overleftarrow{"       "}")
   'overleftrightarrow   '("\\overleftrightarrow{"  "}")
   'underrightarrow      '("\\underrightarrow{"     "}")
   'underleftarrow       '("\\underleftarrow{"      "}")
   'underleftrightarrow  '("\\underleftrightarrow{" "}")
   'overrightharpoon   '("\\overrightharpoon{"  "}")
   'overleftharpoon    '("\\overleftharpoon{"   "}")
   'underrightharpoon  '("\\underrightharpoon{" "}")
   'underleftharpoon   '("\\underleftharpoon{"  "}")
   ))

(define (tex-func1? op)
  (hash-has-key? tex-func1-hash op))

(define (tex-func1 op sexp)
  (match-let ([(list op1 op2) (hash-ref tex-func1-hash op)])
    (list op1 sexp op2)))

;;; Functions with two arguments
; These functions are used for operations like fractions, superscripts, etc.

(define tex-func2-hash
  (hasheq
   'sub       '("{"           "}_{"  "}")
   'sup       '("{"           "}^{ " "}")
   'frac      '("\\frac{"     "}{"   "}")
   'overset   '("\\overset{"  "}{"   "}")
   'underset  '("\\underset{" "}{"   "}")))

(define (tex-func2? op)
  (hash-has-key? tex-func2-hash op))

(define (tex-func2 op sexp1 sexp2)
  (match-let ([(list op1 op2 op3) (hash-ref tex-func2-hash op)])
    (list op1 sexp1 op2 sexp2 op3)))

;;; Functions with three arguments
; These functions are used for large operators like integrals, sums, etc.

(define tex-func3-hash
  (hasheq
   'int       '("\\int_{"  "}^{"    "}{" "}")
   'oint      '("\\oint_{" "}^{"    "}{" "}")
   'lim       '("\\lim_{"  " \\to " "}{" "}")))

;;; Matrices and Arrays
; These functions are used to format matrices and arrays in LaTeX.

(define (tex-matrice-content sexp)
  (if (not (apply = (map length sexp)))
      (error "MATRICE: invalid input" sexp)
      ; add-between is used to join each line with " & "
      ; and then join all lines with " \\\\ "
      (string-join (map (lambda (r) ; r is each row
                          (string-join r " & "))
                        sexp)
                   " \\\\\n")))

(define tex-matrice-hash
  (hasheq
   'array    "array"
   'matrix   "matrix"
   'pmatrix  "pmatrix"
   'bmatrix  "bmatrix"
   'Bmatrix  "Bmatrix"
   'vmatrix  "vmatrix"
   'Vmatrix  "Vmatrix"))

(define (tex-matrice? op)
  (hash-has-key? tex-matrice-hash op))

(define (tex-matrice op sexp)
  (define op-string (hash-ref tex-matrice-hash op))
  (list "\\begin{" op-string "}\n"
        (tex-matrice-content sexp)
        "\n\\end{" op-string "}"))

(define (tex-sum . sexps)
  (match sexps
    [(list start end content) (list "\\sum_{" start "}^{" end "} " content)]
    [(list range content) (list "\\sum_{" range "} " content)]
    [(list content) (list "\\sum " content)]))

;;; Parentheses and Brackets
; These functions are used to format parentheses, brackets, and other delimiters in LaTeX.
; They ensure that the delimiters are properly sized according to the content inside them.
; The delimiters can be parentheses, brackets, braces, angle brackets, vertical bars, etc.

(define tex-paren-hash
  (hasheq
   'paren    '("\\left("         "\\right)")
   'bracket  '("\\left["         "\\right]")
   'brace    '("\\left\\{"       "\\right\\}")
   'lbrace   '("\\left\\{"       "\\right.")
   'angle    '("\\left\\langle"  "\\right\\rangle")
   'vert     '("\\left|"         "\\right|")
   'norm     '("\\left\\|"       "\\right\\|")
   'floor    '("\\lfloor"        "\\rfloor")
   'ceil     '("\\lceil"         "\\rceil")))

(define (tex-paren? op)
  (hash-has-key? tex-paren-hash op))

(define (get-tex-paren op)
  (hash-ref tex-paren-hash op))

(define (tex-paren paren sexp)
  (match-let ([(list left right) (get-tex-paren paren)])
    (list left " " sexp " " right)))

;;; Binary Operations which can be combined with other operations
; These operations include addition, subtraction, multiplication, division, logical operations, etc.

(define tex-binary-hash
  (hasheq
   '+    "+"
   '-    "-"
   '*    "\\,"
   '/    "/"
   'and  "\\land"
   'or   "\\lor"
   'xor  "\\oplus"))

(define (tex-binary? op)
  (hash-has-key? tex-binary-hash op))

(define (tex-binary op sexps)
  (define op-string (hash-ref tex-binary-hash op))
  (add-between sexps (string-append " " op-string " ")))

(displayln (latex '(+ 1 2 3)))
(displayln (latex `((+ 1 2 3))))
(displayln (latex `(,(+ 1 2 3) "Hello!")))
(displayln (latex '(paren (+ 1 2 3))))
(displayln (latex '(matrix (1 2 3) (4 5 6) (7 8 9))))
(displayln (latex '(matrix (1 (+ 2 2 2) 3) (4 5 6) (7 8 9))))
(displayln (latex '(1 + 2 * 3)))
