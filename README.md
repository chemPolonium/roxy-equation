# roxy-equation
Convert Racket expression to LaTeX equations

Example:
```
(displayln (latex '(+ 1 2 3)))

(displayln (latex `((+ 1 2 3))))

(displayln (latex `(,(+ 1 2 3) "Hello!")))

(displayln (latex '(paren (+ 1 2 3))))

(displayln (latex '(matrix (1 2 3) (4 5 6) (7 8 9))))

(displayln (latex '(matrix (1 (+ 2 2 2) 3) (4 5 6) (7 8 9))))

(displayln (latex '(1 + 2 * 3)))
```

Output:
```
1 + 2 + 3

1 + 2 + 3

6 Hello!

\left( 1 + 2 + 3 \right)

\begin{matrix}
1 & 2 & 3 \\
4 & 5 & 6 \\
7 & 8 & 9
\end{matrix}

\begin{matrix}
1 & 2 + 2 + 2 & 3 \\
4 & 5 & 6 \\
7 & 8 & 9
\end{matrix}

1 + 2 * 3
```
