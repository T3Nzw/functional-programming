#lang racket

; разлики с Haskell:
; > Scheme е динамично типизиран, т.е. типовете стават ясни по време
;   на изпълнение на програмата, а не - по време на компилация
; > нямаме вграден къринг на функциите, а оттам - и частично прилагане;
;   това означава, че ако искаме да фиксираме някои от аргументите на фунцкия,
;   винаги пишем λ-функция
; > модел на оценяване

; синтактис - S-изрази; базови типове

; синтаксисът на Scheme се състои изцяло от S-изрази (S-expressions):
; атомарни обекти като 1, #\a, #t, 1.5, "abcd", 'foo и символи като bar са S-изрази
; ако s1 и s2 са S-изрази, то (s1 . s2) е S-израз

; правила за оценяване на S-изрази; нека s e S-израз:
; > ако s е атом, то s се оценява до себе си
; > ако s е символ, то s се оценява до стойността, с която е свързан символът
; > ако s = (s0 s1 ... sN):
;   > ако s0 е процедура (procedure? s0), то Scheme прилага s0 върху
;     аргументите s1, s2, ..., sN;
;   > ако s0 е ключова дума (let, define, cond, if, lambda, and и др.),
;     то s се нарича специална форма и следва "специални" правила за оценяване, напр.:
;     - let и define създават някакви дефиниции;
;     - cond и if са условни конструкции;
;     - lambda създава λ-функция;
;     - and/or позволяват short-circuiting (т.е. не всички аргументи в израза се оценяват).

; горното означава, че при прилагането на функции се използва префиксен запис, т.е.
; (+ 1 2)

; модел на оценяване - използва се апликативна стратегия на оценяване
; (стриктно оценяване) за разлика от Haskell, където оценяването е
; лениво (нормална стратегия на оценяване); т.е. например списъците сами
; по себе си не могат да бъдат безкрайни и се изискват по-сложни конструкции

; има случаи, в които нормалната стратегия на оценяване дава резултат,
; но апликативната - не.

; Пример:

; В Haskell:
; tuple :: (Int, Double)
; tuple = (1, 5.0 / 0)
; ghci> fst tuple
; 1

; т.е. не хвърляме грешка, тъй като никъде не изискваме стойността във втората
; компонента, т.е. не я оценяваме

; В Scheme:
; (define tuple (cons 1 (/ 5.0 0)))

; Scheme дава грешка още при опит за създаване на горната дефиниция,
; тъй като се опитва да оцени целия израз:

; /: division by zero
;    context...:
;    body of top-level

; Scheme записва символа zero в средата с дефиниции (може да си го мислим като речник),
; като символът zero се свързва със стойността 0
(define zero 0)

; оценяване, специални форми

(define (foo x y)
  (define x*y (* x y))
  (+ 1 x*y))

; локалните дефиниции в let са независими една от друга
(define (bar x y z)
  (let ((a (* x y))
        (b (+ y z)))
    (+ a b)))

; локална дефиниция в let* може да зависи само от
; други дефицинии, които се намират "по-нагоре" от дадената
; (т.е. по-рано дефинирани)
(define (baz x y z)
  (let* ((a (* x y))
         (b (+ a z)))
    (+ a b)))

; локалните дефиници в letrec могат да бъдат взаимно рекурсивни
(define (qux x y z)
  (letrec ((a (* b y))
           (b (+ z z)))
    (+ a b)))

(define (1+ x) (+ 1 x))

; list, quote, eval

(define lst '(1 2 3))

(define lst2 (cons 1 '(2 3)))

; (car lst) ~> 1 (head в Haskell)
; (cdr lst) ~> '(2 3) (tail в Haskell)

; оценяват се до '(1 2 (+ 3 4)) заради ' (quote)
(define list-using-quote '(1 2 (+ 3 4)))
(define list-using-quote2 (quote (1 2 (+ 3 4))))

; оценява се до '(1 2 7)
(define list-using-list (list 1 2 (+ 3 4)))

(quote (+ 1 2 3)) ; ~> '(+ 1 2 3)
(eval (quote (+ 1 2 3))) ; ~> (eval '(+ 1 2 3)) ~> (+ 1 2 3) ~> 6

; сравнение на обекти - =, eq?, eqv?, equal?

; примитивна и опашкова рекурсия

(define (sum-digits n)
  (if (= n 0)
      0
      (+ (remainder n 10) (sum-digits (quotient n 10)))))

; примитивна рекурсия - имаме натрупване на отложени операции:
; > (fib 3)
; > (+ (fib 2) (fib 1))
; > (+ (+ (fib 1) (fib 0)) 1)
; > (+ (+ 1 0) 1)
; > (+ 1 1)
; > 2
(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

; опашкова рекурсия - рекурсивното извикване е последно,
; т.е. нямаме натрупване на операции;
; постига се посредством акумулиране на стойности в
; някакъв аргумент на рекурсивната функция
; (fib* 3)
; (fib-helper 3 0 1)
; ~> (fib-helper (- 3 1) 1 (+ 0 1))
; (fib-helper 2 1 1)
; ~> (fib-helper (- 2 1) 1 (+ 1 1))
; (fib-helper 1 1 2)
; ~> (fib-helper (- 1 1) 2 (+ 1 2))
; (fib-helper 0 2 3)
; 2
(define (fib* n)
  (define (fib-helper n prev curr)
    (if (= n 0)
        prev
        (fib-helper (- n 1) curr (+ prev curr))))
  (fib-helper n 0 1))


; Scheme превежда такива дефиниции на функции до
; дефиниции като add2, т.е. даден символ се свързва
; с λ-функция
(define (add x y) (+ x y))

(define add2 (lambda (x y) (+ x y)))
(define add3 (λ (x y) (+ x y)))

; вариадични функции, apply

(define (append2 lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (append2 (cdr lst1) lst2))))

(define (foldr op nv lst)
  (if (null? lst)
      nv
      (op (car lst) (foldr op nv (cdr lst)))))

; тъй като append може да приема 0 или повече списъци като аргументи,
; няма нужда задължително да приемаме някакви аргументи, т.е.
; (define (append* x . xs) ...) би означавало, че функцията винаги
; приема поне един аргумент и след това неопределен брой аргументи.
; xs се интерпретира като списък от някакви стойнсти (в случая - списъци)
; т.е. все едно искаме да свием целия този списък чрез конкатенация на всички
; списъци в xs. при извикване на функцията НЕ подаваме аргументите като списък.
(define (append* . xs)
  (foldr append2 '() xs))

(define var-append-list (append* '(1 2 3) '(4 5 6) '(7 8 9)))

; чрез apply можем да приложим N-местна функция върху списък с N елемента;
; ако функцията е вариадична, можем да я приложим върху неопределен брой аргументи
(define apply-var-append-list (apply append* '((1 2 3) (4 5 6) (7 8 9))))

; структурна рекурсия над списъци

(define (len lst)
  (if (null? lst)
      0
      (+ 1 (len (cdr lst)))))

(define (take n lst)
  (cond ((null? lst) lst)
        ((<= n 0) lst)
        (else (cons (car lst) (take (- n 1) (cdr lst))))))

; функции от по-висок ред; ламбда-функции
; конструктивни функции

(define (filter p? lst)
  (cond ((null? lst) lst)
        ((p? (car lst))
         (cons (car lst)
               (filter p? (cdr lst))))
        (else (filter p? (cdr lst)))))

(define (at n lst)
  (cond ((null? lst) #f)
        ((= n 0) (car lst))
        (else (at (- n 1) (cdr lst)))))

(define (member? x lst)
  (cond ((null? lst) #f)
        ((= x (car lst)) lst)
        (else (member? x (cdr lst)))))

; асоциативни списъци:
; ще ги представяме като наредени двойки от вида (x . y)

; за разлика от списъците взимането на втори елемент става само чрез cdr

(define get-key car)
(define get-value cdr)
(define mk-pair cons)

(define (assoc-search =? key al)
  (cond ((null? al) #f)
        ((=? key (get-key (car al))) (get-value (car al)))
        (else (assoc-search =? key (cdr al)))))

(define (assoc-insert =? key value al)
  (cond ((null? al) (list (mk-pair key value)))
        ((=? key (get-key (car al))) al)
        (else (cons (car al) (assoc-insert =? key value (cdr al))))))

(define (assoc-remove =? key al)
  (cond ((null? al) al)
        ((=? key (get-key (car al))) (cdr al))
        (else (cons (car al) (assoc-remove =? key (cdr al))))))

(define (assoc-insert-many =? alist . pairs)
  (foldr (λ (x xs) (assoc-insert =? (get-key x) (get-value x) xs)) alist pairs))

(define alist
  (assoc-insert-many = '()
                     (mk-pair 1 4)
                     (mk-pair 2 5)
                     (mk-pair 9 #\a)
                     (mk-pair -1 "abcd")))

; дървета:
; ще ги представяме като списъци:
; '() за празното дърво
; (root left-subtree right-subtree) за непразно дърво,
; където left-subtree и right-subtree са дървета,
; образувани по същите правила

(define empty-tree '())
(define empty? null?)

(define (mk-tree x l r) (list x l r))

; t = '(1 (2 () ()) (3 () ()))
(define get-root car)

; (cadr x) == (car (cdr x))
(define (get-left-subtree t)
  (cadr t))

; (caddr x) = (car (cdr (cdr x)))
(define (get-right-subtree t)
  (caddr t))

; пример за декларативна дефиниция на предикат,
; т.е. дефинираме функцията чрез някакви правила
(define (tree? t)
  (or (empty? t)
      (and (list? t)
           (= 3 (length t))
           (tree? (get-left-subtree t))
           (tree? (get-right-subtree t)))))

(define (mk-leaf x) (list x empty-tree empty-tree))

(define leaf? (lambda (x)
                (and (tree? x)
                     (not (empty? x))
                     (empty? (get-left-subtree x))
                     (empty? (get-right-subtree x)))))

(define (inorder tree)
  (if (empty? tree)
      '()
      (append (inorder (get-left-subtree tree))
              (list (get-root tree))
              (inorder (get-right-subtree tree)))))

(define (tree-level n tree)
  (cond ((empty? tree) '())
        ((= n 0) (list (get-root tree)))
        (else (append (tree-level (- n 1) (get-left-subtree tree))
                      (tree-level (- n 1) (get-right-subtree tree))))))

(define tree1
  (mk-tree 1
           (mk-tree 2
                    (mk-leaf 3)
                    (mk-leaf 4))
           (mk-tree 5
                    (mk-tree 6
                             (mk-leaf 7)
                             empty-tree)
                    (mk-leaf 8))))

(define (count-leaves tree)
  (cond ((empty? tree) 0)
        ((leaf? tree) 1)
        (else (+ (count-leaves (get-left-subtree tree))
                 (count-leaves (get-right-subtree tree))))))

(define (count-nodes tree)
  (if (empty? tree)
      0
      (+ 1
         (count-nodes (get-left-subtree tree))
         (count-nodes (get-right-subtree tree)))))

; дълбоки списъци
; дълбок списък наричаме списък, в който всеки елемент
; е или атом (т.е. не е списък), или е списък

(define id (λ (x) x))

; списъците всъщност са наредени двойки
; от глава (някакъв обект) и опашка (списък)
(define (atom? obj) (not (pair? obj)))

(define deep-list1
  (list (list 1 2 3) (list (list 4 5) 6 7 (list (list 8)))))

(define (flatten dl)
  (cond ((null? dl) dl)
        ((atom? (car dl)) (list (car dl)))
        (else (append (flatten (car dl)) (flatten (cdr dl))))))

(define (deep-map f dl)
  (cond ((null? dl) dl)
        ((atom? dl) (f dl))
        (else (cons (deep-map f (car dl)) (deep-map f (cdr dl))))))

(define (deep-filter p? dl)
  (cond ((null? dl) dl)
        ((atom? (car dl))
         (if (p? (car dl))
             (cons (car dl) (deep-filter p? (cdr dl)))
             (deep-filter p? (cdr dl))))
        (else (cons (deep-filter p? (car dl)) (deep-filter p? (cdr dl))))))

(define (deep-foldr op f nv dl)
  (cond ((null? dl) nv)
        ((atom? dl) (f dl))
        (else (op (deep-foldr op f nv (car dl)) (deep-foldr op f nv (cdr dl))))))

(define (deep-map* f dl)
  (deep-foldr cons f '() dl))

(define (flatten* dl)
  (deep-foldr append list '() dl))

; матрици
; ще ги представяме като списъци от списъци

(define matrix1
  (list (list 1 2 3)
        (list 4 5 6)
        (list 7 8 9)))

(define (all? p? lst)
  (foldr (λ (x xs) (and (p? x) xs)) #t lst))

; първо искаме обектът да е някакъв списък, който е непразен,
; след което искаме и всичките редове да имат една и съща дължина
(define (matrix? m)
  (and (list? m)
       (not (null? m))
       (let ((msize (length (car m))))
         (all? (λ (row) (= msize (length row))) (cdr m)))))

(define (transpose m) (apply map list m))

(define (eq-dimensions? m1 m2)
  (or (and (null? m1)
           (null? m2))
      (and (not (null? m1))
           (not (null? m2))
           (= (length (car m1)) (length (car m2)))
           (eq-dimensions? (cdr m1) (cdr m2)))))

(define fst car)
(define snd cdr)

(define (zip l1 l2) (map cons l1 l2))
(define (zip-with op l1 l2)
  (map (λ (pair) (op (fst pair) (snd pair))) (zip l1 l2)))

(define (zip-matrices op m1 m2)
  (and (eq-dimensions? m1 m2)
       (foldr
        (λ (x xs)
          (cons (map
                 (λ (pair) (op (fst pair) (snd pair)))
                 (zip (fst x) (car (snd x))))
                xs))
        '()
        (zip m1 (map list m2)))))

(define (scalar-product l1 l2)
  (apply + (zip-with * l1 l2)))

(define (multiply-matrices m1 m2)
  (define valid-matrices?
    (and (matrix? m1)
         (matrix? m2)
         (let ((cols1 (length (car m1)))
               (rows2 (length m2)))
           (= cols1 rows2))))
  ; така дефинираме 0-местна функция. това е полезно,
  ; защото е начин за симулиране на лениво оценяване
  ; в Scheme, тъй като функциите са специални форми
  ; и се оценяват едва при извикването си. конкретно проблемът
  ; тук е, че ако m2 не е валидна матрица (напр. литералът 1),
  ; тогава ще се опитаме да го транспонираме въпреки горната
  ; дефиниция и проверката за валидна матрица по-долу
  ; (това е заради стриктното оценяване). така че искаме
  ; да "отложим" оценяването на (transpose m2) максимално
  (define (transposed-m2) (transpose m2))
  (and valid-matrices?
       (map (λ (row1)
              (map (λ (row2) (scalar-product row1 row2)) (transposed-m2)))
            m1)))
