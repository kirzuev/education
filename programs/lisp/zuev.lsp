; Зуев К.А., 324 группа, 2017 год
; Вариант: Суммирование рациональных выражений
;
; Дроби хранятся в программе в виде списка из 4 списков,
; где каждый список содержит полином, то есть числитель/знаменатель.
; ((числитель1) (знаменатель1) (числитель2) (знаменатель2))
; Каждый полином хранится как список из 2 атомов-чисел:
; коэффициент со знаком и значение степени.
; ((коэффициент1 степень1)..)
; Входная структура в виде суммы 2 дробей переводится во внутренней представление,
; вычисляется, а затем снова переводится в привычное представление дроби.
; Дроби приводятся к общему знаменателю, складываются, затем приводятся
; подобные слагаемые, нули удаляются и производится сокращение
; на максимально возможный коэффициент и максимально возможную степень X.
;
; Тесты находятся внутри программы и запускаются автоматически.
; Вывод каждого теста содержит 3 строки:
; входные данные;
; результат до сокращения;
; результат после сокращения.
;
; Можно протестировать другие входные данные:
; (main '((числитель1) / (знаменатель1)) '+ '((числитель2) / (знаменатель2)))
; (Замечание: в этом случае будет лишний вывод знаменателя)

; -- The set of tests
(defun test (n) (test-print n)
    (cond
        ((= n 1) (main (print '((7 + 15 - 3 X) / (2 X ^ 3 + 3 x ^ 2))) (princ '+\ )
            (princ '((X ^ 5 - 3 x ^ 4 + 12 x ^ 3 - 100 x ^ 1 + 0) / (5 X ^ 8 - x)))))
        ((= n 2) (main (print '((3 + x - 2 - x + 3 x) / (- 5 x + x ^ 2)))
            (princ '+\ ) (princ '((- x ^ 4 + 12 x ^ 10) / (- 4 x ^ 3)))))
        ((= n 3) (main (print '((x + 1) / x)) (princ '+\ )
            (princ '((x - 1) / (x ^ 2)))))
        ((= n 4) (main (print '(1 / 3)) (princ '+\ ) (princ '(1 / 6))))
        ((= n 5) (main (print '(1 / 3)) (princ '+\ ) (princ '(-1 / 6))))
        ((= n 6) (main (print '((x ^ 2 - 2 x + 3) / (x - 4))) (princ '+\ ) 
            (princ '((x ^ 3) / (x ^ 2 - 5 x + 2)))))
        ((= n 7) (main (print '(x / 3)) (princ '+\ ) (princ '((-2 x) / 6))))
        ((= n 8) (main (print '(x / (3 x ^ 2))) (princ '+\ )
            (princ '((2 x) / (6 x ^ 2)))))
        ((= n 9) (main (print '((x ^ 2 + 2 x) / 3)) (princ '+\ )
            (princ '((- x ^ 2 - 2 x + 1) / 3))))

        ((= n 10) (main (print '((2 x + 5) / (4 x ^ 2 + 10 x + 25))) (princ '+\ )
            (princ '((- 2 x + 5) / (4 x ^ 2 - 10 x + 25)))))
        ((= n 11) (main (print '((x - 1) / (x + 1))) (princ '+\ )
            (princ '((x + 1) / (x - 1)))))
        ((= n 12) (main (print '((x ^ 2 - x) / (x ^ 2 + x)))
            (princ '+\ ) (princ '((x + 1) / (x - 1)))))
        ((= n 13) (main (print '((x ^ 2 + 2 x + 1) / (x + 1))) (princ '+\ )
            (princ '((2 - x - 3) / 1))))
        ((= n 14) (main (print '(1 / (x ^ 2 - 2 x + 2))) (princ '+\ )
            (princ '(1 / (x ^ 2 + 2 x + 2)))))
        ((= n 15) (main (print '(3 / (x ^ 2 + 1))) (princ '+\ )
            (princ '((2 + 7 x - 5 - 7 x) / (x ^ 2 + 1)))))
        ((= n 16) (main (print '((x + 1) / (x + 1))) (princ '+\ )
            (princ '((x - 1) / (x - 1)))))
        ((= n 17) (main (print '(x ^ 2 + x / x ^ 2 - x)) (princ '+\ )
            (princ '(1 - x / x + 1))))
        (t "ERROR: wrong test")))

(defun main (l1 p l2)
    (output (calculate (input l1 p l2))))

; -- Print the number of test
(defun test-print (n)
    (print 'test) (princ n))

; -- Calculate the result
(defun calculate (l)
    (normalize (list (add (mult (car l) (cadddr l))
                          (mult (cadr l) (caddr l)))
                     (cut (mult (cadr l) (cadddr l))))))

; -- Summ of two expressions
(defun add (l1 l2)
    (cut (append l1 l2)))

; -- Multiplication of two expressions
(defun mult (l1 l2)
    (cond
        ((null l1) nil)
        (t (append (mapcar (lambda (x) (list (* (caar l1) (car x))
                                             (+ (cadar l1) (cadr x))))
                           l2)
                   (mult (cdr l1) l2)))))

; -- Take expression and summ the similar summands
; -- Delete summands with 0
(defun cut (l)
    (filter (sum l nil nil nil)))

(defun sum (l k acc tail)
    (cond
        ((null l)
            (cond
                ((null tail)
                    (cond
                        ((null k) acc)
                        (t (cons k acc))))
                ((null k) (sum (cdr tail) (car tail) acc nil))
                (t (sum (cdr tail) (car tail) (cons k acc) nil))))
        ((null k) (sum (cdr l) (car l) acc tail))
        ((= (cadar l) (cadr k)) (sum (cdr l)
                                     (cons (+ (car k) (caar l))
                                           (cdr k))
                                     acc
                                     tail))
        (t (sum (cdr l) k acc (cons (car l) tail)))))

(defun filter (l)
    (cond
        ((null l) nil)
        ((= (caar l) 0) (cut (cdr l)))
        (t (cons (car l) (cut (cdr l))))))
	
; -- Normalize the expression
(defun normalize (l) (output l)
    (divide l (list (apply 'gcd (mapcar (lambda (x) (car x))
                                        (append (car l) (cadr l))))
                    (apply 'min (mapcar (lambda (x) (cadr x)) 
                                        (append (car l) (cadr l)))))))


; -- Divide the expression l by the expression x
(defun divide (l d)
    (mapcar (lambda (x) (mapcar (lambda (y) (list (/ (car y) (car d))
                                                  (- (cadr y) (cadr d)))) x))
            l))

; -- Read the expressions
(defun input (l1 p l2)
    (list (convert (car (parse l1))) (convert (caddr (parse l1)))
          (convert (car (parse l2))) (convert (caddr (parse l2)))))

(defun parse (l)
    (list (take l) '/ (drop l)))

(defun take (l)
    (cond
        ((null l) nil)
        ((eq '/ (car l)) nil)
        ((atom (car l)) (cons (car l) (take (cdr l))))
        (t (car l))))

(defun drop (l)
    (cond
        ((null l) nil)
        ((eq '/ (car l))
            (cond
                ((atom (cadr l)) (cdr l))
                (t (cadr l))))
        (t (drop (cdr l)))))

; -- Convert the result to the natural view
(defun output (l)
    (cond
        ; expression = 0
        ((null (car l)) (print 0))
        (t (prin3 (math-view (car l)) '/\  (math-view (cadr l))))))

; -- Print 3 elements
(defun prin3 (x y z)
    (cond
        ((null (cdr x)) (print (car x)))
        (t (print x)))
    (princ y)
    (cond 
        ((null (cdr z)) (princ (car z)))
        (t (princ z))))

; -- Convert the expression from the internal representation
; -- to the natural view
(defun math-view (l)
    (cond
        ((null l) nil)
        ; one summand
        ((null (cdr l)) (math-summand (car l)))
        (t (append (math-summand (car l))
                (cond
                    ((> (caadr l) 0) (cons '+ (math-view (cdr l))))
                    (t (cons '- (math-view (cons (cons (- 0 (caadr l))
                                                       (cdadr l))
                                                 (cddr l))))))))))

; -- Convert the summand from the internal representation
; -- to the natural view
(defun math-summand (l)
    (cond
        ; const
        ((= 0 (cadr l)) (list (car l)))
        ((= 1 (cadr l))
            (cond
                ; x
                ((= 1 (car l)) (list 'x))
                ; - x
                ((= -1 (car l)) (list '- 'x))
                ; const x
                (t (list (car l) 'x))))
        ; x ^ const
        ((= 1 (car l)) (list 'x '^ (cadr l)))
        ; - x ^ const
        ((= -1 (car l)) (list '- 'x '^ (cadr l)))
        ; const x ^ const
        (t (list (car l) 'x '^ (cadr l)))))

; -- Convert the expression to the internal representation
(defun convert (l)
    (cond
        ((null l) nil)
        ((atom l) (list (summand (list l))))
        ((null (cdr l)) (list (summand l)))
        (t (monom l))))

; -- Collect the first summand of the List and save the List's tail
(defun monom (l)
    (cond
        ((null l) nil)
        ((atom (car l)) (monom (cons (list (car l)) (cdr l))))
        ((null (cdr l)) (list (summand (car l))))
        ((eql '+ (cadr l)) (cons (summand (car l)) (monom (cddr l))))
        ((eql '- (cadr l)) (cons (summand (car l)) (monom (cdr l))))
        (t (monom (cons (append (car l) (list (cadr l))) (cddr l))))))

; -- Convert summand to the internal representation
; -- [sign] [const1] [x] [^ const2] converts to ([sign]const1 const2)
(defun summand (l)
    (cond
        ((null l) (print "ERROR: empty summand") nil)
        ((eql '+ (car l)) (summand (cdr l)))
        ((null (cdr l))
            (cond
                ; const
                ((numberp (car l)) (append l '(0)))
                ; x
                (t '(1 1))))
        ((null (cddr l))
            (cond
                ; - const
                ((and (eql '- (car l)) (numberp (cadr l)))
                    (list (- 0 (cadr l)) 0))
                ; const x
                ((numberp (car l)) (cons (car l) '(1)))
                ; - x
                (t '(-1 1))))
        ((null (cdddr l))
            (cond
                ; - const x
                ((eql '- (car l)) (cons (- 0 (cadr l)) '(1)))
                ; x ^ const
                (t (cons '1 (cddr l)))))
        ((null (cddddr l))
            (cond
                ; - x ^ const
                ((eql '- (car l)) (cons '-1 (cdddr l)))
                ; const x ^ const
                (t (cons (car l) (cdddr l)))))
        ((null (cdr (cddddr l)))
                ; - const x ^ const
                (cons (- 0 (cadr l)) (cddddr l)))
        (t (print "ERROR: wrong type of summand") nil)))

(test 1)
(test 2)
(test 3)
(test 4)
(test 5)
(test 6)
(test 7)
(test 8)
(test 9)
(test 10)
(test 11)
(test 12)
(test 13)
(test 14)
(test 15)
(test 16)
(test 17)
