(def add1
	(fn [x]
		(+ 1 x)
		))

(def sub1
	(fn [x]
		(- x 1)
		))

(def plus
	(fn [x,y]
		(cond
			(zero? x) y
			:else (plus (sub1 x) (add1 y))
			)))

(def minus
	(fn [x,y]
		(cond
			(zero? y) x
			:else (minus (sub1 x) (sub1 y))
			)
		))

(def tup?
	(fn [t]
		(cond
			(empty? t) true
			(number? (first t)) (tup? (rest t))
			:else false
			)
		))

(def addtup
	(fn [t]
		(cond
			(empty? t) 0
			:else (plus (first t) (addtup (rest t)))
			)
	))

(def mult
	(fn [x,y]
		(cond
			(zero? y) 0
			:else  (plus x (mult x (sub1 y)))
			)
		))

(def tup+
	(fn [t1, t2]
		(cond
			(and (empty? t1) (empty? t2)) ()
			:else (cons (plus 
					(cond (nil? (first t1)) 0 :else (first t1))
					(cond (nil? (first t2)) 0 :else (first t2))
				) (tup+ (rest t1) (rest t2)))
			)
		))

(def gt
	(fn [a, b]
		(cond
			(zero? a) false
			(zero? b) true
			:else (gt (sub1 a) (sub1 b)) 
			)
		))

(def lt
	(fn [a, b]
		(cond
			(zero? b) false
			(zero? a) true
			:else (lt (sub1 a) (sub1 b))
			)
		))

(def eq2
	(fn [a, b]
		(cond
			(gt a b) false
			(lt a b) false
			:else true
			)
		))

(def pow
	(fn [a, b]
		(cond
			(eq2 b 0) 1
			:else (mult a (pow a (sub1 b)))
			)
		))

(def divide
	(fn [a, b]
		(cond
			(lt a b) 0
			:else (add1 (divide (minus a b) b))
			)
		))

(def length
	(fn [l]
		(cond
			(empty? l) 0
			:else (add1 (length (rest l)))
			)
		))

(def pick
	(fn [l, i]
		(cond
			(empty? l) nil
			(eq2 1 i) (first l)
			:else (pick (rest l) (sub1 i))
			)
		))

(def rempick
	(fn [l, i]
		(cond
			(empty? l) l
			(eq2 1 i) (rest l)
			:else (cons (first l) (rempick (rest l) (sub1 i)))
			)
		))

(def nonums
	(fn [l]
		(cond
			(empty? l) l
			(number? (first l)) (nonums (rest l))
			:else (cons (first l) (nonums (rest l)))
			)
		))

(def allnums
	(fn [l]
		(cond
			(empty? l) l
			(number? (first l)) (cons (first l) (allnums (rest l)))
			:else (allnums (rest l))
			)
		))

(def eqan?
	(fn [a1, a2]
		(cond
			(and (and (number? a1) (number? a2)) (eq2 a1 a2)) true
			:else (eq? a1 a2)
			)
		))

(def occur
	(fn [a, l]
		(cond
			(empty? l) 0
			(eqan? a (first l)) (add1 (occur a (rest l)))
			:else (occur a (rest l))
			)
		))

(def one?
	(fn [a]
		(eqan? 1 a)
		))

(def rempick2
	(fn [l, i]
		(cond
			(empty? l) l
			(one? i) (rest l)
			:else (cons (first l) (rempick (rest l) (sub1 i)))
			)
		))