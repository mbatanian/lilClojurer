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