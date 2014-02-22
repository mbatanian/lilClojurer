(def numbered?
	(fn [x]
		(cond
			(atom? x) (number? x)
			(eq? (first (rest x)) 'plus) (and (numbered? (first x)) (numbered? (first (rest (rest x)))))
			(eq? (first (rest x)) 'mult) (and (numbered? (first x)) (numbered? (first (rest (rest x)))))
			(eq? (first (rest x)) 'pow) (and (numbered? (first x)) (numbered? (first (rest (rest x)))))
			:else false
		)
	))

(def value
	(fn [x]
		(cond
			(atom? x) x
			(eq? (first (rest x)) 'plus) (value (plus (first x)) (value (first (rest (rest x)))))
			(eq? (first (rest x)) 'mult) (value (mult (first x)) (value (first (rest (rest x)))))
			(eq? (first (rest x)) 'pow) (value (pow (first x)) (value (first (rest (rest x)))))
			:else nil
		)
	))

(def first-sub-expr
	(fn [x]
		(first (rest x))
		))

(def second-sub-expr
	(fn [x]
		(first (rest (rest x)))
		))

(def operator
	(fn [x]
		(first x)
		))

(def value2
	(fn [x]
		(cond
			(atom? x) x
			(eq? (operator x) 'plus) (plus (value2 (first-sub-expr x)) (value2 (second-sub-expr x)))
			(eq? (operator x) 'mult) (mult (value2 (first-sub-expr x)) (value2 (second-sub-expr x)))
			(eq? (operator x) 'pow) (pow (value2 (first-sub-expr x)) (value2 (second-sub-expr x)))
			:else nil
		)
	))

; now let's do stuff with weird list count representation of numbers
; () is 0, ( () ) is 1, ( () () ) is 2, and so on
(def sero?
	(fn [n]
		(empty? n)
		))

(def edd1
	(fn [n]
		(cons '() n)
		))

(def zub1
	(fn [n]
		(rest n)
		))

(def pluz
	(fn [a, b]
		(cond
			(sero? b) a
			:else (edd1 (pluz a (zub1 b)))
		)
	))