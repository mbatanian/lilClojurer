; chapter 1
(def atom?
	(fn [a]
	( not (seq? a))))

(def eq?
	(fn [a,b]
		( = a b )))
; chapter 2
(def lat?
	(fn [a]
		(cond
			(empty? a) true
			(atom? (first a)) (lat? (rest a))
			:else false
			)))

(def member?
	(fn [l, m]
		(cond
			(empty? l) false
			(eq? m (first l)) true
			:else (member? (rest l) m)
			)))

; chapter 3
(def rember
	(fn [a, l]
		(cond
			(empty? l) ()
			(eq? (first l) a) (rest l)
			:else (cons (first l) (rember a (rest l)))
				)))

(def firsts
	(fn [l]
		(cond
			(empty? l) ()
			:else (cons (first (first l)) (firsts (rest l)))
			)))

(def insertR
	(fn [n, o, lat]
		(cond
			(empty? lat) ()
			(eq? (first lat) o) (cons o (cons n (rest lat)))
			:else (cons (first lat) (insertR n o (rest lat)))
			)
		))

(def insertL
	(fn [n, o, lat]
		(cond
			(empty? lat) ()
			(eq? (first lat) o) (cons n (cons o (rest lat)))
			:else (cons (first lat) (insertL n o (rest lat)))
			)
		))

(def subst
	(fn [n, o, lat]
		(cond
			(empty? lat) ()
			(eq? (first lat) o) (cons n (rest lat))
			:else (cons (first lat) (subst n o (rest lat)))
			)
		))

(def subst2
	(fn [n, o1, o2, lat]
		(cond
			(empty? lat) ()
			(or (eq? (first lat) o1) (eq? (first lat) o2)) (cons n (rest lat))
			:else (cons (first lat) (subst2 n o1 o2 (rest lat)))
			)
		))

(def multirember
	(fn [a, l]
		(cond
			(empty? l) ()
			(eq? (first l) a) (multirember a (rest l))
			:else (cons (first l) (multirember a (rest l)))
			)
		))
