(def rember-f
	(fn [f, a, l]
		(cond
			(empty? l) ()
			(f (first l) a) (rest l)
			:else (cons (first l) (rember-f f a (rest l)))
			)
		))

; currying!
(def eq?-c
	(fn [a]
		(fn [x]
			(eq? x a))
		))

(def rember-f2
	(fn [test?]
		(fn [a, l]
			(cond
				(empty? l) ()
				(test? (first l) a) (rest l)
				:else (cons	(first l) ((rember-f2 test?) a (rest l)))
				)
			)
		))

(def insertL-f
	(fn [test?]
		(fn [n o l]
			(cond
				(empty? l) ()
				(test? (first l) o) (cons n (cons o (rest l)))
				:else (cons (first l) ((insertL-f test?) n o (rest l)))
				)
			)
		)
	)

(def insertR-f
	(fn [test?]
		(fn [n o l]
			(cond
				(empty? l) ()
				(test? (first l) o) (cons o (cons n (rest l)))
				:else (cons (first l) ((insertR-f test?) n o (rest l)))
				)
			)
		)
	)

(def seqL
	(fn [n o l]
		(cons n (cons o l))
		))

(def seqR
	(fn [n o l]
		(cons o (cons n l))
		))

(def insert-g
	(fn [seqn]
		(fn [n o l]
			(cond
				(empty? l) ()
				(eq? (first l) o) (seqn n o (rest l))
				:else (cons (first l) ((insert-g seqn) n o (rest l)))
				)
			)
		)
	)

(def insertL-g
	(fn [n o l]
		((insert-g seqL) n o l)
		)
	)


(def insertR-g
	(fn [n o l]
		((insert-g seqR) n o l)
		)
	)

; define the seq function inline
(def insertL-g2
	(fn [n o l]
		((insert-g
			(fn [n o l]
				(cons n (cons o l)))
			)
			n o l )
		))

(def seqS
	(fn [n o l]
		(cons n l)
		))

(def subst-g
	(fn [n o l]
		((insert-g seqS) n o l)
		))

(def seqrem
	(fn [n o l]
		l
		))

(def rember-g
	(fn [a l]
		((insert-g seqrem) false a l)
		)
	)

(def atom-to-function
	(fn [op]
		(cond
			(eq? op 'plus) plus
			(eq? op 'mult) mult
			:else pow
			)
		))

(def value
	(fn [x]
		(cond
			(atom? x) x
			:else ((atom-to-function (operator x)) (first-sub-expr x) (second-sub-expr x))
			)
		))

(def multirember-f
	(fn [test?]
		(fn [a, l]
			(cond
				(empty? l) ()
				(test? (first l) a) ((multirember-f test?) a (rest l))
				:else (cons (first l) ((multirember-f test?) a (rest l)))
				)
			)
		))

; for this one, contain the logic for what to compare to in test?
; so, for example, test? is (eq?-c 'a)
(def multiremberT
	(fn [test? l]
		(cond
			(empty? l) ()
			(test? (first l)) (multiremberT test? (rest l))
			:else (cons (first l) (multiremberT test? (rest l)))
			)
		))

; continuations and such
(def multirember-co
	(fn [a l col]
		(cond
			(empty? l) (col '() '())
			; if they're equal, recurse with a, rest of the list, and a new function that adds first l to the second
			; argument to col
			(eq? (first l) a) (multirember-and-co a (rest l) (fn [newl seen] (col newl (cons (first l) seen))))
			; else, recurse with the function adding first l to the first argument of col
			:else (multirember-and-co a (rest l) (fn [newl seen] (col (cons (first l) newl) seen )))
			)
		)
	)

(def multiinsertLR
	(fn [n oldL oldR l]
		(cond
			(empty? l) ()
			(eq? (first l) oldL) (cons n (cons oldL (multiinsertLR n oldL oldR (rest l))))
			(eq? (first l) oldR) (cons oldR (cons n (multiinsertLR n oldL oldR (rest l))))
			:else (cons (first l) (multiinsertLR n oldL oldR (rest l)))
			)
		)
	)

(def multiinsertLR-co
	(fn [n oldL oldR l col]
		(cond
			(empty? l) (col '() '0 '0)
			(eq? (first l) oldL) (multiinsertLR-co n oldL oldR (rest l) (fn [nl L R] (col (cons n (cons oldL nl)) (add1 L) R )))
			(eq? (first l) oldR) (multiinsertLR-co n oldL oldR (rest l) (fn [nl L R]  (col (cons oldR (cons n nl)) L (add1 R) )))
			:else (multiinsertLR-co n oldL oldR (rest l) (fn [nl L R] (col (cons (first l) nl) L R) ))
			)
		)
	)

(def evens-only*
	(fn [l]
		(cond
			(empty? l) '()
			(atom? (first l))
				(cond
					(even? (first l)) (cons (first l) (evens-only* (rest l)))
					:else (evens-only* (rest l))
					)
			:else (cons (evens-only* (first l)) (evens-only* (rest l)))
			)
		))

(def evens-only*-co
	(fn [l co]
		(cond
			(empty? l) (co '() '1 '0)
			(atom? (first l))
				(cond
					(even? (first l)) (evens-only*-co (rest l) (fn [nl etot otot] (co (cons (first l) nl) (mult etot (first l)) otot)))
					:else (evens-only*-co (rest l) (fn [nl etot otot] (co nl etot (plus (first l) otot) )))
					)
			:else (evens-only*-co (first l) (fn [al ae ao] 
				(evens-only*-co (rest l) (fn [dl de dod]
						(co (cons al dl) (mult ae de) (plus ao dod))
					))) )
			)
		))
