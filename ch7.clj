
(def isSet?
	(fn [l]
		(cond
			(empty? l) true
			(member? (rest l) (first l)) false
			:else (isSet? (rest l))
			)
		))

; make it a set by checking if our first element is a member of the rest, and if so excluding
; then calling the function recursively
(def makeSet
	(fn [l]
		(cond
			(empty? l) ()
			(member? (rest l) (first l)) (makeSet (rest l))
			:else (cons (first l) (makeSet (rest l)))
			)
		))

; make it a set by removing all other instances of the first element, then consing that on
; and recursing
(def makeSet2
	(fn [l]
		(cond
			(empty? l) ()
			:else (cons (first l) (makeSet2 (multirember (first l) (rest l))))
			)
		))

(def subset?
	(fn [l1, l2]
		(cond
			(empty? l1) true
			(member? l2 (first l1)) (subset? (rest l1) l2)
			:else false
			)
		))

(def subset2?
	(fn [l1, l2]
		(cond
			(empty? l1) true
			:else (and (member? l2 (first l1)) (subset2? (rest l1) l2))
			)
		))

(def eqset?
	(fn [s1, s2]
		(and (subset? s1 s2) (subset? s2 s1))
		))

(def intersect?
	(fn [s1, s2]
		(cond
			(empty? s1) false
			(member? s2 (first s1)) true
			:else (intersect? (rest s1) s2)
			)
		))

(def intersect2?
	(fn [s1, s2]
		(cond
			(empty? s1) false
			:else (or (member? s2 (first s1)) (intersect2? (rest s1) s2))
			)
		))

(def intersect
	(fn [s1, s2]
		(cond
			(empty? s1) ()
			(member? s2 (first s1)) (cons (first s1) (intersect (rest s1) s2))
			:else (intersect (rest s1) s2)
			)
		))

(def unionm
	(fn [s1, s2]
		(cond
			(empty? s1) s2
			(member? s2 (first s1)) (unionm (rest s1) s2)
			:else (cons (first s1) (unionm (rest s1) s2))
			)
		))

; get the items in the list of sets that are in all of the sets
(def intersectall
	(fn [ls]
		(cond
			(empty? (rest ls)) (first ls)
			:else (intersect (first ls) (intersectall (rest ls)))
			)
		))

(def a-pair?
	(fn [l]
		(cond
			(atom? l) false
			(empty? l) false
			(nil? (first l)) false
			(empty? (rest (rest l))) true
			:else false
			)
		))

(def firstn
	(fn [p]
		(first p)
		))

(def secondn
	(fn [p]
		(first (rest p))
		))

(def thirdn
	(fn [l]
		(first (rest (rest l)))
		))

(def build
	(fn [a1, a2]
		(cons a1 (cons a2 ()))
		))

(def fun?
	(fn [l]
		(isSet? (firsts l))
		))

(def revrel
	(fn [l]
		(cond
			(empty? l) ()
			:else (cons (build (secondn (first l)) (firstn (first l))) (revrel (rest l)))
			)
		))

(def revpair
	(fn [p]
		(build (secondn p) (firstn p))
		))

(def revrel2
	(fn [l]
		(cond
			(empty? l) ()
			:else (cons (revpair (first l)) (revrel2 (rest l)))
			)
		))

(def seconds
	(fn [l]
		(cond
			(empty? l) ()
			:else (cons (secondn (first l)) (seconds (rest l)))
			)
		))

(def fullfun?
	(fn [l]
		(and (isSet? (firsts l)) (isSet? (seconds l)))
		))
; this is the same as fullfun? above, just impl'd differently
(def one-to-one?
	(fn [l]
		(and (fun? l) (fun? (revrel l)))
		))