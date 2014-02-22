(def rember*
	(fn [a, l]
		(cond
			(empty? l) ()
			(atom? (first l))
			(cond ;; if you're an atom, and your first is a, give me the rest of the list, render* on that
				(eq? (first l) a) (rember* a (rest l))
				:else (cons (first l) (rember* a (rest l))) ;otherwise, continue render* ing on the rest of the list
				)
			:else ; if you're a list, rember* the first, render* the rest
				(cons (rember* a (first l)) (rember* a (rest l)))
			)
	))


(def insertR*
  (fn [new, old, l]
    (cond
       (empty? l) ()
       (atom? (first l))
       (cond 
        (eq? (first l) old) (cons old (cons new (insertR* new old (rest l))))
        :else (cons (first l) (insertR* new old (rest l)))
        )
       :else ; not an atom
         (cons (insertR* new old (first l)) (insertR* new old (rest l)))
     )
    ))


(def occur*
  (fn [a, l]
    (cond
     (empty? l) 0
     (atom? (first l))
     (cond
      (eq? (first l) a) (add1 (occur* a (rest l)))
      :else (occur* a (rest l))
      )
     :else
       (+ (occur* a (first l)) (occur* a (rest l)))
     )
    ))

(def subst*
  (fn [n, o, l]
    (cond
     (empty? l) ()
     (atom? (first l))
     (cond
      (eq? (first l) o) (cons n (subst* n o (rest l)))
      :else (cons (first l) (subst* n o (rest l)))
      )
     :else
       (cons (subst* n o (first l)) (subst* n o (rest l)))
     )    
    ))


(def insertL*
  (fn [n o l]
    (cond
     (empty? l) ()
     (atom? (first l))
     (cond
      (eq? (first l) o) (cons n (cons o (insertL* n o (rest l))))
      :else (cons (first l) (insertL* n o (rest l)))
      )
     :else
       (cons (insertL* n o (first l)) (insertL* n o (rest l)))
    )
  ))

(def member*
  (fn [a l]
    (cond
     (empty? l) false
     (atom? (first l))
     (cond
      (eq? (first l) a) true
      :else (member* a (rest l))
      )
     :else (or (member* a (first l)) (member* a (rest l)))
     )
    ))

  
(def leftmost
  (fn [l]
    (cond
     (empty? l) nil
     (atom? (first l)) (first l)
     :else (leftmost (first l))
     )
    ))

(def eqlist?
  (fn [l1, l2]
    (cond
      ; if both empty, then we're equal -if only one, clearly not
      (and (empty? l1) (empty? l2)) true
      (or (empty? l1) (empty? l2)) false
      ; if both first elements are atoms, check if the atoms are equal -if so, move to the rest of the list
      (and (atom? (first l1)) (atom? (first l2)))
        (cond
          (eqan? (first l1) (first l2)) (eqlist? (rest l1) (rest l2))
          :else false
        )
      ; but if only one is an atom, then it's false
      (or (atom? (first l1)) (atom? (first l2))) false
      ; so, since the first elements are both lists, (since we've exhausted all of the other possibilities)
      ; compare the two lists to make sure they're equal
      (eqlist? (first l1) (first l2)) (eqlist? (rest l1) (rest l2))
      :else false
     )
    ))