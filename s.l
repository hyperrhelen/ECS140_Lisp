(defun double-recursive (y)
        (cond 
	((not (listp y)) nil)
	((null y) nil)
;	(t (list (car y) (car y) (double-recursive(cdr y)))))
	(t (cons (car y) ( cons (car y) (double-recursive(cdr y))) )))	
);this fucntion recurisvely runs until nil

(defun double-iterative (y)
	(cond
	((null y) nil)
	((not (listp y)) nil)
	(t 
	  (do 
	    ;first we want to initialize x to y and each time, we're going to find the cdr of x
	    (( x y ( cdr x ))
	    ;we first initalize result to nil

	    ; then each time, we want to append the head twice and then do the do loop
	    (result nil (append result (cons (car x) (list(car x ))))))

	    ((null x) result) ;when x is finally null, then we want to return the result
	  )
	)
    )
)

(defun double-mapcar (y)

	(cond 
	((null y) nil)
	((not (listp y)) nil)
	(t
		(append (apply 'cons (list (car y) (list (car y)))) (double-mapcar(cdr y) ))
		; add to the list recursively using append.
		
;	  (do (( x y (cdr x))
;	 	(result nil (append result (apply 'cons (list (car x) (list (car x)))))))
;		((null x) result)
;	  )
	)

	)
)

(defun rdouble(y)
	(cond 
	((null y) nil)
	((not (listp y)) nil)
	(t 
		(cond 
		;if it's an integer, you just want to add it to the list and then move on
		((integerp (car y)) (cons (car y) (cons (car y) (rdouble(cdr y)))))
		;if it's a list, then you want to recurse
		((listp (car y)) (cons (rdouble(car y)) (rdouble(cdr y))))
		);conditions
	); t condition
	); condition
)

(defun my-mapcar(fun l1 l2)
	(cond
	; if any of them are null, then return nil
	((null l1) nil)
	((null l2) nil)
	((not (listp l1)) nil)
	((not (listp l2)) nil)
	(t
		(cons (funcall fun (car l1) (car l2)) (my-mapcar fun (cdr l1) (cdr l2)))
		;map car does the first of the two lists and continues .. 
;		(list (funcall fun (car l1) (car l2)) (my-mapcar(fun (cdr l1) (cdr l2))))

	);t paranthesis
	);conditional paranthesis
)

(defun rewrite(x)
	(cond
	((null x) nil)
	((not (listp x)) x)
	(t 
		(cond
		((listp (car x)) (cons (rewrite (car x)) (rewrite (cdr x))))
		((equal (car x) 'if) 
			(cons 'cond 
			(cond 
			; has to have either length of 3 or 4. for (if else) or (if)
			((equal (length x) 4)
				(cons (cons (rewrite (car(cdr x))) (list(rewrite(car(cdr(cdr x))))))
				(list (cons 't (list (rewrite(car(cdr(cdr(cdr x))))))))))
			((equal (length x) 3)
				(list (cons (rewrite (car(cdr x))) (list(rewrite(car(cdr(cdr x))))))))
			);condition
			);cons 
		); equal -if statement
		; if it's just a symbol, you leave it as is.
		((symbolp (car x)) (cons (car x) (rewrite(cdr x))))
		;otherwise
		(t (cons (car x) (rewrite (cdr x))))
		)	
	)

	); condition
)

(defun check(x)

	(cons (equal (eval x) (eval (rewrite x))) (cons (eval x) (list (eval (rewrite x)))))
);
