;;;CS 210 programming language
;;;Homework 3
;;;From: Long Nguyen
;;;To: Dr. J
;;;Fall 2011
;;;Lisp programming



;;; 10 functions for homework 3
;;; number-of(), three-same(), four-same(), three-and-two(), semisequential(), 
;;; sequential(), all-same(), sum-of(), most-frequent(), categorize()

(defun number-of (L n)
    (cond ((null L) 0)
		  ((atom L) 0)
          ((= (car L) n) (+ n (number-of (cdr L) n)))
          ((not (= (car L) n)) (number-of (cdr L) n))))

(defun three-same (L)
	(cond ((= 3 (frequency L (car L))) (sum-of L))
		  ((and (not (= 3 (frequency L (car L)))) (null (cdr L))) nil)
		  ((> 3 (frequency L (car L))) (three-same (cdr L)))))
		
(defun four-same (L)
	(cond ((= 4 (frequency L (car L))) (sum-of L))
		  ((and (not (= 4 (frequency L (car L)))) (null (cdr L))) nil)
		  ((> 4 (frequency L (car L))) (four-same (cdr L)))))
		
(defun three-and-two (L)
	(cond ((or (null (two-same L)) (null (threeS L))) nil)
		  ((and (= 2 (two-same L)) (= 3 (threeS L))) 25)))
		
(defun semisequential (L)
	(cond ((> 4 (length L)) nil)
		  ((and (= 4 (length (sort L #'<))) (not (= 4 (check4 (sort L #'<))))) nil)
		  ((= 4 (check4 (sort L #'<))) 30)
	      ((= 0 (check4 (sort L #'<))) (semisequential (cdr L)))))
	
(defun sequential (L)
	(cond ((> 5 (length L)) nil)
		  ((and (= 5 (length (sort L #'<))) (not (= 4 (check5 (sort L #'<))))) nil)
		  ((= 5 (check5 (sort L #'<))) 40)
		  ((= 0 (check5 (sort L #'<))) (sequential (cdr L)))))
		
(defun all-same (L)
	(cond ((= (length L) (frequency L (car L))) 50)
	((> (length L) (frequency L (car L))) nil)))
	
(defun sum-of (L)
	(cond ((null L) 0)
    ((not (null L)) (+ (isInt (car L)) (sum-of (cdr L))))))

(defun most-frequent (L)
	(fmost L 0 0))
	
(defun categorize (L)
	(cate  (list  (if (null (number-of L 1)) 0 (number-of L 1))
		   (if (null (three-same L)) 0 (three-same L))
		   (if (null (four-same L)) 0 (four-same L))
		   (if (null (three-and-two L)) 0 (three-and-two L))
		   (if (null (semisequential L)) 0 (semisequential L))
		   (if (null (sequential L)) 0 (sequential L))
		   (if (null (all-same L)) 0 (all-same L))
		   (if (null (sum-of L)) 0 (sum-of L))
		   (most-frequent L))))

;;; End of 10 functions for homework 3


;;; supplement functions
;;; the following functions support for 10 function above

;;; isInt() check a number is a integer or not
;;; if not it return 0
(defun isInt (n)
	(cond ((numberp n) n)
		  ((not (numberp n)) 0)))
		
;;; frequency() return frequency of a number in a list
;;; if that number not in the list return 0
(defun frequency (L n )
	(cond ((null L) 0)
		  ((= (car L) n) (+ 1  (frequency (cdr L) n)))
		  ((not (= (car L) n)) (frequency (cdr L) n))))


;;; two-same() check a list if there is 2 number is the same
;;; otherwise return nil		
(defun two-same (L)
	(cond ((= 2 (frequency L (car L))) 2)
			((and (not (= 2 (frequency L (car L)))) (null (cdr L))) nil)
			((and (not (= 2 (frequency L (car L)))) (not (null (cdr L)))) (two-same (cdr L)))))		
		

;;;threeS() check a list if there is 3 numbers is the same
;;;otherwise return nil
(defun threeS (L)
		(cond ((= 3 (frequency L (car L))) 3)
			 ((and (not (= 3 (frequency L (car L)))) (null (cdr L))) nil)
			 ((and (not (= 3 (frequency L (car L)))) (not (null (cdr L)))) (threeS (cdr L)))))
		

;;; check4() check a list if there is 4 numbers is the same, if there is return 4
;;; else return 0
(defun check4 (L)
	(cond ((and (< (car L) (car (cdr L)))
			   (< (nth 1 L) (nth 2 L))
			   (< (nth 2 L) (nth 3 L))) 4)
		  ((or (> (car L) (car (cdr L)))
				(> (nth 1 L) (nth 2 L))
				(> (nth 2 L) (nth 3 L))) 0)
	      ((or (= (car L) (car (cdr L)))
				(= (nth 1 L) (nth 2 L))
				(= (nth 2 L) (nth 3 L))) 0)))
				

;;; check5() check a list if there is 5 numbers is the same, if there is return 5
;;; else return 0			   
(defun check5 (L)
		(cond ((and (< (car L) (car (cdr L)))
				    (< (nth 1 L) (nth 2 L))
				    (< (nth 2 L) (nth 3 L))
				    (< (nth 3 L) (nth 4 L))) 5)
			   ((or (> (car L) (car (cdr L)))
					(> (nth 1 L) (nth 2 L))
					(> (nth 2 L) (nth 3 L))
					(> (nth 3 L) (nth 4 L))) 0)
		       ((or (= (car L) (car (cdr L)))
					(= (nth 1 L) (nth 2 L))
					(= (nth 2 L) (nth 3 L))
					(= (nth 3 L) (nth 4 L))) 0)))
					
					

;;;fmost() find most frequent item in a list	
(defun fmost (L most temp)
	 (cond ((null L) temp)
		   ((< most (frequency L (car L))) (fmost (cdr L) (frequency L (car L)) (car L)))
	       ((> most (frequency L (car L))) (fmost (cdr L) most temp))
		   ((= most (frequency L (car L))) (cond ((> temp (car L)) (fmost (cdr L) most temp))
												 ((= temp (car L)) (fmost (cdr L) most temp))
											     ((< temp (car L)) (fmost (cdr L) most (car L)))))))


;;; cate() support for categorize()		
(defun cate (L)
	(cond ((= (car L) (maximum L)) 6)
		  ((= (nth 1 L) (maximum L)) 7)
	   	  ((= (nth 2 L) (maximum L)) 8)
		  ((= (nth 3 L) (maximum L)) 9)
		  ((= (nth 4 L) (maximum L)) 10)
		  ((= (nth 5 L) (maximum L)) 11)
		  ((= (nth 6 L) (maximum L)) 12)
		  ((= (nth 7 L) (maximum L)) 13)
	  	  ((= (nth 8 L) (maximum L)) 0)))

;;; maximum() find a maximum number in a list	
(defun maximum (L)
	(find-max L 0))

(defun find-max (L max)
	(cond ((null L) max)
  	  ((> (car L) max) (find-max (cdr L) (car L)))
  	  ((< (car L) max) (find-max (cdr L) max))
  	  ((null (car L)) (find-max (cdr L) max))))

;;;End of supplement functions

;;; End of file											


	