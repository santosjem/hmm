;;
;; File
;;   hmm.scm
;;
;; Author
;;   Jerod Weinman - Noyce 3825
;;
;; Summary
;;   Provides support routines for a simple first-order language markov model
;;
;; Provides
;;   (count-transitions! filename counts)
;;   (marginal-counts counts)
;;   (normalize-marginal-counts! counts-marg)
;;   (evidence-exact corrupt-filename typo-cond lang-cond lang-marg)
;;   (log-evidence corrupt-filename typo-cond lang-cond lang-marg)
;;   (most-likely-sequence corrupt-filename  typo-cond lang-cond lang-marg)
;;   (count-errors-file correct-filename other-filename)
;;   (count-errors-list correct-filename char-list)
;;   (list->file char-list out-filename)
;;
;; Depends
;;   Some procedures (i.e., vector-map!) from charmodel.scm are needed; 
;;     execute (load "charmodel.scm") prior to use

;;
;; Procedure
;;   iota
;;
;; Purpose
;;   Create a list of the first N integers
;;
;; Parameters
;;   n, an integer
;;
;; Produces
;;   lst, a list
;;
;; Preconditions
;;   n >= 0
;;
;; Postconditions
;;  (= index (list-ref lst index)) for 0 <= index < n
;;
;; Props
;;   Inspired by a routine of the same name in the Grinnell CSC-151 curriculum
(define iota
  (lambda (n)
    (when (not (and (integer? n) (>= n 0)))
      (error "expected non-negative integer as first parameter, given " n))
    (let loop ((list-so-far null)
               (i (- n 1)))
      (if (< i 0)
          list-so-far
          (loop (cons i list-so-far) (- i 1))))))


;;
;; Procedure
;;   make-list
;;
;; Purpose
;;   Create a list of  N copies of a value
;;
;; Parameters
;;   n, an integer
;;   val, a value
;;
;; Produces
;;   lst, a list
;;
;; Preconditions
;;   n >= 0
;;
;; Postconditions
;;  (= val (list-ref lst index)) for 0 <= index < n
;;
;; Props
;;   Inspired by a routine of the same name in the Grinnell CSC-151 curriculum

(define make-list
  (lambda (n val)
    (when (not (and (integer? n) (>= n 0)))
      (error "expected non-negative integer as first parameter, given " n))
    (let loop ((list-so-far null)
               (i (- n 1)))
      (if (< i 0)
          list-so-far
          (loop (cons val list-so-far) (- i 1))))))


;;
;; Procedure
;;   left-section
;;
;; Purpose
;;   Transform a binary to a unary procedure by fixing the left operand
;;
;; Parameters
;;   op, a procedure
;;   left, a value
;;
;; Produces
;;   unary, a procedure
;;
;; Preconditions
;;   op is a binary procedure
;;   left is a valid (first) parameter to op
;;
;; Postconditions
;;   unary takes one parameter so that 
;;     (unary right)  == (op left right)
;;
;; Props
;;   Inspired by a routine of the same name in the Grinnell CSC-151 curriculum

(define left-section
  (lambda (op left)
    (lambda (right)
      (op left right))))

(define l-s left-section)

;;
;; Procedure
;;   arg+max
;;
;; Purpose
;;   Find the index of and value of the largest number in a list
;;
;; Parameters
;;   vals, a list
;;   
;;
;; Produces
;;   result, a pair
;;
;; Preconditions
;;   vals is non-empty: (> 0 (length vals))
;;   Every element of vals is a number: (number? (list-ref vals i)) 
;;      for 0 <= i < (length vals)
;;
;; Postconditions
;;   (car pair) is a number
;;   (car pair) is a member of vals: (member? vals (car pair))
;;   (car pair) is a largest element of vals: 
;;     (>= (car pair) (list-ref vals i)) for 0 <= i < (length vals)
;;   (cdr pair) is an index of (car pair): 
;;      (= (car pair) (list-ref vals (cdr pair)))

(define arg+max
  (lambda (vals)
    (let loop ([argmax 0]
	       [maxval (car vals)]
	       [current 1]
	       [remaining (cdr vals)])
      (cond
       [(null? remaining)
	(cons maxval argmax)]
       [(> (car remaining) maxval)
	(loop current
	      (car remaining)
	      (+ 1 current)
	      (cdr remaining))]
       [else
	(loop argmax
	      maxval
	      (+ 1 current)
	      (cdr remaining))]))))

;;
;; Procedure
;;   argmax
;;
;; Purpose
;;   Find the index of the largest number in a list
;;
;; Parameters
;;   vals, a list
;;
;; Produces
;;   index, an integer
;;
;; Preconditions
;;   vals is non-empty: (< 0 (length vals))
;;   Every element of vals is a number: (number? (list-ref vals i)) 
;;      for 0 <= i < (length vals)
;;
;; Postconditions
;;   0 <= index < (length vals)
;;   The value at index is a largest element of the list
;;     (>= (list-ref vals index) (list-ref vals i)) for 0 <= i < (length vals)

(define argmax (compose cdr arg+max))

;;
;; Procedure
;;   index->char
;;
;; Purpose
;;    Convert an integral index in [0-27] to a character in [a-z] or whitespace
;;
;; Parameters
;;   index, an integer
;;
;; Produces
;;   ch, a character
;;
;; Preconditions
;;   index is in [0,27]
;;
;; Postconditions
;;   ch is in [a-z], i.e., a lower-case Roman character, or whitespace
;;    with 0 corresponding to #\a, 25 corresponding to #\z, 
;;   and 26 corresponding to space, and 27 corresponding to newline
(define index->char
  (let ((a-index (char->integer #\a)))
    (lambda (index)
     (cond
       [(= index 26) 
	#\space]
       [(= index 27)
	#\newline]
       [else
	(integer->char (+ index a-index))]))))
    

;;
;; Procedure
;;   count-transitions!
;;
;; Purpose
;;   Count the number of times one character is followed by another in a file
;;
;; Parameters
;;   filename, a string
;;   counts, a vector
;;
;; Produces
;;   [Nothing. Called for side-effect.]
;;
;; Preconditions
;;   filename refers to a valid text file
;;   counts is a vector of length 28
;;   Each entry in counts is a vector of length 28
;;   Each entry within those member vectors is a number
;;
;; Postconditions
;;   (get-count counts prev next) is incremented for each adjacent
;;     character pair (prev next) in the file pointed to by filename
;;     
(define count-transitions!
  (lambda (filename counts)
    (let ((inport (open-input-file filename))); Open file for reading
      (let loop ((prev-ch (read-char inport)) ; Read first char as prev
		 (next-ch (read-char inport))); Read second char as next
	(cond
	 ((eof-object? next-ch)               ; All done?
	  (close-input-port inport))          ; Close port
	  ;counts)                             ; Return counts
	 (else
	  (increment-count! counts            ; increment counts with
			    (char->index prev-ch) ; previous character index
			    (char->index next-ch)); current/next character index
	  (loop next-ch (read-char inport)))))))) ; loop, reading new next char

;;
;; Procedure
;;   transpose-counts
;;
;; Purpose
;;   Produce a count structure with reversed indices
;;
;; Parameters
;;   counts, a vector
;;   
;;
;; Produces
;;   tr-counts, a vector
;;
;; Preconditions
;;   Let counts be a vector of length num-chars
;;   Each entry in counts is a vector of length num-chars
;;
;; Postconditions
;;   (get-count counts i j) = (get-count tr-counts j i) 
;;     for all 0 <= i,j < num-chars

(define transpose-counts
  (lambda (counts)
    (let* ([len (vector-length counts)]
	   [indices (iota len)]
	   [counts-tr (create-counts len 0)])
      (for-each
       (lambda (index-1)
	 (let ([slice (get-count-slice counts-tr index-1)])
	   (for-each
	    (lambda (index-2)
	      (vector-set! slice index-2 (get-count counts index-2 index-1)))
	    indices)))
       indices)
      counts-tr)))

;;
;; Procedure
;;   marginal-counts
;;
;; Purpose
;;   Produce a vector of all the counts in the first index of a count structure
;;
;; Parameters
;;   counts, a vector
;;
;; Produces
;;   marginal, a vector
;;
;; Preconditions
;;   Let counts be a vector of length num-chars
;;   Each entry in counts is a vector of length num-chars
;;   Each entry within those member vectors is a number, i.e.,
;;     (number? (vector-ref (vector-ref counts i) j)) 
;;        for 0 <= i,j < num-chars
;;
;; Postconditions
;;   Each entry in marginal is the sum of the corresponding "column" in counts:
;;     marg[index2] = sum_{index1} counts[index1,index2]
(define marginal-counts
  (lambda (counts)
    (vector-map! (transpose-counts counts) vector-sum)))


;;
;; Procedure
;;   normalize-marginal-counts!
;;
;; Purpose
;;   Normalize a non-negative count vector as a marginal probabilitu
;;
;; Parameters
;;   counts-marg, a vector
;;
;; Produces
;;   [Nothing. Called for side effect.]
;;
;; Preconditions
;;   Every entry in counts-marg is a number:
;;     (number? (vector-ref counts-marg i)) 
;;       for 0 <= i < (vector-length counts-marg)
;;
;; Postconditions
;;   The sum of the vector is one:
;;     (vector-sum counts-marg) == 1

(define normalize-marginal-counts! 
  (lambda (counts-marg)
    (let ([total (vector-sum counts-marg)])
      (vector-map! counts-marg (lambda (num) (/ num total)))
      (void))))


;;
;; Procedure
;;   counts->list
;;
;; Purpose
;;   Transform vector of vectors into vector of lists
;;
;; Parameters
;;   counts, a vector
;;
;; Produces
;;   count-list, a vector
;;
;; Preconditions
;;   Each entry in counts is a vector
;;
;; Postconditions
;;   Each entry in counts-list is a list:
;;     (list? (vector-ref counts-list i)) for 0 <= i < (vector-length counts)
;;   Each list in counts-list corresponds to the original vector in counts:
;;     (length (vector-ref counts-list i)) == 
;;       (vector-length (vector-ref counts-list i)) 
;;       for 0 <= i < (vector-length counts)
;;     (list-ref (vector-ref counts-list i) j) ==
;;       (vector-ref (vector-ref counts-list i) j)
;;       for 0 <= i < (vector-length counts) and
;;           0 <= j < (vector-length (vector-ref counts i))

(define counts->list
  (lambda (counts)
    (list->vector (map vector->list (vector->list counts)))))


;;
;; Procedure
;;   logsumexp
;;
;; Purpose
;;   Calculate a numerically stable sum of values in log space
;;
;; Parameters
;;   lst, a list
;;
;; Produces
;;   logsum, a number
;;
;; Preconditions
;;   vals is non-empty: (< 0 (length vals))
;;   Every element of vals is a number: (number? (list-ref vals i)) 
;;      for 0 <= i < (length vals)
;;
;; Postconditions
;;   Mathematically, logsum == (log (apply + (map exp lst)))
;;     However, lst's largest value is subtracted from every element
;;       prior to exponentiation for numerical stability with inexact
;;       values. lsts's largest value is added to the result.
(define logsumexp
  (lambda (lst)
    (let ((mx (apply max lst)))
      (+ mx 
	 (log 
	  (apply + 
		 (map (lambda (val)
			(exp (- val mx)))
		      lst)))))))
		     

;;
;; Procedure
;;   evidence-exact
;;
;; Purpose
;;   Calculate exact marginal evidence for observed data
;;
;; Parameters
;;   corrupt-filename, a string
;;   typo-cond, a vector
;;   lang-cond, a vector
;;   lang-marg, a vector
;;
;; Produces
;;   ev, a number
;;
;; Preconditions
;;   corrupt-filename points to a valid, readable file
;;   typo-cond, lang-cond, and lang-marge have the same length:
;;     (= (vector-length typo-cond)
;;        (vector-length lang-cond)
;;        (vector-length lang-marg)
;;   The elements of typo-cond and lang-cond are all vectors the same 
;;     length as lang-marg:
;;       (= (vector-ref typo-cond i) (vector-length lang-marg)) and
;;       (= (vector-ref lang-cond i) (vector-length lang-marg))
;;         for all 0 <= i < (vector-length typo-cond)
;;   The elements of vectors in typo-cond and lang-cond are numbers:
;;     (number? (vector-ref (vector-ref typo-cond i) j)) and
;;     (number? (vector-ref (vector-ref lang-cond i) j)) 
;;        for 0 <= i,j  < (vector-length typo-cond)
;;   The elements of lang-marg are numbers:
;;     (number? (vector-ref lang-marg i))
;;        for 0 <= i  < (vector-length lang-marg)
;;
;; Postconditions
;;   ev represents P(o|D,L,I) = \sum_{c} P(o,c|D,L,I)
;;
;; Practica
;;   Because the evidence is calculated exactly if the counts are exact, 
;;     this procedure is prohibitively slow.
;;
;; Props
;;   Loosely inspired by Equation (15.7) in AIMA (3/e)
(define evidence-exact
  (lambda (corrupt-filename typo-cond lang-cond lang-marg)
    (let* ([num-chars (vector-length lang-marg)]
	   [lang-indices (iota num-chars)]
	  ; List form for (tranposed) typist model so that we have a vector of 
	  ; lists, where the first (vector) index corresponds to the observed 
          ; character, and the second (list) index corresponds to the hidden true
	  ; character
	  [typo-cond-tr-lst (counts->list (transpose-counts typo-cond))]
	  ; List form for (transposed) language model so that we have a vector of 
	  ; lists, where the first (vector) index corresponds to the next/current
	  ; character, and the second (list) index corresponds to the 
          ; previous character
	  [lang-cond-tr-lst (counts->list (transpose-counts lang-cond))]
	  ; List form for marginal ("prior") probability for a true character
	  [lang-marg-lst (vector->list lang-marg)]
	  ; Port for reading characters from
	  [corrupt-inport (open-input-file corrupt-filename)]
          ; First character in the file 
	  [first-ch (read-char corrupt-inport)])
      ; Procedure: message
      ; Purpose: Calculate the marginal likelihood function M(C'|o')
      ; Parameter: ch, a character
      ;            prev-message, a list
      ; Produces:  msg, a list
      ; Preconditions: ch is a valid input to char->index or (eof-object? ch)
      ;                prev-message is M(C|o), previous marginal likelihood
      ; Postconditions: msg is a list of length num-chars
      (letrec ([message
		(lambda (ch prev-message)
		  (cond
		   [(eof-object? ch) ; Out of observable characters
		    ; Final processing: sum message
		    (apply + prev-message)] 
		   [else
		    (let* (; Number representing index of observed character
			   [typo-index (char->index ch)]
			   ; List representing P(o'|C')
			   [typo-slice (get-count-slice typo-cond-tr-lst 
							typo-index)]
			   [next-message
			   ; Message is a list over current (true) character C'
			    (map (lambda (lang-index) 
				   (* 
				    (list-ref typo-slice lang-index) ; P(o'|C') x
				 ; Marginal over prior unbserved char C adding
				 ;     P(C'|C) x M(C|o) for all values of C
				    (apply + 
					   (map *
						(get-count-slice lang-cond-tr-lst 
								 lang-index)
						prev-message))))
				   lang-indices)])
		      (message (read-char corrupt-inport)
			       next-message))]))])
	(cond
	 [(eof-object? first-ch) ; No observable characters (empty file)
	  1] ; Likelihood is 1.
	 [else
	  (message first-ch
		   lang-marg-lst)])))))


;;
;; Procedure
;;   log-evidence
;;
;; Purpose
;;   Calculate inexact marginal evidence for observed data
;;
;; Parameters
;;   corrupt-filename, a string
;;   typo-cond, a vector
;;   lang-cond, a vector
;;   lang-marg, a vector
;;
;; Produces
;;   ev, a number
;;
;; Preconditions
;;   corrupt-filename points to a valid, readable file
;;   typo-cond, lang-cond, and lang-marge have the same length:
;;     (= (vector-length typo-cond)
;;        (vector-length lang-cond)
;;        (vector-length lang-marg)
;;   The elements of typo-cond and lang-cond are all vectors the same 
;;     length as lang-marg:
;;       (= (vector-ref typo-cond i) (vector-length lang-marg)) and
;;       (= (vector-ref lang-cond i) (vector-length lang-marg))
;;         for all 0 <= i < (vector-length typo-cond)
;;   The elements of vectors in typo-cond and lang-cond are numbers:
;;     (number? (vector-ref (vector-ref typo-cond i) j)) and
;;     (number? (vector-ref (vector-ref lang-cond i) j)) 
;;        for 0 <= i,j  < (vector-length typo-cond)
;;   The elements of lang-marg are numbers:
;;     (number? (vector-ref lang-marg i))
;;        for 0 <= i  < (vector-length lang-marg)
;; Postconditions
;;   ev represents P(o|D,L,I) = \sum_{c} P(o,c|D,L,I)
;;
;; Props
;;   Loosely Inspired by Equation (15.7) in AIMA (3/e)
(define log-evidence
  (lambda (corrupt-filename typo-cond lang-cond lang-marg)
    (let* ([num-chars (vector-length lang-marg)]
	   [lang-indices (iota num-chars)]
	  ; List form for (tranposed) typist model so that we have a vector of
	  ; lists, where the first (vector) index corresponds to the observed 
          ; character, and the second (list) index corresponds to the hidden 
	  ; true character
	  [typo-cond-tr-lst (vector-map! (counts->list
					  (transpose-counts typo-cond))
					 (l-s map log))]
	  ; List form for (transposed) language model so that we have a vector of 
	  ; lists, where the first (vector) index corresponds to the next/current
	  ; character, and the second (list) index corresponds to the 
          ; previous character
	  [lang-cond-tr-lst (vector-map! (counts->list 
					  (transpose-counts lang-cond))
					 (l-s map log))]
	  ; List form for marginal ("prior") probability for a true character
	  [lang-marg-lst (vector->list lang-marg)]
	  ; Port for reading characters from
	  [corrupt-inport (open-input-file corrupt-filename)]
          ; First character in the file 
	  [first-ch (read-char corrupt-inport)])
      ; Procedure: message
      ; Purpose: Calculate the marginal likelihood function M(C'|o')
      ; Parameter: ch, a character
      ;            prev-message, a list
      ; Produces:  msg, a list
      ; Preconditions: ch is a valid input to char->index or (eof-object? ch)
      ;                prev-message is M(C|o), previous marginal likelihood
      ; Postconditions: msg is a list of length num-chars
      (letrec ([message
		(lambda (ch prev-message)
		  (cond
		   [(eof-object? ch) ; Out of observable characters
		    ; Final processing: sum message
		    (logsumexp prev-message)] 
		   [else
		    (let* (; Number representing index of observed character
			   [typo-index (char->index ch)]
			   ; List representing P(o'|C')
			   [typo-slice (get-count-slice typo-cond-tr-lst 
							typo-index)]
			   [next-message
			   ; Message is a list over current (true) character C'
			    (map (lambda (lang-index) 
				   (+ ; product is sum in log space
				    (list-ref typo-slice lang-index) ; P(o'|C') x
				 ; Marginal over prior unbserved char C adding
				 ;     P(C'|C) x M(C|o) for all values of C
				    (logsumexp
					   (map +  ; product is sum in log space
						(get-count-slice lang-cond-tr-lst 
								 lang-index)
						prev-message))))
				   lang-indices)])
		      (message (read-char corrupt-inport)
			       next-message))]))])
	(cond
	 [(eof-object? first-ch) ; No observable characters (empty file)
	  0] ; Likelihood is log(1).
	 [else
	  (message first-ch
		   (map log lang-marg-lst))])))))

;;
;; Procedure
;;   most-likely-sequence
;;
;; Purpose
;;   Calculate a maximum prob. sequence of characters to explain observations
;;
;; Parameters
;;   corrupt-filename, a string
;;   typo-cond, a vector
;;   lang-cond, a vector
;;   lang-marg, a vector
;;
;; Produces
;;   mls, a list
;;
;; Preconditions
;;   corrupt-filename points to a valid, readable file
;;   typo-cond, lang-cond, and lang-marge have the same length:
;;     (= (vector-length typo-cond)
;;        (vector-length lang-cond)
;;        (vector-length lang-marg)
;;   The elements of typo-cond and lang-cond are all vectors the same 
;;     length as lang-marg:
;;       (= (vector-ref typo-cond i) (vector-length lang-marg)) and
;;       (= (vector-ref lang-cond i) (vector-length lang-marg))
;;         for all 0 <= i < (vector-length typo-cond)
;;   The elements of vectors in typo-cond and lang-cond are numbers:
;;     (number? (vector-ref (vector-ref typo-cond i) j)) and
;;     (number? (vector-ref (vector-ref lang-cond i) j)) 
;;        for 0 <= i,j  < (vector-length typo-cond)
;;   The elements of lang-marg are numbers:
;;     (number? (vector-ref lang-marg i))
;;        for 0 <= i  < (vector-length lang-marg)
;;
;; Postconditions
;;   mls has the same number of characters as the file pointed to by 
;;     corrupt-filename
;;   mls contains characters: (char? (list-ref mls i)) for 0 <= i < (length mls)
;;   mls corresponds to the sequence argmax P(c|o,D,L,I)
;;
;; Props
;;   Loosely inspired by Equation 15.11 of AIMA (3/e).
;;

(define most-likely-sequence
  (lambda (corrupt-filename typo-cond lang-cond lang-marg)
    (let* ([num-chars (vector-length lang-marg)]
	   [lang-indices (iota num-chars)]
	  ; List form for (tranposed) typist model so that we have a vector of 
	  ; lists, where the first (vector) index corresponds to the observed 
          ; character, and the second (list) index corresponds to the true
	  ; character. Results are transformed into log space 
	  ; (so these are log conditional probabilities)
	  [typo-cond-tr-lst (vector-map! (counts->list 
					  (transpose-counts typo-cond))
					 (lambda (lst) (map log lst)))]
	  ; List form for language model so that we have a vector of lists, 
	  ; where the first (vector) index corresponds to the previous 
	  ; character, and the second (list) index corresponds to the 
          ; next/current character. Results are transformed into log space 
	  ; (so these are log conditional probabilities)
	  [lang-cond-lst (vector-map! (counts->list lang-cond)
				      (lambda (lst) (map log lst)))]
	  ; List form for marginal ("prior") probability for a true character
          ; Results are transformed into log space 
	  ; (so these are log marginal probabilities)
	  [lang-marg-lst (map log (vector->list lang-marg))]
	  ; Port for reading characters from
	  [corrupt-inport (open-input-file corrupt-filename)]
          ; First character in the file (for pre-recursion calculation)
	  [first-ch (read-char corrupt-inport)])
      (letrec (
      ; Procedure: message
      ; Purpose: Calculate the most likely paths at each time step
      ; Parameters: ch, a character
      ;             prev-message, a list
      ;             prev-labels, a list
      ; Produces:  labels, a list
      ; Preconditions: ch is a valid input to char->index or (eof-object? ch)
      ;                prev-message is a list of length num-chars
      ;                prev-labels is a list of lists of length num-chars
      ; Postconditions: labels is a list of characters representing the
      ;                 most likely sequence prediction
	       [message 
		(lambda (ch prev-message prev-labels)
		  ;(display "Message: ") (display prev-message)(newline)
		  ;(display "Table: ") (display prev-labels)(newline)
		  
		  (cond
		   [(eof-object? ch) ; Out of observable characters?
		    ; Finish up: run the prediction loop using the label table
		    (let ([index (argmax prev-message)])
		      (predict index
			       (list (index->char index))
			       prev-labels))]
		   [else
		    ;(display (string-append "ch: " (list->string (list ch))))
		    ;(newline)

		    (let* (; Number representing index of observed character
			   [typo-index (char->index ch)]
			   ; List representing P(o'|C')
			   [typo-slice (get-count-slice typo-cond-tr-lst 
							typo-index)]
			   ; List over C' (current true character) of lists over
			   ; C (previous true character)
			   [msg-pairs 
			    (map 
			     (lambda (lang-index) ; mapped over C'
			     ; Calculate value of transition, using products
			     ;     P(o'|C') x P(C'|C) x M(C|o)
			     ; for all values of C,C'
			       (map 
				(l-s + (list-ref typo-slice lang-index))
				(map ; mapped over C 
				 + ; Product is sum in log space, 
				 (get-count-slice lang-cond-lst lang-index)
				 prev-message)))
			       lang-indices)]
			   ; List of max and argmax value pairs for each C'
			   [msg-vals (map arg+max msg-pairs)])
		      ; With all that heavy lifting done, continue the loop
		      (message (read-char corrupt-inport)
			       (map car msg-vals) ; Extract max values
			       (cons (map cdr msg-vals) ; Add labels to table
				     prev-labels)))]))]
      ; Procedure: predict
      ; Purpose: Read back the most likely paths at each time step
      ; Parameters: prev-index, an integer
      ;             predictions, a list
      ;             label-table, a list
      ; Produces:  labels, a list
      ; Preconditions: prev-index is an integer in [0,num-chars]
      ;                predictions is a list of characters
      ;                label-table is a list of lists of character indices
      ; Postconditions: labels is a list of characters representing the
      ;                 most likely sequence prediction
	       [predict
		(lambda (prev-index predictions label-table)
		  (if (null? label-table)
		      predictions
		      (let* ([labels (car label-table)]
			     [next-index (list-ref labels prev-index)]
			     [prediction (index->char next-index)])
			(predict next-index
				 (cons prediction predictions)
				 (cdr label-table)))))])
	
	;; Finally, the body of function most-likely-sequence
	(cond
	 [(eof-object? first-ch) ; No observable characters (empty file)
	  null] ; Prediction is empty list
	 [else
	  ; Start the recursive message calculation
	  (message 
	   ; Read the next character
	   (read-char corrupt-inport)
	  ; Calculate the initial message
	  ; P(C) x P(o|C) over all values of C
	   (map + ; Product is sum in log space
		lang-marg-lst
		(get-count-slice typo-cond-tr-lst
				 (char->index first-ch)))
	   ; Initial conditional previous state list is empty
	   null)])))))

;;
;; Procedure
;;   count-errors-file
;;
;; Purpose
;;   Count the number of discrepancies betwen two files of the same length
;;
;; Parameters
;;   correct-filename, a string
;;   other-filename, a string
;;
;; Produces
;;   errors, a number
;;
;; Preconditions
;;   correct-filename points to a valid, readable text file
;;   other-filename points to a valid, readable text file
;;   Both files have the same length
;;
;; Postconditions
;;   errors >= 0
;;   errors is the number of characters that differ between the files pointed to by
;;     correct-filename and other-filename.
;;
(define count-errors-file
  (lambda (correct-filename other-filename)
    (let (; Open files for reading
	  [correct-inport (open-input-file correct-filename)]
	  [other-inport (open-input-file other-filename)])
      (let loop ([total 0]
		 [correct-ch  (read-char correct-inport)]
		 [other-ch (read-char other-inport)])
      (cond
       [(and (eof-object? correct-ch) ; Both finished, report total
	     (eof-object? other-ch))
	total]
       [(or (eof-object? correct-ch) ; One finished, but not both
	     (eof-object? other-ch))
	(error "Files have different length")]
       [else
	(loop (+ total (if (equal? correct-ch other-ch) 0 1))
	      (read-char correct-inport)
	      (read-char other-inport))])))))

;;
;; Procedure
;;   count-errors-list
;;
;; Purpose
;;   Count the number of discrepancies between a file and list of the same length
;;
;; Parameters
;;   correct-filename, a string
;;   char-list, a list
;;
;; Produces
;;   errors, a number
;;
;; Preconditions
;;   correct-filename points to a valid, readable text file
;;   char-list contains only characters: (char? (list-ref char-list i)) 
;;     for 0 <= i < (length char-list)
;;   The file pointed to by correct-filename has the same length as char-list
;;
;; Postconditions
;;   errors >= 0
;;   errors is the number of characters that differ between the file pointed to by
;;     correct-filename and the corresponding character in char-list
;;

(define count-errors-list
  (lambda (correct-filename char-list)
    (let (; Open file for reading
	  [correct-inport (open-input-file correct-filename)])
      (let loop ([total 0]
		 [correct-ch  (read-char correct-inport)]
		 [char-list char-list])
      (cond
       [(and (eof-object? correct-ch) ; Both finished, report total
	     (null? char-list))
	total]
       [(or (eof-object? correct-ch) ; One finished, but not both
	    (null? char-list))
	(error "File and list have different lengths")]
       [else
	(loop (+ total (if (equal? correct-ch (car char-list)) 0 1))
	      (read-char correct-inport)
	      (cdr char-list))])))))

;;
;; Procedure
;;   list->file
;;
;; Purpose
;;   Write a list of characters to a file
;;
;; Parameters
;;   char-list, a list
;;   out-filename, a string
;;
;; Produces
;;   [Nothing. Called for side-effect.]
;;
;; Preconditions
;;   out-filename points to a valid, writeable filename.
;;   char-list contains only characters: (char? (list-ref char-list i)) 
;;     for 0 <= i < (length char-list)
;;
;; Postconditions
;;   The characters in char-list are written to a file pointed to by out-filename
;;     in their list order.
;;
(define list->file
  (lambda (char-list out-filename)
    (let (; Open file for writing
	  [outport (open-output-file out-filename)])
      (let loop ([chars char-list])
	(cond
	 [(null? chars) ; No more chars to write?
	  (close-output-port outport)] ; Close up shop
	 [else ; Otherwise
	  (write-char (car chars) outport) ; Write char to file
	  (loop (cdr chars))]))))) ; Continue loop on rest of chars
