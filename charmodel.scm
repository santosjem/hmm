;;
;; File
;;   charmodel.scm
;;
;; Author
;;   Jerod Weinman - Noyce 3821
;;
;; Summary
;;   Provides support routines for modeling typos as conditional probabilities
;;
;; Provides
;;   (create-counts num-chars initial-value)
;;   (char->index ch)
;;   (get-count counts index-1 index-2)
;;   (get-count-slice counts index-1)
;;   (increment-count! counts index-1 index-2)
;;   (count-conditionals! correct-filename corrupt-filename counts)
;;   (normalize-counts! counts)
;;   (likelihood correct-filename corrupt-filename normalized-counts)
;;   (log-likelihood correct-filename corrupt-filename normalized-counts)



;;
;; Procedure
;;   create-counts
;;
;; Purpose
;;   Create a two-dimensional vector of vectors (num-chars x num-chars "matrix")
;;
;; Parameters
;;   num-chars, a positive integer
;;   initial-value, a value
;;
;; Produces
;;   counts, a vector
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   counts is a vector of length num-chars
;;   Each entry in counts is a vector of length num-chars
;;   Each entry within those member vectors is initial-value, i.e.,
;;     (vector-ref (vector-ref counts i) j) = initial-value 
;;        for 0 <= i,j < num-chars
(define create-counts
  (lambda (num-chars initial-value)
    (let* ((counts (make-vector num-chars))) ; Initial vector
      (let loop ((index (- num-chars 1)))    ; count down loop over all entries
	(cond
	 ((< index 0)
	  counts)
	 (else
	  ; Set outer slice to vector of initial-value
	  (vector-set! counts index (make-vector num-chars initial-value))
	  (loop (- index 1))))))))

;;
;; Procedure
;;   char->index
;;
;; Purpose
;;    Convert a character in [a-z] or whitespace to an integral index in [0-27]
;;
;; Parameters
;;   ch, a character
;;
;; Produces
;;   index, an integer
;;
;; Preconditions
;;   ch is in [a-z], i.e., a lower-case Roman character, or whitespace
;;
;; Postconditions
;;   index is in [0,27], with 0 corresponding to #\a, 25 corresponding to #\z, 
;;   and 26 corresponding to a space and 27 a newline.
(define char->index
  (let ((a-index (char->integer #\a)))
    (lambda (ch)
      (cond
       [(equal? ch #\space)
	26]
       [(equal? ch #\newline)
	27]
       [else
	(- (char->integer ch) a-index)]))))

;;
;; Procedure
;;   get-count
;;
;; Purpose
;;   Extract the count entry from a vector "matrix"
;;
;; Parameters
;;   counts, a vector
;;   index-1, an integer
;;   index-2, an integer
;;
;; Produces
;;   count, a value
;;
;; Preconditions
;;   index-1, index2 >= 0
;;   counts is a vector of length less than index-1
;;   (vector-ref counts index-1) is a vector of length less than index-2
;;
;; Postconditions
;;   count is the value at counts[index1,index2]
(define get-count
  (lambda (counts index-1 index-2)
    (vector-ref (get-count-slice counts index-1) index-2)))


;;
;; Procedure
;;   get-count-slice
;;
;; Purpose
;;   Get all values for the first index in a matrix
;;
;; Parameters
;;   counts, a vector
;;   index-1, an integer
;;
;; Produces
;;   slice, a vector
;;
;; Preconditions
;;   index-1 >= 0
;;   counts is a vector of length less than index-1
;;
;; Postconditions
;;   slice is the vector at counts[index1,:], i.e., 
;;   (vector-ref counts index-1)
(define get-count-slice vector-ref)

;;
;; Procedure
;;   increment-count!
;;
;; Purpose
;;   Increment the count in a matrix
;;
;; Parameters
;;   counts, a vector
;;   index-1, an integer
;;   index-2, an integer
;;
;; Produces
;;   [Nothing. Called for side-effect.]
;;
;; Preconditions
;;   index-1, index2 >= 0
;;   counts is a vector of length less than index-1
;;   (vector-ref counts index-1) is a vector of length less than index-2
;;
;; Postconditions
;;   counts[index1,index2]++

(define increment-count!
  (lambda (counts index-1 index-2)
    (let* ((slice (get-count-slice counts index-1)) ; Extract slice and
	   (count (vector-ref slice index-2)))      ; current count
      (vector-set! slice index-2 (+ 1 count)))))    ; Set incremented value

;;
;; Procedure
;;   count-conditionals!
;;
;; Purpose
;;   Count the number of times correct and corrupt characters are paired
;;
;; Parameters
;;   correct-filename, a string
;;   corrupt-filename, a string
;;   counts, a vector
;;
;; Produces
;;   [Nothing. Called for side-effect.]
;;
;; Preconditions
;;   correct-filename refers to a valid text file
;;   corrupt-filename refers to a valid text file
;;   The files pointed to by correct-filename and corrupt-filename 
;;      have the same length, and
;;      contain only characters in [a-z] or whitespace
;;   counts is a vector of length 28
;;   Each entry in counts is a vector of length 28
;;   Each entry within those member vectors is a number
;;
;; Postconditions
;;   (get-count counts correct corrupt) is incremented for each
;;     corresponding character pair in the files pointed to by
;;     correct-filename and corrupt-filename, respectively.
(define count-conditionals!
  (lambda (correct-filename corrupt-filename counts)
	  ; Open fies for reading
    (let ((correct-inport (open-input-file correct-filename))
	  (corrupt-inport (open-input-file corrupt-filename)))
      (let loop ((correct-ch (read-char correct-inport)) ; Read correct char
		 (corrupt-ch (read-char corrupt-inport))); Read corrupted char
	(cond
	 ((eof-object? correct-ch)               ; All done?
	  (close-input-port correct-inport)  ; Close up and 
	  (close-input-port corrupt-inport))
	  ;counts)                             ; Return counts
	 (else
	  (increment-count! counts            ; increment counts with
			    (char->index correct-ch) ; correct character index
			    (char->index corrupt-ch)); corrupt character index
	  (loop (read-char correct-inport)
		(read-char corrupt-inport)))))))) ; loop, reading new next chars


;;
;; Procedure
;;   vector-sum
;;
;; Purpose
;;   Tally the numerical entries in a vector
;;
;; Parameters
;;   vec, a vector
;;
;; Produces
;;   total, a number
;;
;; Preconditions
;;   (number? (vector-ref vec i)) holds for 0 <= i < (vector-length vec)
;;
;; Postconditions
;;   total == (apply + (vector->list vec))
(define vector-sum
  (lambda (vec)
    (let loop ((total 0) ; Total-so-far
	       (index (- (vector-length vec) 1))) ; Count-down loop over indices
      (if (negative? index) ; Done when negative
	  total ; Return total
	  (loop (+ total (vector-ref vec index)) ; Loop, adding to total-so-far
		(- index 1))))))                 ; and decrementing index

;;
;; Procedure
;;   vector-map!
;;
;; Purpose
;;   Apply a (side-effecting) procedure to each entry in vector
;;
;; Parameters
;;   vec, a vector
;;   proc!, a procedure
;;
;; Produces
;;   vec, the same vector
;;
;; Preconditions
;;   proc! is a unary procedure
;;   All entries in vector are valid parameters to proc!
;;
;; Postconditions
;;   Each entry in vec becomes (proc! (vector-ref vec i))
(define vector-map!
  (lambda (vec proc!)
    (let loop ((index (- (vector-length vec) 1))) ; Loop, counting down
      (cond
       ((>= index 0) ; Valid index?
	(vector-set! vec index (proc! (vector-ref vec index))) ; Apply proc!
	(loop (- index 1))) ; Loop to next
       (else
	vec)))))

;;
;; Procedure
;;   normalize-counts!
;;
;; Purpose
;;   Normalize non-negative count vectors as conditional probabilities
;;
;; Parameters
;;   counts, a vector
;;
;; Produces
;;   [Nothing. Called for side-effect.]
;;
;; Preconditions
;;   Each entry in counts is a vector
;;   Each entry in the sub-vector is a number
;;
;; Postconditions
;;   The sum of each "row" in the structure is one:
;;     (vector-sum (get-count-slice counts index)) == 1 
;;       for 0 <= index < (vector-length counts)
;;
;; Practica 
;;    To functionally nest calls, normalize-counts! relies on
;;    the property of vector-map! that returns the modified vector
(define normalize-counts!
  (lambda (counts)
    (vector-map! counts 
		 (lambda (slice) ; For each slice
		   (let ((total (vector-sum slice))) ; Total mass in slice, and
		     ; Divide each entry in slice by total (so it sums to 1)
		     (vector-map! slice (lambda (count) (/ count total))))))
    (void))) ; Produce a void result (avoiding excessive output

;;
;; Procedure
;;   likelihood
;;
;; Purpose
;;   Calculate the exact likelihood of a corrupted file given existing counts
;;
;; Parameters
;;   correct-filename, a string
;;   corrupt-filename, a string
;;   counts, a vector
;;
;; Produces
;;   lik, a number
;;
;; Preconditions
;;   correct-filename refers to a valid text file
;;   corrupt-filename refers to a valid text file
;;   The files pointed to by correct-filename and corrupt-filename 
;;      have the same length, and
;;      contain only characters in [a-z] or whitespace
;;   normalized-counts is a vector of length 28
;;   Each entry in counts is a vector of length 28
;;   Each entry within those member vectors is a non-negative number
;;
;; Postconditions
;;   lik is the product of all entries counts[correct,corrupt] for each observed
;;     pair correct (a character from the file pointed to by correct-filename) 
;;     and corrupt (the corresponding character from the file pointed to by 
;;     corrupt-filename)
;;
;; Practica
;;   Because the likelihood is calculated exactly if the counts are exact, 
;;     this procedure is prohibitively slow.
(define likelihood
  (lambda (correct-filename corrupt-filename normalized-counts)
    (let (; Open files for reading
	  [correct-inport (open-input-file correct-filename)]
	  [corrupt-inport (open-input-file corrupt-filename)])
      (let loop ((lik 1) ;  Multiplicative identity
		 (correct-ch (read-char correct-inport)) ; Read correct char
		 (corrupt-ch (read-char corrupt-inport))); Read corrupted char
	(cond
	 ((eof-object? correct-ch)               ; All done?
	  (close-input-port correct-inport)  ; Close up and 
	  (close-input-port corrupt-inport)
	  lik)                             ; Return counts
	 (else
	  (loop (* lik (get-count normalized-counts 
				  (char->index correct-ch)
				  (char->index corrupt-ch))) ; loop, multiplying
		(read-char correct-inport)        ; observed likelihood and 
		(read-char corrupt-inport)))))))) ; reading new next chars


;;
;; Procedure
;;   log-likelihood
;;
;; Purpose
;;   Calculate inexact log-likelihood of a corrupted file given existing counts
;;
;; Parameters
;;   correct-filename, a string
;;   corrupt-filename, a string
;;   normalized-counts, a vector
;;
;; Produces
;;   lik, a number
;;
;; Preconditions
;;   correct-filename refers to a valid text file
;;   corrupt-filename refers to a valid text file
;;   The files pointed to by correct-filename and corrupt-filename 
;;      have the same length, and
;;      contain only characters in [a-z] or whitespace
;;   normalized-counts is a vector of length 28
;;   Each entry in normalized-counts is a vector of length 28
;;   Each entry within those member vectors is a non-negative number
;;
;; Postconditions
;;   lik is the sum of the natural log of all entries counts[correct,corrupt] 
;;     for each observed pair correct (a character from the file pointed to by 
;;     correct-filename) and corrupt (the corresponding character from the file 
;;     pointed to by corrupt-filename)
;;
;; Practica
;;   Because the log-likelihood is calculated inexactly even if the counts are 
;;     exact, this procedure is fast, but at the potential cost of accuracy.
(define log-likelihood
  (lambda (correct-filename corrupt-filename normalized-counts)
    (let (; Open files for reading
	  [correct-inport (open-input-file correct-filename)]
	  [corrupt-inport (open-input-file corrupt-filename)])
      (let loop ((loglik 0) ; Log of multiplicative identity: log(1) = 0
		 (correct-ch (read-char correct-inport)) ; Read correct char
		 (corrupt-ch (read-char corrupt-inport))); Read corrupted char
	(cond
	 [(eof-object? correct-ch)               ; All done?
	  (close-input-port correct-inport)  ; Close up and 
	  (close-input-port corrupt-inport)
	  loglik]                             ; Return counts
	 [else
	  ; Recall that multiplication changes to addition in log space
	  (loop (+ loglik (log (get-count normalized-counts   ; loop, 
				  (char->index correct-ch)    ; "multiplying"
				  (char->index corrupt-ch)))) ; observed 
		(read-char correct-inport)                    ; likelihood and 
		(read-char corrupt-inport))]))))) ; reading new next chars
