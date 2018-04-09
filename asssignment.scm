(include "charmodel.scm")
(include "hmm.scm")

(define o1 "original/1.txt")
(define o3 "original/3.txt")
(define o4 "original/4.txt")
(define o7 "original/7.txt")
(define o8 "original/8.txt")
(define o9 "original/9.txt")
(define o10 "original/10.txt")
(define o11 "original/11.txt")
(define o15 "original/15.txt")
(define o16 "original/16.txt")
(define o18 "original/18.txt")
(define o22 "original/22.txt")

(define t1-1 "corrupted/typist1/1.txt")
(define t1-2 "corrupted/typist1/2.txt")
(define t1-8 "corrupted/typist1/8.txt")
(define t1-14 "corrupted/typist1/14.txt")
(define t1-16 "corrupted/typist1/16.txt")
(define t1-17 "corrupted/typist1/17.txt")

(define t2-4 "corrupted/typist2/4.txt")
(define t2-9 "corrupted/typist2/9.txt")
(define t2-12 "corrupted/typist2/12.txt")
(define t2-13 "corrupted/typist2/13.txt")
(define t2-18 "corrupted/typist2/18.txt")
(define t2-19 "corrupted/typist2/19.txt")

(define u3 "corrupted/unknown/3.txt")
(define u5 "corrupted/unknown/5.txt")
(define u6 "corrupted/unknown/6.txt")
(define u7 "corrupted/unknown/7.txt")
(define u10 "corrupted/unknown/10.txt")
(define u11 "corrupted/unknown/11.txt")
(define u15 "corrupted/unknown/15.txt")
(define u20 "corrupted/unknown/20.txt")
(define u21 "corrupted/unknown/21.txt")
(define u22 "corrupted/unknown/22.txt")
(define u23 "corrupted/unknown/23.txt")
(define u24 "corrupted/unknown/24.txt")

(define skip-first-tests #f) ; prevents first tests so it runs faster

(define skip-second-tests #f) ; prevents second tests so it runs faster

;;; Procedure:
;;;  ln-to-log10
;;; Parameters:
;;;  num
;;; Purpose:
;;;  transform natural log to log10 using change of base formula
;;; Produces:
;;;  n, a number
(define ln-to-log10
  (lambda (num)
    (/ num (log 10))))

;;; Procedure:
;;;  first-tests
;;; Parameters:
;;;  prior-typist-1, a number
;;;  prior-typist-2, a number
;;;  corrupted-typist-1, a filename
;;;  original-typist-1, a filename
;;;  corrupted-typist-2, a filename
;;;  original-typist-2, a filename
;;;  to-test, a filename
;;; Purpose:
;;;  Calculate Jaynes' evidence that typist1 wrote to-test,
;;;  training on the given files
;;; Produces:
;;;  e, a number
;;; Preconditions:
;;;  Prior probability that typist 1 wrote it is
;;;  (prior-typist-1 / (prior-typist-1 + prior-typist-2))
;;;  all filenames point to readable files
;;; Postconditions:
;;;  e is the evidence that typist1 wrote to-test, given
;;;  our training files
(define first-tests
  (lambda
      (prior-typist-1
       prior-typist-2
       corrupted-typist-1
       original-typist-1
       corrupted-typist-2
       original-typist-2
       to-test)
    (if
     skip-first-tests
     0
     (let ([typist-1-typo (create-counts 28 1)]
           [typist-2-typo (create-counts 28 1)]
           [trans (create-counts 28 1)]
           [prior-evidence
            (* 10 (ln-to-log10 (log (/ prior-typist-1 prior-typist-2))))])
       (count-transitions! original-typist-1 trans) 
       (count-transitions! original-typist-2 trans) ; train transition model
       (count-conditionals! original-typist-1 corrupted-typist-1 typist-1-typo)
       ; train typist 1 model
       (count-conditionals! original-typist-2 corrupted-typist-2 typist-2-typo)
       ; train typist 2 typo model
       (let ([marg (marginal-counts trans)])
         (normalize-marginal-counts! marg)
         (normalize-counts! typist-1-typo)
         (normalize-counts! typist-2-typo) ; normalize everything
         (+ prior-evidence
            (* 10 (ln-to-log10
                   (- (log-evidence to-test typist-1-typo trans marg)
                      (log-evidence to-test typist-2-typo trans marg)
                      ; difference in log likelihoods for evidence calculation
                      )))))))))

(display "we used a uniform prior for the writers of the letters") (newline)
(display "Training on letters 1 and 4, testing letter 8") (newline)
(display (first-tests 1 1 t1-1 o1 t2-4 o4 t1-8)) (newline)
(display "Training on letters 1 and 4, testing letter 16") (newline)
(display (first-tests 1 1 t1-1 o1 t2-4 o4 t1-16)) (newline)
(display "Training on letters 1 and 4, testing letter 9") (newline)
(display (first-tests 1 1 t1-1 o1 t2-4 o4 t2-9)) (newline)
(display "Training on letters 1 and 4, testing letter 18") (newline)
(display (first-tests 1 1 t1-1 o1 t2-4 o4 t2-18)) (newline)

(newline)

(display "Training on letters 8 and 4, testing letter 1") (newline)
(display (first-tests 1 1 t1-8 o8 t2-4 o4 t1-1)) (newline)
(display "Training on letters 8 and 4, testing letter 16") (newline)
(display (first-tests 1 1 t1-8 o8 t2-4 o4 t1-16)) (newline)
(display (display "Training on letters 8 and 4, testing letter 9")) (newline)
(display (first-tests 1 1 t1-8 o8 t2-4 o4 t2-9)) (newline)
(display "Training on letters 8 and 4, testing letter 18") (newline)
(display (first-tests 1 1 t1-8 o8 t2-4 o4 t2-18)) (newline)

(newline)

(display "Training on letters 16 and 4, testing letter 1") (newline)
(display (first-tests 1 1 t1-16 o16 t2-4 o4 t1-1)) (newline)
(display "Training on letters 16 and 4, testing letter 8") (newline)
(display (first-tests 1 1 t1-16 o16 t2-4 o4 t1-8)) (newline)
(display "Training on letters 16 and 4, testing letter 9") (newline)
(display (first-tests 1 1 t1-16 o16 t2-4 o4 t2-9)) (newline)
(display "Training on letters 16 and 4, testing letter 18") (newline)
(display (first-tests 1 1 t1-16 o16 t2-4 o4 t2-18)) (newline)

(newline)

(display "Training on letters 1 and 9, testing letter 8") (newline)
(display (first-tests 1 1 t1-1 o1 t2-9 o9 t1-8)) (newline)
(display "Training on letters 1 and 9, testing letter 16") (newline)
(display (first-tests 1 1 t1-1 o1 t2-9 o9 t1-16)) (newline)
(display "Training on letters 1 and 9, testing letter 4") (newline)
(display (first-tests 1 1 t1-1 o1 t2-9 o9 t2-4)) (newline)
(display "Training on letters 1 and 9, testing letter 18") (newline)
(display (first-tests 1 1 t1-1 o1 t2-9 o9 t2-18)) (newline)

(newline)

(display "Training on letters 8 and 9, testing letter 1") (newline)
(display (first-tests 1 1 t1-8 o8 t2-9 o9 t1-1)) (newline)
(display "Training on letters 8 and 4, testing letter 16") (newline)
(display (first-tests 1 1 t1-8 o8 t2-9 o9 t1-16)) (newline)
(display "Training on letters 8 and 4, testing letter 4") (newline)
(display (first-tests 1 1 t1-8 o8 t2-9 o9 t2-4)) (newline)
(display "Training on letters 8 and 4, testing letter 18") (newline)
(display (first-tests 1 1 t1-8 o8 t2-9 o9 t2-18)) (newline)

(newline)

(display "Training on letters 16 and 9, testing letter 1") (newline)
(display (first-tests 1 1 t1-16 o16 t2-9 o9 t1-1)) (newline)
(display "Training on letters 16 and 9, testing letter 8") (newline)
(display (first-tests 1 1 t1-16 o16 t2-9 o9 t1-8)) (newline)
(display "Training on letters 16 and 9, testing letter 4") (newline)
(display (first-tests 1 1 t1-16 o16 t2-9 o9 t2-4)) (newline)
(display "Training on letters 16 and 9, testing letter 18") (newline)
(display (first-tests 1 1 t1-16 o16 t2-9 o9 t2-18)) (newline)

(newline)

(display "Training on letters 1 and 18, testing letter 8") (newline)
(display (first-tests 1 1 t1-1 o1 t2-18 o9 t1-8)) (newline)
(display "Training on letters 1 and 18, testing letter 16") (newline)
(display (first-tests 1 1 t1-1 o1 t2-18 o9 t1-16)) (newline)
(display "Training on letters 1 and 18, testing letter 4") (newline)
(display (first-tests 1 1 t1-1 o1 t2-18 o9 t2-4)) (newline)
(display "Training on letters 1 and 18, testing letter 9") (newline)
(display (first-tests 1 1 t1-1 o1 t2-18 o9 t2-9)) (newline)

(newline)

(display "Training on letters 8 and 18, testing letter 1") (newline)
(display (first-tests 1 1 t1-8 o8 t2-18 o18 t1-1)) (newline)
(display "Training on letters 8 and 18, testing letter 16") (newline)
(display (first-tests 1 1 t1-8 o8 t2-18 o18 t1-16)) (newline)
(display "Training on letters 8 and 18, testing letter 4") (newline)
(display (first-tests 1 1 t1-8 o8 t2-18 o18 t2-4)) (newline)
(display "Training on letters 8 and 18, testing letter 9") (newline)
(display (first-tests 1 1 t1-8 o8 t2-18 o18 t2-9)) (newline)

(newline)

(display "Training on letters 16 and 18, testing letter 1") (newline)
(display (first-tests 1 1 t1-16 o16 t2-18 o18 t1-1)) (newline)
(display "Training on letters 16 and 18, testing letter 8") (newline)
(display (first-tests 1 1 t1-16 o16 t2-18 o18 t1-8)) (newline)
(display "Training on letters 16 and 18, testing letter 4") (newline)
(display (first-tests 1 1 t1-16 o16 t2-18 o18 t2-4)) (newline)
(display "Training on letters 16 and 18, testing letter 9") (newline)
(display (first-tests 1 1 t1-16 o16 t2-18 o18 t2-9)) (newline)



;;; Second bullet point

(define transition-model (create-counts 28 1))

;;; Procedure:
;;;  train-transition-model
;;; Parameters:
;;;  lst, a list of filenames
;;; Purpose:
;;;  trains a transition model on a list of files at once
;;; Produces:
;;;  nothing, called for side effects
;;; Preconditions:
;;;  each element of lst points to a readable file
;;; Postconditions:
;;;  transition-model, a global variable, will be updated to have been
;;;  trained on each file in lst
(define train-transition-model
  (lambda (lst)
    (if
     (null? lst)
     (values) ; no need to return anything
     (count-transitions! (car lst) transition-model))
    (if
     (null? lst)
     (values) ; no need to return anything
     (train-transition-model (cdr lst)))))


;;; Procedure:
;;;  train-typo-model
;;; Parameters:
;;;  originals, a list of files
;;;  corrupted, a list of files
;;;  count, a counts structure
;;; Purpose:
;;;  trains a typo model on a list of files at once
;;; Produces:
;;;  nothing, called for side effects
;;; Preconditions:
;;;  for each i, the ith element of originals is the original file for
;;;  the ith element of corrupted
;;; Postconditions:
;;;  counts will be updated to be trained on each pair of letters in the lists
(define train-typo-model
  (lambda (originals corrupted count)
    (if
     (null? originals)
     (values) ; no need to return anything
     (count-conditionals! (car originals) (car corrupted) count))
    (if
     (null? originals)
     (values) ; no need to return anything
     (train-typo-model (cdr originals) (cdr corrupted) count))))

(define originals (list o1 o8 o16 o4 o9 o18))

(define t1-originals (list o1 o8 o16))
(define t2-originals (list o4 o9 o18))

(define t1-corrupteds (list t1-1 t1-8 t1-16))
(define t2-corrupteds (list t2-4 t2-9 t2-18))

(train-transition-model originals)
(define marg (marginal-counts transition-model))
(normalize-marginal-counts! marg)

(define typist-1-counts (create-counts 28 1))
(define typist-2-counts (create-counts 28 1))
 
(train-typo-model t1-originals t1-corrupteds typist-1-counts)
(train-typo-model t2-originals t2-corrupteds typist-2-counts)

(normalize-counts! typist-1-counts)
(normalize-counts! typist-2-counts)
(normalize-counts! transition-model)

;;; Procedure:
;;;  second-test
;;; Parameters:
;;;  prior1, a number
;;;  prior2, a number
;;;  letter, a filename
;;; Purpose:
;;;  training on all available data, calculate Jaynes's evidence that
;;;  typist1 wrote letter
;;; Produces:
;;;  e, a number
;;; Preconditions:
;;;  typist-1-counts, typist-2-counts, transition-model, and marg
;;;  (global variables) are all appropriately trained
;;;  Prior probability that typist 1 wrote it is
;;;  (prior-typist-1 / (prior-typist-1 + prior-typist-2))
;;;  letter points to a readable file
;;; Postconditions:
;;;  e is Jaynes's evidence that typist1 wrote letter, given our trained models
(define second-test
  (lambda (prior1 prior2 letter)
    (if
     skip-second-tests
     0
     (* 10
        (+
         (ln-to-log10 (log (/ prior1 prior2)))
         (ln-to-log10
          (-
           (log-evidence letter typist-1-counts transition-model marg)
           (log-evidence letter typist-2-counts transition-model marg))))))))

(display "uniform prior for who wrote the letter") (newline)
(display "Training on all letters, testing letter 2 by typist 1") (newline)
(display (second-test 1 1 t1-2)) (newline)
(display "Training on all letters, testing letter 14 by typist 1") (newline)
(display (second-test 1 1 t1-14)) (newline)
(display "Training on all letters, testing letter 17 by typist 1") (newline)
(display (second-test 1 1 t1-17)) (newline)
(display "Training on all letters, testing letter 12 by typist 2") (newline)
(display (second-test 1 1 t2-12)) (newline)
(display "Training on all letters, testing letter 13 by typist 2") (newline)
(display (second-test 1 1 t2-13)) (newline)
(display "Training on all letters, testing letter 19 by typist 2") (newline)
(display (second-test 1 1 t2-19)) (newline)
(display "Training on all letters, testing unknown letter 5") (newline)
(display (second-test 1 1 u5)) (newline)
(display "Training on all letters, testing unknown letter 6") (newline)
(display (second-test 1 1 u6)) (newline)
(display "Training on all letters, testing unknown letter 20") (newline)
(display (second-test 1 1 u20)) (newline)
(display "Training on all letters, testing unknown letter 21") (newline)
(display (second-test 1 1 u21)) (newline)
(display "Training on all letters, testing unknown letter 23") (newline)
(display (second-test 1 1 u23)) (newline)
(display "Training on all letters, testing unknown letter 24") (newline)
(display (second-test 1 1 u24)) (newline)
       
    

