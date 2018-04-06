;; I just copy-pasted some procedures that made my life easier last time in here
;; Not sure if the documentation is still correct

;; Procedure
;;   original
;; 
;; Purpose
;;   Produces a relative path from a letter string to an original
;;     text file indicated by letter-num
;;
;; Preconditions
;;   letter-num, a string that correspond to a valid original letter
;;     number; acceptable inputs are "1", "8", "16", "4", "9", "18",
;;     "3", "7", "10", "11", "15", and "22"
;;
;; Postconditions
;;   A relataive path pointing to a valid file in form of a string
(define original
  (lambda (str)
    (string-append "original/" str ".txt")))

;; Procedure
;;   corrupt-typist1 
;; 
;; Purpose
;;   Produces a relative path from a letter string to a corrupt
;;     file by typist1 indicated by letter-num
;;
;; Preconditions
;;   letter-num, a string that correspond to a valid original letter
;;     number; acceptable inputs are "1", "8", and "16".
;;
;; Postconditions
;;   A relative path pointing to a valid file in form of a string
(define corrupt-typist1
  (lambda (str)
    (string-append "corrupted/typist1/" str ".txt")))

;; Procedure
;;   corrupt-typist2 
;; 
;; Purpose
;;   Produces a relative path from a letter string to a corrupt
;;     file by typist2 indicated by letter-num
;;
;; Preconditions
;;   letter-num, a string that correspond to a valid original letter
;;     number; acceptable inputs are "4", "9", and "18".
;;
;; Postconditions
;;   A relative path pointing to a valid file in form of a string
(define corrupt-typist2
  (lambda (str)
    (string-append "corrupted/typist2/" str ".txt")))

;; Procedure
;;   corrupt-unknown 
;; 
;; Purpose
;;   Produces a relative path from a letter string to an
;;     unattributed corrupt file indicated by letter-num
;;
;; Preconditions
;;   letter-num, a string that correspond to a valid original letter
;;     number; acceptable inputs are "3", "7", "10", "11", "15",
;;     and "22"
;;
;; Postconditions
;;   A relative path pointing to a valid file in form of a string
(define corrupt-unknown
  (lambda (str)
    (string-append "corrupted/unknown/" str ".txt")))
