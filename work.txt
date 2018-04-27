#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)
(include "work2.rkt")




(define objects '((2 "a silver dagger")
                  (3 " a shield")
                  (1 "a gold coin")
                  (4 "knights helmet")
                  (5 "car keys")
                  (6 "bottle of water")))

(define descriptions '((0 "The aim of this game is to choose the right path directions to complete the game example 'north'")
                       (1 "You are in the lobby")
                       (2 "You are in the hallway")
                       (3 "You are in a swamp")
                       (4 "you are in the dungeon")
                       (5 "now you are in the town")
                       (6 "you are now on the hill")
                       (7 "you are now in Tafari's headquaters")
                       (8 "you are now at the office doorway")
                       (9"you are now free!!!!! well done")))



(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define actions `(,@look ,@quit,@pick,@put,@inventory))

(define decisiontable `((1 ((north) 2) ((north west) 3) ((ASK) 0) ,@actions)
                        (2 ((south) 2) ((south west) 4),@actions)
                        (3 ((south east) 5) ((east) 4),@actions)
                        (4 ((west) 1) ((way out) 9),@actions)))

;hash table this will keep track of what is in the room and what is being carried
;object database, defining objects

(define objectdb (make-hash))
(define inventorydb (make-hash))

(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

  (define (add-objects db)
    (for-each
     (lambda (r)
       (add-object db (first r) (second r))) objects))
  
  (add-objects objectdb)

  ;displaying objects

  (define (display-objects db id)
    (when (hash-has-key? db id)
      (let* ((record (hash-ref db id))
             (output (string-join record " and ")))
        (if (eq? id 'bag)
            (printf "you are carrying ~a.\n" output)
            (printf "you can see ~a.\n" output)))))

;removing objects from rooms

(define (remove-object-from-room db id str)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "i don't see that item in the room!\n"))
            (else
             (printf "added ~a to your bag.\n" (first item))
             (add-object inventorydb 'bag (first item))
             (hash-set! db id result))))))
;removing objects from your inventory

(define (remove-object-from-inventory db id str)
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
           (cond ((null? item)
                  (printf "Removed ~a from your bag.\n" (first item))
                  (add-object objectdb id (first item))
                  (hash-set! db 'bag result))))))
  ;calling functions

  (define (pick-item id input)
    (let ((item (string-join (cdr (string-split input)))))
          (remove-object-from-room objectdb id item)))

    (define (put-item id input)
      (let ((item (string-join (cdr (string-split input)))))
        (remove-object-from-inventory inventorydb id item)))
  
  (define (display-inventory)
    (display-objects inventorydb 'bag))



;(define (get-directions id)
;  (let ((record (assq id decisiontable)))
;    (let ((result (filter (lambda (n) (number? (second n))) (cdr record))))
;      (printf "You can see exits to the ")nor
;      (for-each (lambda (direction) (printf "~a " (first direction))) result))
;      (printf "\n")))

(define (slist->string l)
  (string-join (map symbol->string l)))

(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-response id)
  (car (assq-ref descriptions id)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))


(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))


(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (if description
        (printf "~a\n> " (get-response id))
        (printf ""))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (format #t "huh? I didn't understand that!\n")
               (loop id #f))
              ((eq? response 'look)
               (get-directions id)
               (loop id #f))
              ((eq? response 'pick)
               (pick-item id input)
               (loop id #f))
              ((eq? response 'drop)
               (put-item id input)
               (loop id #f))
              ((eq? response 'object)
               (display-objects id)
               (loop id #f))
              ((eq? response 'inventory)
               (display-inventory)
               (loop id #f))
              ((eq? response 'quit)
               (format #t "So Long, and Thanks for playing...\n")
               (exit)))))))



(startgame 1)
