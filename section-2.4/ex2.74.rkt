#lang sicp

;; a)

(define (get-record name file)
  ;; Looks up the appropriate get-record method by checking the type-tag of the file
  ((get 'get-record (type-tag file)) name))

(define (install-engineering-records)
  (define file (lambda ()
                 ;; read file and get list of records
                 ;; tag each record with division name
                 ))
  (define (get-record name)
    ;; lookup records in file and return the appropriate record
    )
  
  (define (tag x) (attach-tag 'engineering x))
  ;; Install the get-record method for this file
  ;; Also ensure that the record is properly tagged before returning
  (put 'get-record 'engineering (lambda (name)
                                  (tag (get-record name))))
  (tag records))

;; b)

(define (install-engineering-records)
  (define file (lambda ()
                 ;; read file and get list of records
                 ;; tag each record with division name
                 ))
  (define (get-record name)
    ;; lookup records in file and return the appropriate record
    )

  (define (name record) ...)
  (define (salary record) ...)
  (define (address record) ...)

  (define (tag x) (attach-tag 'engineering x))
  (put 'get-record 'engineering (lambda (name)
                                  (tag (get-record name))))
  (put 'get-name '(engineering) name)
  (put 'get-salary '(engineering) salary)
  (tag records))

(define (get-name record) (apply-generic 'get-name record))
(define (get-salary record) (apply-generic 'get-salary record))

;; c)

(define (find-employee-record name files)
  (if (null? files)
      false
      (let ((record (get-record name (car files))))
        (or record (find-employee-record name (cdr files))))))

;; d) When a new company is acquired,
;;    * For personnel incorporated in existing divisions, only their records need to be added to the division files
;;    * For every new divisions created, i) a type-tag needs to be assigned, and ii) the personnel file needs to be created. Everything else remains untouched.
