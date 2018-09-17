#lang sicp

;; a. the names of all people who are supervised by Ben Bitdiddle, together with their addresses

(and (supervisor ?who (Bitdiddle Ben))
     (address ?who ?where))

;; b. all people whose salary is less than Ben Bitdiddle's, together with their salary and Ben Bitdiddle's salary

(and (salary ?person ?minion-salary)
     (salary (Bitdiddle Ben) ?ben-salary)
     (lisp-value < ?minion-salary ?ben-salary))

;; c. all people who are supervised by someone who is not in the computer division, together with the supervisor's name and job.

(and (supervisor ?minion ?super)
     (job ?minion ?minion-job)
     (job ?super ?super-job)
     (not (job ?super (computer . ?))))
