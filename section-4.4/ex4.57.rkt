#lang sicp

;; Define a rule that says that person 1 can replace person 2 if either person 1 does the same job as person 2 or someone who does person 1's job can also do person 2's job, and if person 1 and person 2 are not the same person.

(rule (can-replace ?p1 ?p2)
      (and (job ?p1 ?j1)
           (job ?p2 ?j2)
           (or (same ?j1 ?j2)
               (can-do-job j1 j2))))

           