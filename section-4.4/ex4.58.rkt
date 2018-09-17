#lang sicp

;; Define a rule that says that a person is a ``big shot'' in a division if the person works in the division but does not have a supervisor who works in the division.

(rule (big-shot ?person)
      (and (supervisor ?person ?boss)
           (or (same ?person ?boss)
               (and (job ?person (?person-div . ?))
                    (job ?boss (?boss-div . ?))
                    (not (same ?person-div ?boss-div))))))
