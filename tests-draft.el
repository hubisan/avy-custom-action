;;; POSITIONS

;;;;; Initial position
(progn
  (setq avy-custom-action-initial-position nil)
  (avy-custom-action-restore-initial-position) ; error
  (= (length avy-custom-action-initial-position) 0)
  (avy-custom-action--store-initial-position)
  ;; ((136 . #<window 9 on tests-draft.el>))
  (= (length avy-custom-action-initial-position) 1)
  (integerp (car (car avy-custom-action-initial-position)))
  (windowp (cdr (car avy-custom-action-initial-position)))
  (avy-custom-action-restore-initial-position)
  ;; t
  (avy-custom-action--store-initial-position)
  ;; ((480 . #<window 9 on tests-draft.el>))
  (= (length avy-custom-action-initial-position) 1)
  (integerp (car (car avy-custom-action-initial-position)))
  (windowp (cdr (car avy-custom-action-initial-position)))
  (avy-custom-action-restore-initial-position))


;;;;; Positions
(progn
  (setq avy-custom-action-positions nil)
  (avy-custom-action-goto-position 0) ; error
  ;; LOOP ------------------------------
  (avy-custom-action--store-position)
  ;; ((759 . #<window 9 on tests-draft.el>))
  (= (length avy-custom-action-positions) 1)
  (integerp (car (nth 0 avy-custom-action-positions)))
  (windowp (cdr (nth 0 avy-custom-action-positions)))
  (avy-custom-action--store-position)
  ;; ((894 . #<window 9 on tests-draft.el>) (759 . #<window 9 on tests-draft.el>))
  (= (length avy-custom-action-positions) 2)
  (integerp (car (nth 1 avy-custom-action-positions)))
  (windowp (cdr (nth 1 avy-custom-action-positions)))
  ;; ... (do x times)
  (avy-custom-action-goto-position 0)
  (avy-custom-action-goto-position 1)
  (avy-custom-action-goto-position 2) ; error
  (avy-custom-action-goto-position 0 t)
  (avy-custom-action-goto-position 1 t)
  (avy-custom-action-goto-position 2 t) ; error
  )

;;;;; Go to position and check if at right place
;; Open two predefined buffers
;; Jump to point
;; Store position
;; Move somewhere else
;; Go to position
;; Store position again
;; Compare positions
