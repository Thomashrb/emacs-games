;;; guessnum --- summary
;; a one player game where you attempt to guess a random number
;;; Commentary:
;; activate using (guessnum) and play with the spacebar
;;; Code:
(require 'seq)

(defvar *guessnum-board* nil
  "The guessnum board.")

(defvar *guessnum-answer* nil
  "The guessnum number that is the answer for this game.")

(defvar *guessnum-size* 9
  "The initial count of numbers to guess from.")

(defvar *guessnum-guess-count* nil
  "Number of guesses made.")

(defun guessnum ()
  "Play guessnum."
  (interactive)
  (switch-to-buffer "guessnum")
  (guessnum-mode)
  (guessnum-init))

(define-derived-mode guessnum-mode special-mode "guess-num"
  (setq font-lock-defaults '((("_" . font-lock-constant-face)
                              ("WINNER\\|hit" . font-lock-type-face))))
  (define-key guessnum-mode-map (kbd "SPC") 'guessnum-mark))

(defun guessnum-init-board ()
  "Initialize and reset the board."
  (dotimes (i *guessnum-size*)
    (aset *guessnum-board* i i)))

(defun guessnum-init ()
  "Initialize a guessnum game."
  ;; blow up font size. this may be too big depending on screen resolution
  (buffer-face-set '(:family "Monospace" :height 900))
  (guessnum-init-board)
  (setq *guessnum-answer* (random *guessnum-size*))
  (setq *guessnum-guess-count* 0)
  (guessnum-print-board))

(defun guessnum-board-to-string (board)
  "Convert BOARD to printable string without spaces."
  (let* ((string-with-parenthesis (format "%S" board))
         (end (- (length string-with-parenthesis) 1)))
    (replace-regexp-in-string "\s" ""
                              (replace-regexp-in-string "checked" "_" (substring string-with-parenthesis 1 end)))))

(defun guessnum-print-board ()
  "Show current board."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (guessnum-board-to-string *guessnum-board*))
    (insert "\n\n")
    (if (seq-position *guessnum-board* 'hit)
        (insert "WINNER")
      (insert (concat "Num guesses: "  (format "%S" *guessnum-guess-count*))))))

(defun guessnum-mark ()
  "Mark cursored square as checked by setting it to 'checked or 'hit."
  (interactive)
  (let ((index (current-column)))
    (if (equal *guessnum-answer* index)
        (aset *guessnum-board* index 'hit)
      (aset *guessnum-board* index 'checked)))
  (setq  *guessnum-guess-count* (1+ *guessnum-guess-count*))
  (guessnum-print-board))

;;; guessnum.el ends here
