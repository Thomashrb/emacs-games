;;; tictactoe --- summary
;; a two player tic tac toe impl for Emacs
;;; Commentary:
;; based on: https://github.com/viniciustozzi/tictactoe-emacs/blob/master/tictactoe.el
;; activate using (tictactoe) and play with the spacebar
;;; Code:

(defvar *tictactoe-board* nil
  "The tic tac toe board.")

(defvar *tictactoe-size* 3
  "The height and width of the board.")

(defvar *tictactoe-current-player* nil
  "The character of the players whos turn it is to mark the board.")

(defun tictactoe ()
  "Play tic tac toe."
  (interactive)
  (switch-to-buffer "tictactoe")
  (tictactoe-mode)
  (tictactoe-init))

(define-derived-mode tictactoe-mode special-mode "tic-tac-toe"
  (define-key tictactoe-mode-map (kbd "SPC") 'tictactoe-mark))

(defun tictactoe-init ()
  "Initialize the board as a vector filled with periods."
  ;; blow up font size. this may be too big depending on screen resolution
  (buffer-face-set '(:family "Monospace" :height 500))
  (setq *tictactoe-board* (make-vector (* *tictactoe-size*
                                          *tictactoe-size*) ?\.))
  (setq *tictactoe-current-player* ?\X)
  (tictactoe-print-board))

(defun tictactoe-print-board ()
  "Show current board."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *tictactoe-size*)
      (dotimes (column *tictactoe-size*)
        (insert (tictactoe-get-square row column)))
      (insert "\n"))))

(defun tictactoe-get-square (row column)
  "Get the value of the (ROW COLUMN) square of the board."
  (elt *tictactoe-board*
       (+ column
          (* row
             *tictactoe-size*))))

(defun tictactoe-set-square (row column value)
  "Set the (ROW COLUMN VALUE) square of the board to VALUE."
  (aset *tictactoe-board*
       (+ column
          (* row
             *tictactoe-size*))
       value))

(defun tictactoe-swap-players ()
  "Swap from the current player to the other player X <-> O."
  (setq *tictactoe-current-player*
        (if (char-equal *tictactoe-current-player* ?\X)
            ?\O
          ?\X)))

(defun tictactoe-mark ()
  "Mark cursored square."
  (interactive)
  (let ((row (1- (line-number-at-pos)))
        (column (current-column)))
    (tictactoe-set-square row column *tictactoe-current-player*))
  (tictactoe-print-board)
  (tictactoe-swap-players))

;;; tictactoe.el ends here
