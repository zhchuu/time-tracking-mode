;;; time-tracking-mode --- A minor mode that record your working time.

;;; Commentary:
;;
;; Record your working time when you are using Emacs.
;;
;; Wakatime-mode and activity-watch-mode are two similar modes that utilize a third-party app to record working time.
;; While these modes offer more advanced features and detailed analysis, they also raise my privacy concerns.
;; Personally, I prefer a more general overview of my working time rather than a detailed analysis.
;;
;; The time-tracking-mode is a simpler option that only records working time locally using pure Elisp, without
;; the additional features of third-party integration.
;;

;;; Setup and usage:
;;
;; Put time-tracking-mode.el to your load-path.
;; Put the following code to your emacs startup file:
;; (require 'time-tracking-mode)
;; (time-tracking-mode t)
;;
;; Use the following function to see how long have worked today:
;; M-x time-tracking-mode-summarize-today
;;

;;; TODO:
;;
;; [x] Customize the update interval.
;; [x] Create new dir for recording.
;; [ ] Better interface to show working history.
;;

;; Vars
(defvar time-tracking-mode-last-working-time nil)
(defvar time-tracking-mode-last-win-point nil)
(defvar time-tracking-mode-snapshot-dir (file-name-as-directory (concat user-emacs-directory "time-tracking-mode-log")))
(defvar time-tracking-mode-update-interval-sec 60)
(defvar time-tracking-mode-timer nil)

;; Funcs
(defun time-tracking-mode-today ()
  (format-time-string "%Y%m%d"))

(defun time-tracking-mode-update-last-working-time ()
  (setq time-tracking-mode-last-working-time (current-time)))

(defun time-tracking-mode-update-last-win-point ()
  (setq time-tracking-mode-last-win-point (window-point)))

(defun time-tracking-mode-format-time (&optional time)
  (if time
      (format-time-string "%Y%m%d %H:%M:%S" time)
    (format-time-string "%Y%m%d %H:%M:%S")))

(defun time-tracking-mode-dump-snapshot ()
  (let* ((elapsed-time (float-time (time-subtract (current-time) time-tracking-mode-last-working-time)))
         (start-time (time-tracking-mode-format-time time-tracking-mode-last-working-time))
         (end-time (time-tracking-mode-format-time))
         (today (time-tracking-mode-today))
         (snapshot-file (concat time-tracking-mode-snapshot-dir today)))
    (message "You have worked for %.2f seconds [%s - %s]." elapsed-time start-time end-time)
    (with-temp-buffer
      (setq-local write-region-annotate-functions nil)
      (insert (format "%s,%s,%.2f\n" start-time end-time elapsed-time))
      (write-region (point-min) (point-max) snapshot-file 'append))
    ))

(defun time-tracking-mode-checkpoint ()
  (progn
    (if (not (equal time-tracking-mode-last-win-point (window-point)))
        (if time-tracking-mode-last-working-time
            (time-tracking-mode-dump-snapshot)))
    (time-tracking-mode-update-last-working-time)
    (time-tracking-mode-update-last-win-point)
    ))

(defun time-tracking-mode-sumup-secs-helper (lst)
  "Sum up the third element of the list."
  (if (null lst)
      0
    (let ((sum 0))
      (dolist (item lst)
        (setq sum (+ sum (string-to-number (nth 2 (split-string item ","))))))
      sum)
    ))

(defun time-tracking-mode-summarize-today ()
  "Describe the time worked today."
  (interactive)
  (let* ((today (time-tracking-mode-today))
         (snapshot-file (concat time-tracking-mode-snapshot-dir today))
         (lines (with-temp-buffer
                  (insert-file-contents snapshot-file)
                  (split-string (buffer-string) "\n" t)))
         (today-total-secs (time-tracking-mode-sumup-secs-helper lines)))
    (if (> today-total-secs 3600)
        (message "You have worked for %.2f hours today." (/ today-total-secs 3600))
      (if (> today-total-secs 60)
          (message "You have worked for %.2f mins today." (/ today-total-secs 60))
        (message "You have worked for %.2f seconds today." today-total-secs)))
    )
  )

(defun time-tracking-mode-create-snapshot-dir ()
  (if (null (file-directory-p time-tracking-mode-snapshot-dir))
      (make-directory time-tracking-mode-snapshot-dir)))

(defun time-tracking-mode-turn-on ()
  (progn
    (time-tracking-mode-create-snapshot-dir)
    (time-tracking-mode-update-last-working-time)
    (time-tracking-mode-update-last-win-point)
    (setq time-tracking-mode-timer (run-at-time nil time-tracking-mode-update-interval-sec 'time-tracking-mode-checkpoint))
    (message "Turn on time-tracking-mode done.")
    ))

(defun time-tracking-mode-turn-off ()
  (progn
    (if time-tracking-mode-timer
        (cancel-timer time-tracking-mode-timer))
    (setq time-tracking-mode-timer nil)
    (message "Turn off time-tracking-mode done.")
    ))

;; Hooks
;; (add-hook 'after-init-hook #'time-tracking-mode-update-last-working-time)
;; (add-hook 'after-init-hook #'time-tracking-mode-update-last-win-point)

;;;###autoload
(define-minor-mode time-tracking-mode
  "Record your working time when you are using Emacs."
  :global t
  (if time-tracking-mode
      (time-tracking-mode-turn-on)
    (time-tracking-mode-turn-off))
  )

(provide 'time-tracking-mode)

;;; time-tracking-mode.el ends here
