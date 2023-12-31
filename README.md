# time-tracking-mode
Record your working time when you are using Emacs.

### Setup and usage:
Put `time-tracking-mode.el` to your load-path. Put the following code to your emacs startup file:
```lisp
(require 'time-tracking-mode)
(time-tracking-mode t)
```

Use the following function to see how long have worked today:
```lisp
M-x time-tracking-mode-summarize-today
```

Use the following function to see how long have worked this week:
```lisp
M-x time-tracking-mode-summarize-week
```
```lisp
;; will be displayed in your mini-buffer
Mon: 3h32m, Tue: 4h55m, Wed: 26m, Thu: 2h35m, Fri: 1h36m, Sat: 1h30m, 
```

This minor mode determines if you are working and records every `time-tracking-mode-update-interval-sec` seconds. You can customize it by:
```lisp
(setq time-tracking-mode-update-interval-sec 30)  ;; Default 60
```
