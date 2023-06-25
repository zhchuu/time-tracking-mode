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
