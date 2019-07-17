;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((chicken-mode
  . ((eval . (put 'chicken::condition-case		'scheme-indent-function 1))
     (eval . (put 'chicken::with-exception-handler	'scheme-indent-function 1))
     (eval . (put 'chicken::make-property-condition	'scheme-indent-function 1))
     )))

;;; end of file
