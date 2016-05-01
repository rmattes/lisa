
(defun lisa-debugger ()
  (translate-logical-pathname "lisa:debugger;lisa-debugger.lisp"))

;;; Sets up the environment so folks can use the non-portable form of REQUIRE
;;; with some implementations...

#+:allegro
(setf system:*require-search-list*
  (append system:*require-search-list*
          `(:newest ,(lisa-debugger))))

#+:clisp
(setf custom:*load-paths*
  (append custom:*load-paths* `(,(lisa-debugger))))

#+:openmcl
(pushnew (pathname-directory (lisa-debugger)) ccl:*module-search-path* :test #'equal)

#+:lispworks
(let ((loadable-modules `(("lisa-debugger" . ,(lisa-debugger)))))
  (lw:defadvice (require lisa-require :around)
      (module-name &optional pathname)
    (let ((lisa-module
           (find module-name loadable-modules
                 :test #'string=
                 :key #'car)))
      (if (null lisa-module)
          (lw:call-next-advice module-name pathname)
        (lw:call-next-advice module-name (cdr lisa-module))))))
