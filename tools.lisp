
(defun describe-to-string (obj)
  "Return the output of `DESCRIBE` as a string."
  (with-output-to-string (s)
    (let ((*debug‑io* s))
      (describe obj))))

(defun read-form-from-string (s)
  "Read the first Lisp form from the string S.
   Signals an error if nothing can be read."
  (with-input-from-string (in s)
    (let ((form (read in nil nil)))
      (unless form
        (error "No form found in input string: ~S" s))
      form)))

(defun safe-princ-to-string (obj)
  "Like PRIN1 → string, but catches any printer errors."
  (with-output-to-string (out)
    (handler-case (princ obj out)
      (error (c) (format out "<printer error: ~A>" c)))))

;;
;;Tools
;;

;;Evalute Common Lisp code
(defun eval-form (source-string &optional (env '("COMMON-LISP")) (bindings nil))
  "Evaluate the first Lisp form in SOURCE-STRING in a controlled environment.
Returns a plist with :values, :error, and :output."
  (let ((form (read-form-from-string source-string))
        (allowed-packages (mapcar #'find-package env)))
    (labels ((allowedp (sym)
               (member (symbol-package sym) allowed-packages)))
      (let ((result nil)
            (printed-output nil)
            (error-data nil))
        (handler-case
            (progn
              (with-output-to-string (out)
                (let ((*standard-output* out)
                      (*debug-io* out)
                      (*error-output* out))
                  (let ((bindings* (if bindings
                                       (loop for (sym . val) in bindings
                                             do (setf (symbol-value sym) val))
                                       nil)))
                    (setf result (eval form))))
                (setf printed-output (get-output-stream-string out))))
          (error (c)
            (setf error-data (list :type (type-of c)
                                   :message (error-message-string c)
                                   :restarts (mapcar #'princ-to-string
                                                     (compute-restarts c))))))
        (result->plist (list :value (safe-princ-to-string result))
                       :error error-data
                       :output printed-output)))))



;;Describe a symbol
(defun symbol-info (symbol &optional (pkg (find-package "COMMONLISPMCP")))
  "Return a plist with useful information about SYMBOL (a symbol object or a
   symbol name string).  PKG defaults to the current *default* package.
   The result is ready to be turned into JSON, EDN, etc."
  ;; 1) make sure we have a symbol object
  (let ((sym (cond ((symbolp symbol) symbol)
                   ((stringp symbol) (intern symbol pkg))
                   (t (error "SYMBOL‑INFO: ~S is neither a symbol nor a string."
                             symbol)))))
    (multiple-value-bind (found‑sym status) (find-symbol (symbol-name sym) (symbol-package sym))
      (declare (ignore status))   ; we already have the symbol, ignore the status
      (let ((type (cond ((fboundp found‑sym)   :function)
                        ((macro-function found‑sym) :macro)
                        ((boundp found‑sym)   :variable)
                        (t                    :unknown))))
        (list :name          (symbol-name found‑sym)
              :package       (package-name (symbol-package found‑sym))
              :type          type
              :value         (when (eq type :variable)
                               (when (boundp found‑sym)
                                 (symbol-value found‑sym)))
              :function      (when (member type '(:function :macro))
                               (symbol-function found‑sym))
              :documentation (documentation found‑sym
                                            (if (member type '(:function :macro))
                                                'function
                                                'variable))
              :plist         (symbol-plist found‑sym)
              :describe      (describe-to-string found‑sym))))))




;;maybe for server
(defun handle-eval-request (source-string
			    &key (allowed-packages '("COMMON-LISP"))
			      (bindings nil))
  "Entry‑point used by the network layer.
   *source-string* – the raw text the client sent.
   Returns a JSON string (or any other wire format you prefer)."
  (let ((form (read-form-from-string source-string)))
    (let ((result (eval-form form allowed-packages bindings)))
      ;; ----> Choose your serializer here <----
      ;; 1) JSON (using cl-json)
      (json:encode-json-to-string result)
      ;; 2) EDN (using cl-edn)
      ;; (edn:to-string result)
      )))


(defun result->plist (values &key (error nil) (output nil))
  "Convenient way to turn a result into a plist that can be
   turned into JSON/EDN later."
  (list :values values
        :error  (or error nil)   ; NIL → no error
        :output output))
