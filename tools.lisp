
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


;;Describe symbol
(defun find-symbol-anywhere (name)
  "Finds a symbol by name, handling package-qualified names."
  (let* ((double-colon-pos (search "::" name))
         (single-colon-pos (and (not double-colon-pos) (search ":" name))))
    (cond
      (double-colon-pos
       (let* ((pkg-name (subseq name 0 double-colon-pos))
              (sym-name (subseq name (+ double-colon-pos 2)))
              (pkg (find-package pkg-name)))
         (multiple-value-bind (sym foundp) (find-symbol sym-name pkg)
           (when foundp sym))))
      (single-colon-pos
       (let* ((pkg-name (subseq name 0 single-colon-pos))
              (sym-name (subseq name (1+ single-colon-pos)))
              (pkg (find-package pkg-name)))
         (multiple-value-bind (sym foundp) (find-symbol sym-name pkg)
           (when foundp sym))))
      (t
       ;; No package prefix, search all packages
       (let ((name (string-upcase name)))
         (loop for pkg in (list-all-packages)
               for (sym foundp) = (multiple-value-list (find-symbol name pkg))
               when foundp return sym))))))

(defun describe-symbol-anywhere (name)
  (let ((sym (find-symbol-anywhere name)))
    (if sym
        (describe-to-string sym)
        (format nil "Symbol ~A not found in any package." name))))

;; To test
(describe-symbol-anywhere "CL:CAR")
(describe-symbol-anywhere "safe-princ-to-string")

























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
