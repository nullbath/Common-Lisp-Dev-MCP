;;;; commonlispmcp.lisp
(in-package #:commonlispmcp)
(ql:quickload :cl-json)
(ql:quickload :openrpc-server)
(ql:quickload :40ants-mcp)

(defpackage #:commonlispmcp
  (:use #:cl)
  (:import-from #:40ants-mcp/content/text
		#:text-content)
  (:import-from #:openrpc-server))

(in-package #:commonlispmcp)


;; Define your API
(openrpc-server:define-api (my-tools :title "My Custom Tools"))

;; Define a tool that adds two numbers
(40ants-mcp/tools:define-tool (my-tools add) (a b)
  (:summary "Adds two numbers and returns the result.")
  (:param a integer "First number to add.")
  (:param b integer "Second number to add.")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
		       :text (format nil "The sum of ~A and ~A is: ~A"
				     a b (+ a b)))))








;; Start the server
(40ants-mcp/server/definition:start-server my-tools)
