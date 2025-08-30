;;;; commonlispmcp.asd

(asdf:defsystem #:commonlispmcp
  :description "An MCP client built for common lisp development. Read symbols from the slynk server, evaluate Common Lisp code, and refactor projects with MCP."
  :author "nullbath <nullbath@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:40ants-mcp #:openrpc-server)
  :components ((:file "package")
               (:file "tools")
	       (:file "commonlispmcp")))
