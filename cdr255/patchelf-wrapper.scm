(define-module (cdr255 patchelf-wrapper)
  #:version (0 0 1)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:export (run-patchelf
            process-rpath-specs))

(define* (run-patchelf target 
                       #:key
                       (interpreter "/lib/ld-linux-x86-64.so.2")
                       (rpath-specs '(("gcc" "lib"))))
  (patch-interpreter target interpreter)
  (patch-rpath (build-rpath #:rpath-specs rpath-specs)
               target))

(define (build-guix-build-command-pipe package)
  (display "[build-guix]  package:  ")
  (display package)
  (newline)
  (list "guix" "build" package))

(define (build-grep-command-pipe output)
  (display "[build-grep]  output:  ")
  (display output)
  (newline)

  (cond ((string= output "out")
         (display "Inverse Grep Engaged\n")
         (list "grep" "-v" "'debug\ndebug\nstatic\nsource\ndoc\nbin\n'"))
        (else
         (display "Normal Grep Engaged\n")
         (list "grep" output))))

(define (build-command-pipe package output)
  (list (build-guix-build-command-pipe package)
        (build-grep-command-pipe output)))

(define (run-command-pipe commands)
  (receive (from to pids)
      (pipeline commands)
    (let ((result (read-line from)))
      (close from)
      (close to)
      result)))
  
(define* (rpath-item #:key
                    (rpath-spec '("gcc" "lib")))
  (display "[rpath-item]  rpath-spec:  ")
  (display rpath-spec)
  (newline)
  (string-append (run-command-pipe (build-command-pipe (car rpath-spec)
                                                       (cadr rpath-spec)))
                 "/lib"))


(define* (build-rpath #:key (rpath-specs '(("gcc" "lib"))))
  (display "[build-rpath] rpath-specs: ")
  (display rpath-specs)
  (newline)
  (cond ((eq? (length rpath-specs) 1)
         (list (rpath-item #:rpath-spec (car rpath-specs))))
        (else
         (append (list (rpath-item #:rpath-spec (car rpath-specs)))
               (build-rpath #:rpath-specs (cdr rpath-specs))))))

(define (patch-rpath built-rpath target)
  (let* ((rpath (string-join built-rpath
                            ":"))
         (command (string-append
                   "patchelf --set-rpath "
                   rpath
                   " "
                   target)))
    (display (string-append "Patching rpath using '"
                            command
                            "'…\n\n"))
    (system command)))

(define (patch-interpreter target interpreter)
  (let* ((commands `(,(build-guix-build-command-pipe "glibc")
                     ,(build-grep-command-pipe "out")))
         (result (run-command-pipe commands)))
    (display (string-append "Patching Interpreter as "
                            result
                            interpreter
                            "…\n"))
    (system (string-append
             "patchelf --set-interpreter "
             result
             interpreter
             " "
             target))))

(define (process-rpath-specs rpath-specs)
  (map (lambda (y)
         (string-split
          y
          (lambda (z)
            (memq z '(#\:)))))
       (string-split
        rpath-specs
        (lambda (x)
          (memq x '(#\ ))))))
