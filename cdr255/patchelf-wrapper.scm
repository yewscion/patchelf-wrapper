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
  "Run the patchelf program on the TARGET binary.

This is an ACTION.

Arguments
=========
TARGET<string>: The name of the file we are going to patch; a compiled ELF file.
INTERPRETER<string>: The actual interpreter library file to use from glibc.
RPATH-SPECS<<list> of <lists> of <strings>>: The packages being used from
                                             GNU/Guix to path the runpath of the
                                             TARGET, in pairs like
                                             '(\"name\" \"output\")

Returns
=======
<undefined>

Impurities
==========
Makes Changes to a File, Builds Packages using Guix."
  (patch-interpreter interpreter target)
  (patch-rpath (build-rpath #:rpath-specs rpath-specs)
               target))

(define (build-guix-build-command-pipe package)
  "Create the command needed to build PACKAGE with GNU/Guix.

This is a CALCULATION.

Arguments
=========
PACKAGE<string>: The name of the package to build.

Returns
=======
A <<list> of <strings>> containing each part of the command to run.

Impurities
==========
None."
  (display "[build-guix]  package:  ")
  (display package)
  (newline)
  (list "guix" "build" package))

(define (build-grep-command-pipe output)
  "Create the command needed to filter packages outputs so only OUTPUT remains.

This is a CALCULATION.

Arguments
=========
OUTPUT<string>: A string representing which output we are looking for.

Returns
=======
A <<list> of <strings>> containing each part of the command to run.

Impurities
==========
None."
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
  "Build the actual command pipe to run on the commandline to build PACKAGE and
isolate the correct OUTPUT.

This is a CALCULATION.

Arguments
=========
PACKAGE<string>: The name of the package to build.
OUTPUT<string>: A string representing which output we are looking for.

Returns
=======
A <<list> of <lists> of <strings>> that represent the commands to run in a long
pipe to return the store location of the OUTPUT of PACKAGE after it's built.

Impurities
==========
None."
  (list (build-guix-build-command-pipe package)
        (build-grep-command-pipe output)))

(define (run-command-pipe commands)
  "Run the piped COMMANDS, capturing the first line of their final result.

This is an ACTION.

Arguments
=========
COMMANDS<<list> of <lists> of <strings>>: Each member of the list is a list of
                                          strings specifying a command to be
                                          run, with the entire list being piped
                                          to one another in sequence.

Returns
=======
A <string> that is the first line of output from the piped COMMANDS.

Impurities
==========
Runs arbitrary commands on system."
  (receive (from to pids)
      (pipeline commands)
    (let ((result (read-line from)))
      (close from)
      (close to)
      result)))
  
(define* (rpath-item #:key
                     (rpath-spec '("gcc" "lib")))
  "Run the commands necessary to build the output specified by RPATH-SPEC using
GNU/Guix, capturing the resulting location in the store.

This is an ACTION.

Arguments
=========
RPATH-SPEC<<list> of <strings>>: A pair of strings like
                                 '(\"package\" \"output\"), specifying the
                                 package and output to build with GNU/Guix.

Returns
=======
A <string> representing the path of the package in the store.

Impurities
==========
Builds software using GNU/Guix."
  (display "[rpath-item]  rpath-spec:  ")
  (display rpath-spec)
  (newline)
  (string-append (run-command-pipe (build-command-pipe (car rpath-spec)
                                                       (cadr rpath-spec)))
                 "/lib"))

(define* (build-rpath #:key (rpath-specs '(("gcc" "lib"))))
  "Assemble the total runpath to apply to the target binary.

This is an ACTION.

Arguments
=========
RPATH-SPECS<<list> of <lists> of <strings>>: The packages being used from
                                             GNU/Guix to path the runpath of the
                                             TARGET, in pairs like
                                             '(\"name\" \"output\")

Returns
=======
A <string> that specifies the final rpath for patchelf to use.

Impurities
==========
Builds packages on system using GNU/Guix."
  (display "[build-rpath] rpath-specs: ")
  (display rpath-specs)
  (newline)
  (cond ((eq? (length rpath-specs) 1)
         (list (rpath-item #:rpath-spec (car rpath-specs))))
        (else
         (append (list (rpath-item #:rpath-spec (car rpath-specs)))
               (build-rpath #:rpath-specs (cdr rpath-specs))))))

(define (patch-rpath built-rpath target)
  "Call patchelf to actually apply the BUILT-RPATH to the TARGET binary.

This is an ACTION.

Arguments
=========
BUILT-RPATH<string>: A string of the form '/path/to/lib:/other/path/to/lib' that
                     specifies where a binary should look for shared libraries.
TARGET<string>: The name of the file we are going to patch; a compiled ELF file.

Returns
=======
A <number> representing the return status of the patchelf call.

Impurities
==========
Runs the patchelf command on a binary."
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

(define (patch-interpreter interpreter target)
  "Call patchelf to actually apply the INTERPRETER to the TARGET binary.

This is an ACTION.

Arguments
=========
TARGET<string>: The name of the file we are going to patch; a compiled ELF file.
INTERPRETER<string>: The actual interpreter library file to use from glibc.

Returns
=======
A <number> representing the return status of the patchelf call.

Impurities
==========
Runs the patchelf command on a binary."

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
  "Interpret the commandline's --rpath-specs argument.

This is a CALCULATION.

Arguments
=========
RPATH-SPECS<string>: A string in the form 'package:output package:output' that
                     specifies all of the needed libraries for the runpath of
                     a binary.
Returns
=======

A <<list> of <lists> of <strings>> representing the packages to build with
GNU/Guix to patch the runpath of a binary, in pairs like '(\"name\" \"output\").

Impurities
==========
None."
  (map (lambda (y)
         (string-split
          y
          (lambda (z)
            (memq z '(#\:)))))
       (string-split
        rpath-specs
        (lambda (x)
          (memq x '(#\ ))))))
