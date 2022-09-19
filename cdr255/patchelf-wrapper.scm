(define-module (cdr255 patchelf-wrapper)
  #:version (0 0 1)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:export (run-patchelf
            process-rpath-specs))
(define %grep-non-out
  "'debug\ndebug\nstatic\nsource\nidle\ndoc\nbin\n'")
(define* (run-patchelf target 
                       #:key
                       (interpreter "/lib/ld-linux-x86-64.so.2")
                       (rpath-specs '(("gcc" "lib")))
                       (verbosity #false))
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
VERBOSITY<boolean>: Whether to print explicit actions to standard output during
                    the process.

Returns
=======
<undefined>

Impurities
==========
Makes Changes to a File, Builds Packages using Guix."
  (patch-interpreter interpreter
                     target
                     #:verbosity
                     verbosity)
  (patch-rpath (build-rpath #:rpath-specs rpath-specs #:verbosity verbosity)
               target
               #:verbosity
               verbosity))

(define* (build-guix-build-command-pipe package #:key
                                       (verbosity #false))
  "Create the command needed to build PACKAGE with GNU/Guix.

This is a CALCULATION.

Arguments
=========
PACKAGE<string>: The name of the package to build.
VERBOSITY<boolean>: Whether to print explicit actions to standard output during
                    the process.

Returns
=======
A <<list> of <strings>> containing each part of the command to run.

Impurities
==========
None."
  (when verbosity
    (display "[build-guix]  package:  ")
    (display package)
    (newline))
  (list "guix" "build" package))

(define* (build-grep-command-pipe output #:key
                                 (verbosity #false))
  "Create the command needed to filter packages outputs so only OUTPUT remains.

This is a CALCULATION.

Arguments
=========
OUTPUT<string>: A string representing which output we are looking for.
VERBOSITY<boolean>: Whether to print explicit actions to standard output during
                    the process.

Returns
=======
A <<list> of <strings>> containing each part of the command to run.

Impurities
==========
None."
  (when verbosity
    (display "[build-grep]  output:  ")
    (display output)
    (newline))
  (cond ((string= output "out")
         (when verbosity
           (display "[build-grep]  Inverse Grep Engaged\n"))
         (list "grep" "-v" %grep-non-out))
        (else
         (when verbosity
           (display "[build-grep]  Normal Grep Engaged\n"))
         (list "grep" output))))

(define* (build-command-pipe package output #:key
                            (verbosity #false))
  "Build the actual command pipe to run on the commandline to build PACKAGE and
isolate the correct OUTPUT.

This is a CALCULATION.

Arguments
=========
PACKAGE<string>: The name of the package to build.
OUTPUT<string>: A string representing which output we are looking for.
VERBOSITY<boolean>: Whether to print explicit actions to standard output during
                    the process.

Returns
=======
A <<list> of <lists> of <strings>> that represent the commands to run in a long
pipe to return the store location of the OUTPUT of PACKAGE after it's built.

Impurities
==========
None."
  (when verbosity
    (display "[build-cpip]  package:  ")
    (display package)
    (newline)
    (display "[build-cpip]  output:  ")
    (display output)
    (newline))
  (list (build-guix-build-command-pipe package #:verbosity verbosity)
        (build-grep-command-pipe output #:verbosity verbosity)))

(define* (run-command-pipe commands #:key
                          (verbosity #false))
  "Run the piped COMMANDS, capturing the first line of their final result.

This is an ACTION.

Arguments
=========
COMMANDS<<list> of <lists> of <strings>>: Each member of the list is a list of
                                          strings specifying a command to be
                                          run, with the entire list being piped
                                          to one another in sequence.
VERBOSITY<boolean>: Whether to print explicit actions to standard output during
                    the process.

Returns
=======
A <string> that is the first line of output from the piped COMMANDS.

Impurities
==========
Runs arbitrary commands on system."
  (when verbosity
    (display "[run-c-pipe]  commands:  ")
    (display (map (lambda (x)
                    (map (lambda (y)
                           (string-replace-substring y
                                                     %grep-non-out
                                                     "%grep-non-out"))
                         x))
                  commands))
    (newline))
  (receive (from to pids)
      (pipeline commands)
    (let ((result (read-line from)))
      (close from)
      (close to)
      (when verbosity
        (display "[run-c-pipe]  result:  ")
        (display result)
        (newline))
      result)))
  
(define* (rpath-item #:key
                     (rpath-spec '("gcc" "lib"))
                     (verbosity #false))
  "Run the commands necessary to build the output specified by RPATH-SPEC using
GNU/Guix, capturing the resulting location in the store.

This is an ACTION.

Arguments
=========
RPATH-SPEC<<list> of <strings>>: A pair of strings like
                                 '(\"package\" \"output\"), specifying the
                                 package and output to build with GNU/Guix.
VERBOSITY<boolean>: Whether to print explicit actions to standard output during
                    the process.

Returns
=======
A <string> representing the path of the package in the store.

Impurities
==========
Builds software using GNU/Guix."
  (when verbosity
    (display "[rpath-item]  rpath-spec:  ")
    (display rpath-spec)
    (newline))
  (let ((result (string-append
                 (run-command-pipe
                  (build-command-pipe (car rpath-spec)
                                      (cadr rpath-spec)
                                      #:verbosity verbosity)
                  #:verbosity verbosity)
                 "/lib")))
    (when verbosity
      (display "[rpath-item]  result:  ")
      (display result)
      (newline))
    result))

(define* (build-rpath #:key (rpath-specs '(("gcc" "lib")))
                      (verbosity #false))
  "Assemble the total runpath to apply to the target binary.

This is an ACTION.

Arguments
=========
RPATH-SPECS<<list> of <lists> of <strings>>: The packages being used from
                                             GNU/Guix to path the runpath of the
                                             TARGET, in pairs like
                                             '(\"name\" \"output\")
VERBOSITY<boolean>: Whether to print explicit actions to standard output during
                    the process.

Returns
=======
A <string> that specifies the final rpath for patchelf to use.

Impurities
==========
Builds packages on system using GNU/Guix."
  (when verbosity
    (display "[build-rpth]  rpath-specs: ")
    (display rpath-specs)
    (newline))
  (cond ((eq? (length rpath-specs) 1)
         (list (rpath-item #:rpath-spec (car rpath-specs) #:verbosity verbosity)))
        (else
         (append (list (rpath-item #:rpath-spec (car rpath-specs) #:verbosity verbosity))
               (build-rpath #:rpath-specs (cdr rpath-specs) #:verbosity verbosity)))))

(define* (patch-rpath built-rpath target #:key
                     (verbosity #false))
  "Call patchelf to actually apply the BUILT-RPATH to the TARGET binary.

This is an ACTION.

Arguments
=========
BUILT-RPATH<string>: A string of the form '/path/to/lib:/other/path/to/lib' that
                     specifies where a binary should look for shared libraries.
TARGET<string>: The name of the file we are going to patch; a compiled ELF file.
VERBOSITY<boolean>: Whether to print explicit actions to standard output during
                    the process.

Returns
=======
A <number> representing the return status of the patchelf call.

Impurities
==========
Runs the patchelf command on a binary."
  (when verbosity
    (display "[patch-rpth]  built-rpath: ")
    (display built-rpath)
    (newline)
    (display "[patch-rpth]  target: ")
    (display target)
    (newline))
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

(define* (patch-interpreter interpreter target #:key
                            (verbosity #false))
  "Call patchelf to actually apply the INTERPRETER to the TARGET binary.

This is an ACTION.

Arguments
=========
TARGET<string>: The name of the file we are going to patch; a compiled ELF file.
INTERPRETER<string>: The actual interpreter library file to use from glibc.
VERBOSITY<boolean>: Whether to print explicit actions to standard output during
                    the process.

Returns
=======
A <number> representing the return status of the patchelf call.

Impurities
==========
Runs the patchelf command on a binary."
  (when verbosity
    (display "[patch-intr]  interpreter:  ")
    (display interpreter)
    (newline)
    (display "[patch-intr]       target:  ")
    (display target)
    (newline))
  (let* ((commands `(,(build-guix-build-command-pipe "glibc" #:verbosity verbosity)
                     ,(build-grep-command-pipe "out" #:verbosity verbosity)))
         (result (run-command-pipe commands #:verbosity verbosity)))
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

(define* (process-rpath-specs rpath-specs #:key
                             (verbosity #false))
  "Interpret the commandline's --rpath-specs argument.

This is a CALCULATION.

Arguments
=========
RPATH-SPECS<string>: A string in the form 'package:output package:output' that
                     specifies all of the needed libraries for the runpath of
                     a binary.
VERBOSITY<boolean>: Whether to print explicit actions to standard output during
                    the process.

Returns
=======

A <<list> of <lists> of <strings>> representing the packages to build with
GNU/Guix to patch the runpath of a binary, in pairs like '(\"name\" \"output\").

Impurities
==========
None."
  (when verbosity
    (display "[proc-rpath]  rpath-specs:  ")
    (display rpath-specs)
    (newline))
  (map (lambda (y)
         (string-split
          y
          (lambda (z)
            (memq z '(#\:)))))
       (string-split
        rpath-specs
        (lambda (x)
          (memq x '(#\ ))))))
