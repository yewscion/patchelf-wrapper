(use-modules
 ;;; These are my commonly needed modules; remove unneeded ones.
 (guix packages)
 ((guix licenses) #:prefix license:)
 (guix download)
 (guix build-system gnu)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages guile)
 (guix gexp))

(package
  (name "patchelf-wrapper")
  (version "0.0.1")
  (source (local-file (string-append "./"
                                     name
                                     "-"
                                     version
                                     ".tar.bz2")))
  (build-system gnu-build-system)
  (arguments
   `(#:tests? #f
     #:phases
     (modify-phases
      %standard-phases
      ;; This allows the paths for guile and java to be embedded in the scripts
      ;; in bin/
      (add-before
       'patch-usr-bin-file 'remove-script-env-flags
       (lambda* (#:key inputs #:allow-other-keys)
         (substitute*
          (find-files "./bin")
          (("#!/usr/bin/env -S guile \\\\\\\\")
           "#!/usr/bin/env guile \\")
          (("\"java")
           (string-append "\"" (search-input-file inputs "/bin/java"))))))
      ;; Java and Guile programs don't need to be stripped.
      (delete 'strip))))
  (native-inputs (list autoconf automake pkg-config texinfo))
  (inputs (list guile-3.0-latest))
  (synopsis "A tool to use patchelf with GNU/Guix")
  (description
   (string-append
    "A script and library based around the idea of making it easier to patch "
    "precompiled binaries to work with GNU/Guix."))
  (home-page
   "https://github.com/yewscion/patchelf-wrapper")
  (license license:agpl3+))
