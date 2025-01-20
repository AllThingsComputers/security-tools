;; Load and compile the Lisp file
;; (load (compile-file "C:/Users/User.user/source/repos/59 Lisp Emacs File Finder/clips.lisp"))

;; Define a function to find files with a specific pattern
(defun find-files (directory pattern)
  "Search for files in DIRECTORY matching the PATTERN."
  (let ((files (directory (merge-pathnames (make-pathname :name "**" :type pattern) directory))))
    (if files
        (progn
          (format t "Files matching ~A in ~A:~%" pattern directory)
          (dolist (file files)
            (format t "~A~%" file)))
        (format t "No files found matching ~A in ~A.~%" pattern directory))
    files))

;; Example of finding audio files in a specific directory
(defun find-audio-files (directory)
  "Search for common audio file formats in DIRECTORY."
  (dolist (extension '("*.mp3" "*.wav" "*.flac" "*.aac" "*.m4a" "*.mp4" "*.wma"))
    (find-files directory extension)))

;; Interactive function to test file finding
(defun interactive-file-search ()
  "Prompt user for a directory and a file pattern, and perform the search."
  (let* ((dir (read-line "Enter the directory path: "))
         (pattern (read-line "Enter the file pattern (e.g., *.mp3): ")))
    (find-files dir pattern)))

;; Main function to demonstrate usage
(defun main ()
  "Main entry point for the file finder utility."
  (write-line "Welcome to the File Finder Utility!")
  (interactive-file-search))

;; Test case
(main)


60 Clojure(Script) Stack Overflow (in intellij at first the vsc)
(ns hello-world.core)

;; Print a welcome message
(println "Hello world!")

;; Function to multiply the sum of two numbers by 2.0
(defn multiply [a b]
  (* (+ a b) 2.0))

;; Function to demonstrate a `while` loop with an `atom`
(defn example []
  (let [x (atom 1)] ;; Define an atom with an initial value of 1
    (while (< @x 9999) ;; Loop while the value of `x` is less than 9999
      (do
        (println @x) ;; Print the current value of `x`
        (swap! x inc))))) ;; Increment the value of `x`

;; Call the example function
(example)
