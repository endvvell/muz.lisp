(in-package #:cl)

;; environment setup:
(setf (readtable-case *readtable*) :invert)

(setf *print-case* :downcase)

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~%~}~^ ~}}"
    (loop :for key :being :the :hash-keys :of object
          :using (hash-value value)
          :collect (list key value))))

;; muz package definition:
(defpackage #:muz
  (:use #:cl
        #:str
        #:uiop
        #:colors)
  (:shadowing-import-from #:uiop/utility "EMPTYP")
  (:local-nicknames (#:cli #:clingon)
                    (#:alex #:alexandria)
                    (#:ser #:serapeum)
                    (#:rex #:cl-ppcre))
  (:export #:main))

(in-package #:muz)

(ser:defalias s= #'string=)


;; primitive data:
(defvar *alphabet* '("A" "B" "C" "D" "E" "F" "G"))


;; https://www.compart.com/en/unicode/block/U+1D100
(defvar *unicode-notation* (ser:dict
                             :natural (string (code-char 9838)) ;; #\U+0266E == #\MUSIC_NATURAL_SIGN
                             :sharp (string (code-char 9839)) ;; #\U+0266F == #\MUSIC_SHARP_SIGN
                             :flat (string (code-char 9837)) ;; #\U+0266D == #\MUSIC_FLAT_SIGN
                             :dsharp (string (code-char 119082)) ;; #\U+01D12A == #\MUSICAL_SYMBOL_DOUBLE_SHARP
                             :dflat (string (code-char 119083)) ;; #\U+01D12B == #\MUSICAL_SYMBOL_DOUBLE_FLAT
                             :aug (string (code-char 43))
                             :dim (string (code-char 176))
                             :maj (string (code-char 916))
                             :min "m"))

(defvar *simple-notation* (ser:dict
                            :natural ""
                            :sharp "#"
                            :flat "b"
                            :dsharp "x"
                            :dflat "bb"
                            :aug "+"
                            :dim "o"
                            :maj "M"
                            :min "m"))


;; this will store either `*unicode-notation*` or `*simple-notation*`
;; depending on whether or not the `-b` flag is passed to the `muz` command
(defvar *accidental-notation*)

(defvar *initialized-octave-obj*)

;; TODO: implement intervals
; (defvar *intervals*)

; (defclass interval ()
;     ((full-name
;        :type string
;        :reader full-name
;        :initarg :full-name)
;      (short-name
;        :type string
;        :reader short-name
;        :initarg :short-name)
;      (alt-name
;        :type string
;        :reader alt-name
;        :initarg :alt-name)
;      (roman
;        :type string
;        :reader roman
;        :initarg :roman)
;      (h-steps-from-root
;        :type integer
;        :reader h-steps-from-root
;        :initarg :h-steps-from-root)
;      (impression
;        :type string
;        :reader impression
;        :initarg :impression)
;      (quality-of-impression
;        :type string
;        :reader quality-of-impression
;        :initarg :quality-of-impression)))


;; TODO: in the future, might want to just put this into `*intervals*`
; (defun init-intervals ()
;   (list
;    (make-instance 'interval
;      :full-name "Unison"
;      :short-name "P1"
;      :alt-name "Root"
;      :roman (format nil "~@R~%" 1)
;      :h-steps-from-root 0
;      :impression "Consonance"
;      :quality-of-impression "Open")
;    (make-instance 'interval
;      :full-name "Minor 2nd"
;      :short-name "m2"
;      :alt-name ""
;      :roman (format nil "~@R~%" 2)
;      :h-steps-from-root 1
;      :impression "Dissonance"
;      :quality-of-impression "Sharp")
;    (make-instance 'interval
;      :full-name "Major 2nd"
;      :short-name "M2"
;      :alt-name ""
;      :roman (format nil "~@R~%" 3)
;      :h-steps-from-root 2
;      :impression "Dissonance"
;      :quality-of-impression "Mild")
;    (make-instance 'interval
;      :full-name "Minor 3rd"
;      :short-name "m3"
;      :alt-name ""
;      :roman (format nil "~@R~%" 4)
;      :h-steps-from-root 3
;      :impression "Consonance"
;      :quality-of-impression "Soft")
;    (make-instance 'interval
;      :full-name "Major 3rd"
;      :short-name "M3"
;      :alt-name ""
;      :roman (format nil "~@R~%" 5)
;      :h-steps-from-root 4
;      :impression "Consonance"
;      :quality-of-impression "Soft")
;    (make-instance 'interval
;      :full-name "Perfect 4th"
;      :short-name "P4"
;      :alt-name ""
;      :roman (format nil "~@R~%" 6)
;      :h-steps-from-root 5
;      :impression "Consonance"
;      :quality-of-impression "Ambiguous")
;    (make-instance 'interval
;      :full-name "Tritone"
;      :short-name "TT"
;      :alt-name ""
;      :roman (format nil "~@R~%" 7)
;      :h-steps-from-root 6
;      :impression "Dissonance"
;      :quality-of-impression "Sharp")
;    (make-instance 'interval
;      :full-name "Perfect 5th"
;      :short-name "P5"
;      :alt-name ""
;      :roman (format nil "~@R~%" 8)
;      :h-steps-from-root 7
;      :impression "Consonance"
;      :quality-of-impression "Open")
;    (make-instance 'interval
;      :full-name "Minor 6th"
;      :short-name "m6"
;      :alt-name ""
;      :roman (format nil "~@R~%" 9)
;      :h-steps-from-root 8
;      :impression "Consonance"
;      :quality-of-impression "Soft")
;    (make-instance 'interval
;      :full-name "Major 6th"
;      :short-name "M6"
;      :alt-name ""
;      :roman (format nil "~@R~%" 10)
;      :h-steps-from-root 9
;      :impression "Consonance"
;      :quality-of-impression "Soft")
;    (make-instance 'interval
;      :full-name "Minor 7th"
;      :short-name "m7"
;      :alt-name ""
;      :roman (format nil "~@R~%" 11)
;      :h-steps-from-root 10
;      :impression "Dissonance"
;      :quality-of-impression "Mild")
;    (make-instance 'interval
;      :full-name "Major 7th"
;      :short-name "M7"
;      :alt-name ""
;      :roman (format nil "~@R~%" 12)
;      :h-steps-from-root 11
;      :impression "Dissonance"
;      :quality-of-impression "Sharp")
;    (make-instance 'interval
;      :full-name "Octave"
;      :short-name "P8"
;      :alt-name ""
;      :roman (format nil "~@R~%" 13)
;      :h-steps-from-root 12
;      :impression "Consonance"
;      :quality-of-impression "Open")))


(defclass game ()
    ((levels
      :type hash-table
      :reader get-levels
      :initarg :levels)
     (gameplay-types
      :type hash-table
      :reader get-gameplay-types
      :initarg :gameplay-types)
     (user-level-choice
      :type list
      :reader get-user-level-choice
      :writer set-user-level-choice
      :initarg :user-level-choice)
     (user-gameplay-choice
      :type string
      :reader get-user-gameplay-choice
      :writer set-user-gameplay-choice
      :initarg :user-gameplay-choice)))


;; TODO: add comments to each function
(defmacro ask-user-input (&optional (prompt ""))
  `(declare (type (or string function nil) ,prompt))
  `(progn
    (format *query-io* (or ,prompt ""))
    (finish-output *query-io*)
    (handler-case
        (read-line *query-io*)
      (end-of-file (e)
                   (declare (ignore e))
                   (format t "~%Goodbye~%")
                   (uiop:quit 0))
      (sb-sys:interactive-interrupt (e)
                                    (declare (ignore e))
                                    (format t "~%Goodbye~%")
                                    (uiop:quit 0))
      (error (e)
        (format t "~%Unexpected error: ~A~%" e)
        (uiop:quit 1)))))


(defun extract-note-sets (user-specified-levels game-levels)
  (declare (type list user-specified-levels)
           (type hash-table game-levels))
  (the list ;; TODO: add type (and the custom ones) specifiers everywhere
       (remove-duplicates
           (loop :initially (format t "~%~v@{~A~:*~}~%" 50 "-")
               :for k :in user-specified-levels
               :for found-level = (gethash k game-levels)
                 :when found-level
               :do (format t "~%* ~A~%" (gethash :level-name found-level))
                 :and
                 :append (gethash :notes-set found-level)
               :finally (format t "~%Let's begin:~%")))))


(defmethod ask-level-options ((game-obj game))
  (loop :with found-valid-options = '()
        :with game-levels = (get-levels game-obj)
        :with primary-level-rex = (rex:create-scanner "\\d+")
        :with alt-level-rex = (rex:create-scanner "\\d+[a-zA-Z]+")
        :with level-options-fmt-string = (str:concat "~%Choose levels (can be multiple):~%"
                                           (str:join "~&"
                                                     (loop :for k :being :the :hash-key :of game-levels
                                                           :using (hash-value v)
                                                           :collect (if (rex:scan alt-level-rex k)
                                                                        (format nil "~5@A - ~A" k (gethash :level-name v))
                                                                        (str:concat k " - " (gethash :level-name v)))))
                                           "~%~%Levels: ")
          ;; TODO: I feel like instead of collecting the option specifiers and then retrieving the note sets in `cond`,
          ;; it is instead possible to just retrieve those sets in the loop's body and in the `cond` simply check if any where found and return those
        :for user-choice = (str:trim (ask-user-input level-options-fmt-string))
          ;; checking if any "alternative" options (such as "3a" or "4b") are specified in the user's input:
          :when (rex:scan alt-level-rex user-choice) :do (setf found-valid-options
                                                           (append found-valid-options (rex:all-matches-as-strings alt-level-rex user-choice)))
          ;; checking if there are any distinct "primary" options specified in the user's input:
          :when (rex:scan primary-level-rex user-choice) :do (setf found-valid-options
                                                               (append found-valid-options
                                                                 (remove-if #'(lambda (prim)
                                                                                (member prim found-valid-options
                                                                                        :test #'(lambda (prim alt)
                                                                                                  ;; TODO: this still doesn't match as I would like: try 3a123b => (3a 123b) instead of (3a 1 2 3b)
                                                                                                  (str:contains? prim alt))))
                                                                   (rex:all-matches-as-strings primary-level-rex user-choice))))
        :do
          (cond
           ;; if the valid options are found
           ((some #'(lambda (option-key)
                      (gethash option-key game-levels))
              found-valid-options)
             ;; then:
             (return (set-user-level-choice (extract-note-sets found-valid-options game-levels)
                                            game-obj)))
           ;; if:
           ((member user-choice '("exit" "quit" "q") :test #'s=)
             ;; then:
             (uiop:quit 0))
           ;; else:
           (t (progn
               (format t "~% * Please enter a valid option~%")
               ;; clearing the found options for the next iteration:
               (setq found-valid-options nil))))))


(defmethod ask-gameplay-options ((game-obj game))
  (loop :with game-gameplay-types = (get-gameplay-types game-obj)
        :with valid-options = (alex:hash-table-keys game-gameplay-types)
        :with gameplay-options-fmt-string = (str:concat "~%Choose exercise:~%"
                                              (str:join "~&"
                                                        (loop :for k :being :the :hash-key :of game-gameplay-types
                                                              :using (hash-value v)
                                                              :collect (str:concat k " - " v)))
                                              "~%~%Exercise: ")
        :for user-choice = (str:trim (ask-user-input gameplay-options-fmt-string))
        :do
          (cond
           ((member user-choice valid-options :test #'s=) (return (set-user-gameplay-choice user-choice game-obj)))
           ((member user-choice '("exit" "quit" "q") :test #'s=) (uiop:quit 0))
           (t (progn
               (format t "~% * Please enter a valid option~%"))))))


;; TODO: these can later be made into a switch function
(defun init-major () ;; TODO: should probably rename this into something that indicates that these function are only responsible for attaching names "major" and "minor" to scales
  (mapcar #'(lambda (each-letter)
              (str:unwords (list each-letter "major")))
    *alphabet*))


(defun init-minor ()
  (mapcar #'(lambda (each-letter)
              (str:unwords (list each-letter "minor")))
    *alphabet*))


(defun init-min-maj-sharp-flat (scale-to-use sharp-or-flat-keyword)
  (let ((accidental-to-use (gethash sharp-or-flat-keyword *accidental-notation*)))
    (mapcar #'(lambda (each-note)
                (let* ((split-note-name (str:split " " each-note :limit 2 :omit-nulls t))
                       (each-letter (first split-note-name))
                       (scale-name (second split-note-name)))
                  (str:unwords (list (str:concat each-letter accidental-to-use) scale-name))))
      scale-to-use)))


(defun combine-scales (&rest scales-to-combine-into-one-list)
  (apply #'concatenate 'list scales-to-combine-into-one-list))

(defun init-all-sharps (&rest all-sharp-scales)
  (apply #'combine-scales all-sharp-scales))

(defun init-all-flats (&rest all-flat-scales)
  (apply #'combine-scales all-flat-scales))

(defun init-all-accidentals (&rest all-accidentals)
  (apply #'combine-scales all-accidentals))

(defun init-all-notes (&rest all-scales)
  (apply #'combine-scales all-scales))


(defclass octave ()
    ((major
      :type list
      :reader get-major
      :initarg :major)
     (minor
      :type list
      :reader get-minor
      :initarg :minor)
     (maj-sharps
      :type list
      :reader get-maj-sharps)
     (maj-flats
      :type list
      :reader get-maj-flats)
     (min-sharps
      :type list
      :reader get-min-sharps)
     (min-flats
      :type list
      :reader get-min-flats)
     (all-sharps
      :type list
      :reader get-all-sharps)
     (all-flats
      :type list
      :reader get-all-flats)
     (all-accidentals
      :type list
      :reader get-all-accidentals)
     (all-notes
      :type list
      :reader get-all-notes)))


(defmethod initialize-instance :after ((unfinished-instance octave) &key)
  (let ((major-scale (setf (slot-value unfinished-instance 'major)
                       (init-major)))
        (minor-scale (setf (slot-value unfinished-instance 'minor)
                       (init-minor))))

    ;; maj-sharps
    (setf (slot-value unfinished-instance 'maj-sharps)
      (init-min-maj-sharp-flat major-scale :sharp))

    ;; maj-flats
    (setf (slot-value unfinished-instance 'maj-flats)
      (init-min-maj-sharp-flat major-scale :flat))

    ;; min-sharps
    (setf (slot-value unfinished-instance 'min-sharps)
      (init-min-maj-sharp-flat minor-scale :sharp))

    ;; min-flats
    (setf (slot-value unfinished-instance 'min-flats)
      (init-min-maj-sharp-flat minor-scale :flat))

    ;; all-sharps
    (with-slots (maj-sharps min-sharps) unfinished-instance
      (setf (slot-value unfinished-instance 'all-sharps)
        (init-all-sharps maj-sharps min-sharps)))

    ;; all-flats
    (with-slots (maj-flats min-flats) unfinished-instance
      (setf (slot-value unfinished-instance 'all-flats)
        (init-all-flats maj-flats min-flats)))

    ;; all-accidentals
    (with-slots (maj-sharps maj-flats min-sharps min-flats) unfinished-instance
      (setf (slot-value unfinished-instance 'all-accidentals)
        (init-all-accidentals maj-sharps maj-flats min-sharps min-flats)))

    ;; all-notes
    (with-slots (major minor all-accidentals) unfinished-instance
      (setf (slot-value unfinished-instance 'all-notes)
        (init-all-notes major minor all-accidentals)))))


(defun init-levels (initialized-octave)
  (ser:dict
    "1" (ser:dict
          :level-name "Major scales"
          :notes-set (get-major initialized-octave))
    "2" (ser:dict
          :level-name "Minor scales"
          :notes-set (get-minor initialized-octave))
    "3" (ser:dict
          :level-name (str:concat "All sharp accidental scales (" (gethash :sharp *accidental-notation*) ")")
          :notes-set (get-all-sharps initialized-octave))
    "3a" (ser:dict
           :level-name (str:concat "Only major sharp scales (major " (gethash :sharp *accidental-notation*) ")")
           :notes-set (get-maj-sharps initialized-octave))
    "3b" (ser:dict
           :level-name (str:concat "Only minor sharp scales (minor " (gethash :sharp *accidental-notation*) ")")
           :notes-set (get-min-sharps initialized-octave))
    "4" (ser:dict
          :level-name (str:concat "All flat accidental scales (" (gethash :flat *accidental-notation*) ")")
          :notes-set (get-all-flats initialized-octave))
    "4a" (ser:dict
           :level-name (str:concat "Only major flat scales (major " (gethash :flat *accidental-notation*) ")")
           :notes-set (get-maj-flats initialized-octave))
    "4b" (ser:dict
           :level-name (str:concat "Only minor flat scales (minor " (gethash :flat *accidental-notation*) ")")
           :notes-set (get-min-flats initialized-octave))
    "5" (ser:dict
          :level-name "Everything"
          :notes-set (get-all-notes initialized-octave))))


(defun init-gameplay-types ()
  (ser:dict
    "1" "Find Tone"
    "2" "Write Scales"))


(defun init-game-resources ()
  (let* ((initialized-octave (make-instance 'octave))

         ;; TODO: intervals:
         ;  (initialized-intervals (init-intervals))

         (initialized-game-options (make-instance 'game
                                     :levels (init-levels initialized-octave)
                                     :gameplay-types (init-gameplay-types))))
    ;; TODO: might want to put these into the init functions and just call those functions without binding return values
    (setf *initialized-octave-obj* initialized-octave)

    ;; TODO: intervals:
    ; (setf *intervals* initialized-intervals)

    initialized-game-options))


(defun find-next-letter-alphabet (current-letter half-steps)
  "We prioritize sharps over flats, so passing (E 1) -- i.e. E# to this function
   will return the alphabet of the E scale, not of the F scale."
  ;; TODO: ^^ provide more example in the docstring
  (multiple-value-bind (full-steps remainder) (truncate half-steps 2)
    (let* ((idx-of-letter-in-alphabet (position current-letter *alphabet*
                                        :test #'s=))

           (distance-in-alphabet-to-next-letter full-steps)

           (idx-of-next-letter-relative-to-alphabet-list (+ idx-of-letter-in-alphabet distance-in-alphabet-to-next-letter))

           (preliminary-subseq (cond (;; if the distance to the next letter in the *alphabet* list exceeds the bounds of that list (goes below 0)
                                       (< idx-of-next-letter-relative-to-alphabet-list 0)
                                        (progn
                                         ;; [root-idx, <end-of-list>]
                                         (append
                                           (subseq *alphabet*
                                                   0 idx-of-letter-in-alphabet)
                                           (subseq (reverse *alphabet*)
                                                   0 (abs idx-of-next-letter-relative-to-alphabet-list)))))
                                     ;; else, if the distance to the next letter in the *alphabet* list exceeds the bounds of that list (goes above the length of the list)
                                     ((> idx-of-next-letter-relative-to-alphabet-list (length *alphabet*))
                                       (progn
                                        ;; [root-idx, <end-of-list>]
                                        (append
                                          (subseq *alphabet*
                                                  idx-of-letter-in-alphabet)
                                          (subseq *alphabet*
                                                  0 (1+ (- idx-of-next-letter-relative-to-alphabet-list idx-of-letter-in-alphabet))))))
                                     ;; else:
                                     (t
                                       ;; [root-idx, <end-of-list>]
                                       (subseq *alphabet*
                                               (min idx-of-next-letter-relative-to-alphabet-list
                                                 idx-of-letter-in-alphabet)
                                               (max idx-of-next-letter-relative-to-alphabet-list
                                                 idx-of-letter-in-alphabet)))))

           ;; adjusting for jumps between B -> C and E -> F
           (idx-of-next-letter-relative-to-alphabet-list (if (or (member "E" preliminary-subseq :test #'s=)
                                                                 (member "B" preliminary-subseq :test #'s=))
                                                             ;; here we are adding the half-step to account for an irregular jump to the `remainder`
                                                             ;; and divining the result by 2, and the resulting full-step goes towards producing the correct index of the correct next letter
                                                             (cond (;; accounting for moving forward along the scale
                                                                     (plusp (- idx-of-next-letter-relative-to-alphabet-list idx-of-letter-in-alphabet))
                                                                      (progn
                                                                       (multiple-value-bind (full-steps preliminary-subseq-remainder) (ceiling (1+ remainder) 2)
                                                                         (+ idx-of-next-letter-relative-to-alphabet-list (+ full-steps preliminary-subseq-remainder)))))
                                                                   ((minusp (- idx-of-next-letter-relative-to-alphabet-list idx-of-letter-in-alphabet))
                                                                     (progn
                                                                      (multiple-value-bind (full-steps preliminary-subseq-remainder) (truncate (1- remainder) 2)
                                                                        ;   (+ idx-of-next-letter-relative-to-alphabet-list (+ full-steps preliminary-subseq-remainder)))
                                                                        (+ idx-of-next-letter-relative-to-alphabet-list (+ full-steps preliminary-subseq-remainder)))))
                                                                   (t (progn
                                                                       (+ idx-of-next-letter-relative-to-alphabet-list remainder))))

                                                             (progn
                                                              (+ idx-of-next-letter-relative-to-alphabet-list (if
                                                                                                               ;; when the remainder is negative
                                                                                                               ;; it means we are going backwards
                                                                                                               ;; and in those cases even a remainder of 1 should count towards affecting the index
                                                                                                               ;; because, when deciding between x# or yb representations of the same note, we want to prioritize the "sharp-representation"
                                                                                                               ;; (for the same reason this `if` returns 0 if the remainder if not negative, because, for example,
                                                                                                               ;;  the remainder of 1 caused by a # should not cause G# to be considered Ab, G should not change to A)
                                                                                                               (minusp remainder)
                                                                                                               remainder
                                                                                                               0)))))

           (scale-of-next-letter-in-alphabet (cond (;; if the distance to the next letter in the *alphabet* list exceeds the bounds of that list (goes below 0)
                                                     (< idx-of-next-letter-relative-to-alphabet-list 0)
                                                      (progn
                                                       (append
                                                         ;; [root-idx, <end-of-list>]
                                                         (subseq *alphabet*
                                                                 (+ (length *alphabet*) idx-of-next-letter-relative-to-alphabet-list))
                                                         ;; [<start-of-list>, root-idx]
                                                         (subseq *alphabet*
                                                                 0 (1+ ;; to include the starting letter as well
                                                                      (+ (length *alphabet*) idx-of-next-letter-relative-to-alphabet-list))))))
                                                   ;; else, if the distance to the next letter in the *alphabet* list exceeds the bounds of that list (goes above the length of the list)
                                                   ((>= idx-of-next-letter-relative-to-alphabet-list (length *alphabet*))
                                                     (progn
                                                      (append
                                                        ;; [root-idx, <end-of-list>]
                                                        (subseq *alphabet*
                                                                (- idx-of-next-letter-relative-to-alphabet-list (length *alphabet*)))
                                                        ;; [<start-of-list>, root-idx]
                                                        (subseq *alphabet*
                                                                0 (1+ (- idx-of-next-letter-relative-to-alphabet-list (length *alphabet*)))))))
                                                   ;; else:
                                                   (t (progn
                                                       (append
                                                         ;; [root-idx, <end-of-list>]
                                                         (subseq *alphabet*
                                                                 idx-of-next-letter-relative-to-alphabet-list)
                                                         ;; [<start-of-list>, root-idx]
                                                         (subseq *alphabet*
                                                                 0 (1+ idx-of-next-letter-relative-to-alphabet-list))))))))
      scale-of-next-letter-in-alphabet)))


(defun digit-to-accidental (digit-to-convert)
  "Converts an integer `digit-to-convert` (number of half-steps) into a string of accidentals.
   For positive N, use '#' for one half-step and combine two consecutive '#' into 'x' (double-sharp).
   For negative N, use 'b' for one half-step and combine two consecutive 'b' into 'bb' (double-flat).

    For example:

        -5  => \"bbbbb\"
        3   => \"x#\"
        4   => \"xx\"
    "
  (let* ((acc-magnitude (abs digit-to-convert))
         (acc-direction-symbol (if (minusp digit-to-convert)
                                   (gethash :flat *accidental-notation*)
                                   (gethash :sharp *accidental-notation*))))
    (loop :with symbol-representation = '()
          :for i :from 1 :to acc-magnitude
          :do (if (and symbol-representation (s= (first symbol-representation) acc-direction-symbol))
                  ;; The most recent group is singular, so combine it:
                  (setf (first symbol-representation) (if (s= acc-direction-symbol (gethash :sharp *accidental-notation*))
                                                          (gethash :dsharp *accidental-notation*)
                                                          (gethash :dflat *accidental-notation*)))
                  ;; Otherwise, add a new singular accidental.
                  (push acc-direction-symbol symbol-representation))
          :finally (return ;; Reverse the groups to restore the original order and concatenate them.
                          (apply #'concatenate 'string (reverse symbol-representation))))))


(defun accidental-to-digit (accidental-to-convert)
  "Converts a string of accidentals ACC into its corresponding digit (number of half-steps).
   Interprets 'x' as +2, '#' as +1, 'bb' as -2, and 'b' as -1.

    For example:

        \"x#\"    => 3
        \"xx\"    => 4
        \"bbbbb\" => -5
    "
  (loop :with digit-representation = 0
        :for each-char :across accidental-to-convert
        :do (cond
             ;; x:
             ((s= each-char (gethash :dsharp *accidental-notation*))
               ;; then:
               (incf digit-representation 2))
             ;; #:
             ((s= each-char (gethash :sharp *accidental-notation*))
               ;; then:
               (incf digit-representation 1))
             ;; bb:
             ((s= each-char (gethash :dflat *accidental-notation*))
               ;; then:
               (decf digit-representation 2))
             ;; b:
             ((s= each-char (gethash :flat *accidental-notation*))
               ;; then:
               (decf digit-representation 1))
             ;; else:
             (t (error "Invalid accidental character: ~A" each-char)))
        :finally (return digit-representation)))


;; TODO: move these small function into a utils namespace:
(defun change-num-magnitude (original-num num-to-add-to-magnitude)
  (if (zerop original-num)
      ;; then just add to zero:
      (+ original-num num-to-add-to-magnitude)
      ;; else, alter magnitude appropriately:
      (let ((new-magnitude (+ (abs original-num)
                              num-to-add-to-magnitude)))
        (* (signum original-num) new-magnitude))))


;; this entire function should be rewritten in terms of _intervals_
(defun complete-the-scale (root-note-of-scale
                           scale-type)
  (declare (optimize (debug 3)))
  ;; TODO: leave around the comments next to the bindings describing how every data structure looks like
  (let* ((root-note-accidentals (subseq root-note-of-scale 1))

         (root-note-of-scale (char root-note-of-scale 0))

         (scale-formula (cond
                         ;; formulas in terms of chromatic intervals
                         ;; (which are themselves a count of half-steps from root):
                         ((s= scale-type "major") '(0 2 4 5 7 9 11 12))
                         ((s= scale-type "minor") '(0 2 3 5 7 8 10 12))
                         ;; TODO: add other scale formulas here
                         (t (progn
                             (format t "~%Theoretically unexpected condition: could not match scale formula~%")
                             (uiop:quit 1)))))

         (idx-of-root-in-alphabet (position root-note-of-scale *alphabet*
                                    :test #'s=))

         (correct-alphabet-of-scale (append
                                      ;; [root-idx, <end-of-list>]
                                      (subseq *alphabet*
                                              idx-of-root-in-alphabet)
                                      ;; [<start-of-list>, root-idx]
                                      (subseq *alphabet*
                                              0 (1+ idx-of-root-in-alphabet)))
                                    ;; ^^ looks like this: (D E F G A B C D)
                              ))
    ;; TODO: clean up all the useless comments everywhere
    (loop :with correct-notation-scale = '()
          :for previous-degree = 0 :then each-degree
          :for previous-letter = root-note-of-scale :then (char (first (last correct-notation-scale)) 0)
          :for previous-accidental = root-note-accidentals :then (subseq (first (last correct-notation-scale)) 1)
          :for each-degree :in scale-formula
          :for degree-diff = 0 :then (- each-degree previous-degree)
          :do
            (let* ((correct-alphabet-letter (if (< degree-diff 4) ;; we don't care about keeping track of the next letter if the distance between the degrees is more than 3 half-steps ;; TODO: test this
                                                (nth (length correct-notation-scale) correct-alphabet-of-scale)
                                                (nth (+ ;; count of all current letters the scale contains:
                                                       (length correct-notation-scale)
                                                       ;; + this many whole steps:
                                                       (floor degree-diff 2))
                                                     ;; in:
                                                     correct-alphabet-of-scale))))
              (let* ((previous-accidental-as-digit (accidental-to-digit previous-accidental))

                     (correct-notation-letter (str:concat correct-alphabet-letter
                                                ;; appending correct accidental:
                                                (if (or (= each-degree 0)
                                                        (= each-degree 12))
                                                    ;; then just return the root note's accidental:
                                                    root-note-accidentals
                                                    ;; else, figure out the correct accidental:
                                                    (let* ((prev-letter-e-or-b? (or (s= "E" previous-letter)
                                                                                    (s= "B" previous-letter)))

                                                           (inferred-accidental-digit
                                                            (cond ((= degree-diff 1) (+ previous-accidental-as-digit -1))
                                                                  ((= degree-diff 2) ;; a degree difference of 2 would mean a whole step is made from one letter to the next,
                                                                                    ;; which implies that the accidental stays the same (E and B jumps are accounted for in the code below)
                                                                                    previous-accidental-as-digit)
                                                                  ((= degree-diff 3) (+ previous-accidental-as-digit 1)) ;; TODO: not sure about this, test this
                                                                  ;; if degree-diff is more or equal to 4, then apply no accidental (0):
                                                                  ((>= degree-diff 4) 0)
                                                                  (t ;; else, assume degree-diff = 0 (meaning we are on the same note, ergo same accidental)
                                                                    previous-accidental-as-digit)))

                                                           (inferred-accidental-digit (if prev-letter-e-or-b? ;; (not (zerop encountered-e-or-b?-2))
                                                                                          (progn
                                                                                           ;; adding 1 because, if the previous letter was e or b,
                                                                                           ;; then moving from it to the next letter only take 1 half-step,
                                                                                           ;; unlike when moving from any other letter, where it takes two half-steps,
                                                                                           ;; and so to account for that offset of one missing half-step we add 1:
                                                                                           (1+ inferred-accidental-digit))
                                                                                          ;; else, leave as is:
                                                                                          inferred-accidental-digit)))
                                                      (digit-to-accidental inferred-accidental-digit))))))
                (setf correct-notation-scale
                  (append correct-notation-scale
                    (list correct-notation-letter)))))
          :finally (return correct-notation-scale))))


(defun simplify-note-notation (the-note)
  "Converts ASCII symbols into simplified character"
  (declare (type string the-note))
  (loop :with note-to-simplify = the-note
        :for acc-keyname :being :the :hash-key :in *accidental-notation*
        :using (hash-value acc-symbol)
        :do (when (str:contains? acc-symbol note-to-simplify)
                  (setf note-to-simplify
                    (str:replace-all acc-symbol
                                     (gethash acc-keyname *simple-notation*)
                                     note-to-simplify)))
        :finally (return (the string note-to-simplify))))


(defun find-answer (root-note-of-scale scale-type)
  "Returns the list containing:

    - The list containing that scale's notes adjusted to the possible accidental notation:

        '(\"TODO\" \"todo\")

    - The list containing that scale's notes in 'underlying' notation (the one where the root note
      is not altered by an accidental):

        '(\"TODO\" \"todo\") <-- (notes must represent the same as the list above)

    (underlying notation is used to check if the user provided an answer which is enharmonically correct,
     but which is not correct in the notation specified in the question)"
  (declare (type string root-note-of-scale)
           (type string scale-type))
  (let* (;; (index-of-root-in-octave (find-note-in-octave root-note-of-scale))
         (relevant-scale (complete-the-scale root-note-of-scale
                                             scale-type)))
    (the list relevant-scale)))


(defun pad-list-elems (list-of-elems &key (pad-count 4) (pad-direction :left))
  "Formats each element in the `list-of-elems` to be in a 4-spaces-left-justified string:

   \"Bb\" --> \"  Bb\" (2 spaces on the left)

   \"Bbb\" --> \" Bbb\" (1 space on the left)

    Default `pad-count` is 4

    Default `pad-direction` is `:left`"
  (mapcar #'(lambda (each-elem)
              (if (eql pad-direction :left)
                  ;; TODO: read up on `format` and perhaps include
                  ;; this conditional into the format-string
                  (format nil "~v,,,@A" pad-count each-elem)
                  (format nil "~v,,,A" pad-count each-elem)))
    list-of-elems))


(defun simplify-accidental (accidental-symbol-to-convert)
  (cond
   ((s= accidental-symbol-to-convert (gethash :sharp *accidental-notation*)) "#")
   ((s= accidental-symbol-to-convert (gethash :flat *accidental-notation*)) "b")
   ((s= accidental-symbol-to-convert (gethash :dsharp *accidental-notation*)) "x")
   ((s= accidental-symbol-to-convert (gethash :dflat *accidental-notation*)) "bb")
   ;; if not matched, return back to the caller:
   (t accidental-symbol-to-convert)))


(defun natural-pitch (letter)
  "Compute the natural pitch for LETTER using A as a reference.
   The idea is to sum the diatonic intervals along the rotated alphabet.
   Intervals are 2 half-steps except when leaving a B or an E, in which case they are 1."
  (let* ((idx (position (string-upcase letter) *alphabet* :test #'s=)))
    (unless idx
      (error "Letter ~A not found in alphabet" (string-upcase letter)))
    (loop :with pitch = 0
          :for i :from 0 :below idx
          :do (let ((current-letter (nth i *alphabet*)))
                (incf pitch (if (or (s= current-letter "B")
                                    (s= current-letter "E"))
                                1
                                2)))
          :finally (return pitch))))


(defun sum-accidentals-to-digit (accidentals-as-string)
  (loop :for each-acc :across (string accidentals-as-string)
          :sum (let ((simplified-acc (simplify-note-notation (string each-acc))))
                 (cond
                  ((s= simplified-acc (simplify-note-notation (gethash :sharp *accidental-notation*))) 1)
                  ((s= simplified-acc (simplify-note-notation (gethash :flat *accidental-notation*))) -1)
                  ((s= simplified-acc (simplify-note-notation (gethash :dsharp *accidental-notation*))) 2)
                  ((s= simplified-acc (simplify-note-notation (gethash :dflat *accidental-notation*))) -2)
                  ;; all else:
                  (t 0)))))


(defun note-to-pitch-class (note)
  "Converts the `note` string into its pitch class (0-11) computed from *alphabet*.
   The first character is taken as the natural note letter (its natural pitch is computed via `natural-pitch`)
   and subsequent characters adjust the pitch: '#' adds 1, 'b' subtracts 1, and 'x' adds 2."
  (let* ((letter (subseq note 0 1))
         (accidentals (subseq note 1))
         (offset (loop :for each-acc :across accidentals
                         :sum (cond
                               ((s= each-acc (simplify-note-notation (gethash :sharp *accidental-notation*))) 1)
                               ((s= each-acc (simplify-note-notation (gethash :flat *accidental-notation*))) -1)
                               ((s= each-acc (simplify-note-notation (gethash :dsharp *accidental-notation*))) 2)
                               ((s= each-acc (simplify-note-notation (gethash :dflat *accidental-notation*))) -2)
                               (;; if the accidental is invalid:
                                 (not (member each-acc (mapcar #'simplify-note-notation
                                                           (alex:hash-table-values *accidental-notation*))
                                              :test #'s=))
                                  ;; then, invalidate this note:
                                  most-positive-fixnum)
                               ;; all else:
                               (t 0))))
         (base (natural-pitch letter)))
    (mod (+ base offset) 12)))


(defun is-enharmonic? (scale1 scale2)
  "Return T if `scale1` and `scale2` are enharmonically equivalent but not identically spelled.
   Two scales are enharmonic if their corresponding note pitch classes (computed via `note-to-pitch-class`)
   are equal (modulo 12) while their lists of note names differ.
   For example:

    (is-enharmonic? '(\"Fb\" \"Gb\" \"Ab\" \"Bbb\" \"Cb\" \"Db\" \"Eb\" \"Fb\")
                    '(\"Dx\" \"Exbb###b\" \"F#xbbx\" \"Gx\" \"Ax##bb\" \"Bxbbbx#\" \"Cb#x#\" \"Dx\"))
    \\> T"
  (when (= (length scale1) (length scale2))
        (let ((pitches1 (mapcar #'note-to-pitch-class scale1))
              (pitches2 (mapcar #'note-to-pitch-class scale2)))
          (equal pitches1 pitches2))))


(defun check-answer (question-data user-answer gameplay-option)
  (declare (type list ;; looks like this: (5 (F♭ minor))
                 question-data)
           (type (or list string) user-answer))
  ;; TODO: you need to rename the names of the variables in here:
  (let* ((right-adjusted-padding (length "Relative Minor :"))
         (tone-to-find (first question-data))
         (simplified-user-answer (cond ((s= gameplay-option "1")
                                         (string (simplify-note-notation user-answer)))
                                       ((s= gameplay-option "2")
                                         (mapcar #'simplify-note-notation user-answer))
                                       (t (progn
                                           (format t "~%Theoretically unexpected condition: could not match gameplay option~%")
                                           (uiop:quit 1)))))
         (simplified-lower-case-user-answer (cond ((s= gameplay-option "1")
                                                    (string (string-downcase simplified-user-answer)))
                                                  ((s= gameplay-option "2")
                                                    (mapcar #'string-downcase simplified-user-answer))
                                                  (t (progn
                                                      (format t "~%Theoretically unexpected condition: could not match gameplay option~%")
                                                      (uiop:quit 1)))))
         (root-note-of-scale (first (second question-data)))
         (scale-type (second (second question-data)))

         ;; `found-answer` looks like this: '(F♭ G♭ A𝄫 ..)
         (found-answer (find-answer root-note-of-scale scale-type))

         ;; scale that is using correct notation:
         (simplified-relevant-scale (mapcar #'simplify-note-notation found-answer))
         (simplified-lower-case-relevant-scale (mapcar #'string-downcase simplified-relevant-scale))
         ;; ^^ looks like this: (fb gb abb bbb cb dbb ebb fb)
         (correct-tone-in-relevant-scale (nth (1- tone-to-find) found-answer))
         (simplified-correct-tone-in-relevant-scale (simplify-note-notation correct-tone-in-relevant-scale)))

    ;; Printing the result to the user:
    (format t "~%~%")
    (typecase user-answer
      (string (format t "~v,,,@A ~A~%~%"
                right-adjusted-padding
                "Answer :"
                correct-tone-in-relevant-scale))
      ;; else, assuming list:
      (t (format t "~v,,,@A ~A~%~%"
           right-adjusted-padding
           "Answer :"
           (str:join " " found-answer))))
    (cond
     ;; if:
     ((s= gameplay-option "1")
       ;; then:
       (let* (;; scale using underlying alphabet letters:
              (simplified-lower-case-found-tone (nth (1- tone-to-find) simplified-lower-case-relevant-scale)))
         (when (not (str:empty? user-answer))
               (cond
                ;; if `user-answer` matches
                ;; the correct letter in the relevant notation:
                ((s= simplified-lower-case-user-answer
                     simplified-lower-case-found-tone)
                  ;; then:
                  (format t "~v,,,@A \"~A\" -- ~A~%~%"
                    right-adjusted-padding
                    "Your answer :"
                    user-answer
                    (str:concat GREEN "correct" RESET_COLOR)))
                ;; "else, if the `user-answer` matches the alternative notation
                ;; of the correct key in the scale":
                ((is-enharmonic? (list simplified-user-answer) (list simplified-correct-tone-in-relevant-scale))
                  ;; then:
                  (format t "~v,,,@A \"~A\" -- ~A, but not notation-wise (~A)~%~%"
                    right-adjusted-padding
                    "Your answer :"
                    user-answer
                    (str:concat GREEN "Enharmonically correct" RESET_COLOR)
                    correct-tone-in-relevant-scale))
                ;; else:
                (t (format t "~v,,,@A \"~A\" -- ~A~%~%"
                     right-adjusted-padding
                     "Your answer :"
                     user-answer
                     (str:concat RED "nay" RESET_COLOR)))))))
     ;; if:
     ((s= gameplay-option "2")
       ;; then:
       (when (not (null user-answer))
             (cond (;; if the lists are of equal elements and order:
                     (equal simplified-lower-case-user-answer
                            simplified-lower-case-relevant-scale)
                      ;; then:
                      (format t "~v,,,@A \"~A\" -- ~A~%~%"
                        right-adjusted-padding
                        "Your answer :"
                        (str:join " " user-answer)
                        (str:concat GREEN "correct" RESET_COLOR)))
                   (t ;; else, going over each letter and checking if it's correct:
                     (let* ((discrete-assessment (loop :with discrete-assessment-scale = '()
                                                       :with all-passed? = t
                                                       :for each-note :in simplified-lower-case-user-answer
                                                       :for each-user-answer-note :in user-answer
                                                       :for each-correct-note :in simplified-lower-case-relevant-scale
                                                         :if (s= each-note each-correct-note) :do (setf discrete-assessment-scale
                                                                                                    (append discrete-assessment-scale
                                                                                                      (list (str:concat GREEN each-user-answer-note RESET_COLOR))))
                                                         :else :if (is-enharmonic? (list each-note)
                                                                                   (list each-correct-note)) :do (setf discrete-assessment-scale
                                                                                                                   (append discrete-assessment-scale
                                                                                                                     (list (str:concat YELLOW each-user-answer-note RESET_COLOR))))
                                                         ;; neither full nor enharmonic match (that is, wrong answer):
                                                         :else :do (progn
                                                                    (setf all-passed? nil)
                                                                    (setf discrete-assessment-scale
                                                                      (append discrete-assessment-scale
                                                                        (list (str:concat RED each-user-answer-note RESET_COLOR)))))
                                                       :finally (return (list all-passed? discrete-assessment-scale))))
                            (each-note-valid? (first discrete-assessment))
                            (colored-user-answer-as-list (second discrete-assessment)))
                       (if (and each-note-valid?
                                (= ;; making sure the user's answer
                                  ;; is of the same length as the correct answer:
                                  (length simplified-lower-case-user-answer)
                                  (length simplified-lower-case-relevant-scale)))
                           ;; then:
                           (format t "~v,,,@A \"~A\" -- ~A, but not notation-wise~%~%"
                             right-adjusted-padding
                             "Your answer :"
                             (str:join " " colored-user-answer-as-list)
                             (str:concat GREEN "Enharmonically correct" RESET_COLOR))
                           ;; else, if not all of the notes in the user's answer were correct,
                           ;; or the length didn't match:
                           (format t "~v,,,@A \"~A\" -- ~A~%~%"
                             right-adjusted-padding
                             "Your answer :"
                             (str:join " " (let* ((answer-length-diff (- (length simplified-lower-case-relevant-scale)
                                                                         (length simplified-lower-case-user-answer))))
                                             (cond
                                              ((zerop answer-length-diff)
                                                colored-user-answer-as-list)
                                              ;; else, mark every missing or extra note in the user's answer:
                                              ((plusp answer-length-diff)
                                                (append colored-user-answer-as-list
                                                  (make-list answer-length-diff :initial-element (str:concat RED "_" RESET_COLOR))))
                                              ;; else, there are more letter in the user's answer than in actual answer,
                                              ;; so marking the extra notes:
                                              (t (append colored-user-answer-as-list
                                                   (mapcar #'(lambda (each-extra-letter)
                                                               (str:concat RED_WITH_UNDERLINE each-extra-letter RESET_COLOR))
                                                     (subseq user-answer (+ (length user-answer) answer-length-diff))))))))
                             (str:concat RED "nay" RESET_COLOR))))))))
     (t (progn
         (format t "~%Theoretically unexpected condition: could not match gameplay option~%")
         (uiop:quit 1))))

    ;; Printing whole scale and complementary scales
    (let* ((uniformly-spaced-ruler (pad-list-elems (alex:iota (length found-answer) :start 1)
                                                   :pad-count 4
                                                   :pad-direction :right))
           (uniformly-spaced-string-of-scale (pad-list-elems found-answer
                                                             :pad-count 4
                                                             :pad-direction :right))
           (relative-scale-type (cond
                                 ((s= scale-type "major") "minor")
                                 ((s= scale-type "minor") "major")
                                 ;; TODO: add other scale formulas here
                                 (t (progn
                                     (format t "~%Theoretically unexpected condition: could not match scale formula~%")
                                     (uiop:quit 1)))))
           (root-note-of-relative-scale (cond
                                         ((s= scale-type "major") (nth (- (length found-answer) 3)
                                                                       found-answer))
                                         ((s= scale-type "minor") (nth (- (length found-answer) 6)
                                                                       found-answer))
                                         ;; TODO: add other scale formulas here
                                         (t (progn
                                             (format t "~%Theoretically unexpected condition: could not match scale formula~%")
                                             (uiop:quit 1)))))
           (uniformly-spaced-relative-scale (pad-list-elems (find-answer
                                                              root-note-of-relative-scale
                                                              relative-scale-type)
                                                            :pad-count 4
                                                            :pad-direction :right)))
      (format t "~v,,,@A ~A~%"
        right-adjusted-padding
        "|"
        (str:join "" uniformly-spaced-ruler))
      (format t "~v,,,@A ~A~%"
        right-adjusted-padding
        "Scale :"
        (str:join "" uniformly-spaced-string-of-scale))
      (format t "~v,,,@A ~A~%"
        right-adjusted-padding
        (str:concat "Relative " (string-capitalize relative-scale-type) " :")
        (str:join "" uniformly-spaced-relative-scale)))

    ;; question separator:
    (format t "~%~%~v@{~A~:*~}~%" 50 "-")))


(defun ask-question (gameplay-option level-option)
  "Returns a list such as '(4 \"C#\" \"major\")
    representing the tone and the scale in which to find it"
  (declare (type string gameplay-option)
           (type list level-option))
  (let (;; range [2:8) because we don't want to include 1st and 8th tones (because they are obvious):
        ;; TODO: consider delineating the logic for different gameplays:
        ;; the `random-tone` is not used in some gameplay options at all (e.g. 2 - write out the scale)
        (random-tone (+ 2 (random 6 (make-random-state t))))
        ;; "random scale" such as "C# major"
        (random-scale (nth (random (1- (length level-option)) (make-random-state t)) level-option)))
    (cond
     ;; if:
     ((s= gameplay-option "1")
       ;; then:
       (progn
        (format t "~%~%~A*~A What is the " BLUE RESET_COLOR)
        (cond ((= random-tone 1)
                ;; then:
                (format t "~Ast tone of ~A?~%" random-tone random-scale))
              ((= random-tone 2)
                ;; then:
                (format t "~And tone of ~A?~%" random-tone random-scale))
              ((= random-tone 3)
                ;; then:
                (format t "~Ard tone of ~A?~%" random-tone random-scale))
              ;; else:
              (t (format t "~Ath tone of ~A?~%" random-tone random-scale)))
        (the list (list random-tone (str:words random-scale)))))
     ;; if:
     ((s= gameplay-option "2")
       ;; then:
       (progn
        (format t "~%~%~A*~A Write out ~A scale:~%" BLUE RESET_COLOR random-scale)
        (the list (list 1 (str:words random-scale)))))
     (t (progn
         (format t "~%Theoretically unexpected condition: could not match gameplay option~%")
         (uiop:quit 1))))))


(defun get-complementing-alphabet-of-letters (the-note)
  (let* ((the-letter (string-upcase (char the-note 0)))
         (letter-position-in-alphabet (position the-letter *alphabet* :test #'s=))
         (union-set (if (minusp (1- letter-position-in-alphabet))
                        ;; if the `letter` is "A", because CL does not support negative indexing:
                        (append
                          (subseq *alphabet* (1- (length *alphabet*)))
                          (subseq *alphabet* 0 (1- (length *alphabet*))))
                        ;; else:
                        (append
                          (subseq *alphabet* letter-position-in-alphabet)
                          (subseq *alphabet* 0 letter-position-in-alphabet)))))
    ;; now, disjoining:
    (remove the-letter union-set :test #'s=)))


(defun find-how-far-enharmonic-note-in-half-steps (reference-note alt-note)
  (let* ((reference-note-accidentals (subseq reference-note 1))
         (alt-note-accidentals (subseq alt-note 1))
         (half-step-difference (- (natural-pitch (char alt-note 0))
                                  (natural-pitch (char reference-note 0))))
         (offset-of-reference-note-adjusted-to-alt-note-accidentals (- (sum-accidentals-to-digit alt-note-accidentals)
                                                                       (sum-accidentals-to-digit reference-note-accidentals)))
         (half-step-difference (+ half-step-difference offset-of-reference-note-adjusted-to-alt-note-accidentals)))
    (cond ;; to account for the length of the *alphabet* list:
         ((> half-step-difference 6) (- half-step-difference 12))
         ((< half-step-difference -6) (+ half-step-difference 12))
         ;; else, just returning a valid difference:
         (t half-step-difference))))


(defun give-cheats (cmd-context)
  (let* ((c-position-in-alphabet (position "C" *alphabet* :test #'s=))
         (scale-types '("major" "minor")) ;; TODO: this, similarly to other things, should be abstracted away so the manual updating of this line is not needed
         (alt-notations-count (cli:getopt cmd-context :alt-notation))
         (c-to-b-alphabet (append
                            (subseq *alphabet* c-position-in-alphabet)
                            (subseq *alphabet* 0 c-position-in-alphabet)))
         (c-to-b-alphabet-with-accidentals (loop :with complete-scale = '()
                                                 :for each-letter :in c-to-b-alphabet
                                                   :if (or (s= each-letter "E")
                                                           (s= each-letter "B"))
                                                 :do (setf complete-scale
                                                       (append complete-scale
                                                         (list each-letter)))
                                                   :else
                                                 :do (setf complete-scale
                                                       (append complete-scale
                                                         (list each-letter (str:concat each-letter (gethash :sharp *accidental-notation*)))))
                                                 :finally (return complete-scale))))
    (loop :for each-scale-type :in scale-types
          :do (progn
               (format t "~%~%~42<~A~>~%~%" (str:concat
                                              YELLOW_WITH_UNDERLINE
                                              (string-upcase each-scale-type)
                                              RESET_COLOR))
               (loop :for each-note :in c-to-b-alphabet-with-accidentals
                     :do (let* ((uniformly-spaced-ruler (pad-list-elems (mapcar #'(lambda (each-number)
                                                                                    (str:concat "(" (write-to-string each-number) ")"))
                                                                          (alex:iota 8 :start 1))
                                                                        :pad-count 7
                                                                        :pad-direction :right))
                                (uniformly-spaced-string-of-scale (pad-list-elems (find-answer
                                                                                    each-note
                                                                                    each-scale-type)
                                                                                  :pad-count 4
                                                                                  :pad-direction :right))
                                (base-scale-to-print (format nil "  ~A~A~A~%"
                                                       (format nil "~4@<~A~>" each-note)
                                                       (format nil "~3@<~A~>" "|")
                                                       (str:join "-  " uniformly-spaced-string-of-scale))))

                           (format t "~v<( ~A )~>~%" (floor (length base-scale-to-print) 2) each-note)
                           (format t "~7,,,@A ~A~%"
                             "|"
                             (str:join "" uniformly-spaced-ruler)) ;; | (1) (2) (3) ..
                           (format t "~A" base-scale-to-print)
                           (let* ((all-alt-notation-prints
                                   (loop :with all-alt-notation-prints = '()
                                         :for each-alt-letter :in (get-complementing-alphabet-of-letters each-note)
                                         :do (let* ((each-alt-letter-half-steps-away-from-primary (- ;; making negative, because this value should represent
                                                                                                    ;; the distance from the alt-note to the primary note:
                                                                                                    ;; e.g. `each-note` = C#, `each-alt-letter` = D,
                                                                                                    ;; distance in half steps is 1, but we use this value
                                                                                                    ;; to determine the accidental to mark the D with
                                                                                                    ;; so we make it -1, which translates to the "b" accidental
                                                                                                    (find-how-far-enharmonic-note-in-half-steps each-note each-alt-letter)))
                                                    (each-alt-letter-accidental (digit-to-accidental each-alt-letter-half-steps-away-from-primary))
                                                    (each-alt-letter (str:concat each-alt-letter each-alt-letter-accidental))
                                                    (uniformly-spaced-string-of-scale (pad-list-elems (find-answer
                                                                                                        each-alt-letter
                                                                                                        each-scale-type)
                                                                                                      :pad-count 4
                                                                                                      :pad-direction :right)))
                                               (setf all-alt-notation-prints
                                                 (append all-alt-notation-prints (list
                                                                                  (list each-alt-letter
                                                                                        (format nil "  ~A~A~A~%"
                                                                                          (format nil "~4@<~A~>" each-alt-letter)
                                                                                          (format nil "~3@<~A~>" "|")
                                                                                          (str:join "-  " uniformly-spaced-string-of-scale)))))))
                                         :finally (return all-alt-notation-prints)))
                                  (alt-notations-to-print (loop :with alt-notations-to-print = '()
                                                                :for each-alt-notation-print :in (sort all-alt-notation-prints (lambda (current next)
                                                                                                                                 (< (length (first current)) (length (first next)))))
                                                                :do (when (< (length alt-notations-to-print) alt-notations-count)
                                                                          (setf alt-notations-to-print
                                                                            (append alt-notations-to-print (list (second each-alt-notation-print)))))
                                                                :finally (return alt-notations-to-print))))
                             ;; printing alternative notation for the base scale:
                             (format t "~{~A~}~%" alt-notations-to-print))
                           (terpri)))))))


(defun play (gameplay-option level-option)
  (declare (type string gameplay-option)
           (type list level-option))
  (let ((print-continuation-options? t))
    (loop :for question = (ask-question gameplay-option level-option)
          :for user-answer = (cond ((s= gameplay-option "1")
                                     (str:trim (ask-user-input "~%~%Your answer (optional): ")))
                                   ((s= gameplay-option "2")
                                     (str:split " "
                                                (ask-user-input "~%~%Your answer (space-separated): ")
                                                :omit-nulls t))
                                   (t (progn
                                       (format t "~%Theoretically unexpected condition: could not match gameplay option~%")
                                       (uiop:quit 1))))
          :do
            (cond
             ;; if:
             ((if (typep user-answer 'string)
                  (member user-answer '("exit" "quit" "q") :test #'s=)
                  (< (length (set-difference '("exit" "quit" "q") user-answer
                                             :test #'s=))
                     3))
               ;; then:
               (uiop:quit 0))
             ;; else:
             (t (progn
                 (check-answer question user-answer gameplay-option)
                 ;; ask if user wants to continue playing:
                 (when (member (str:trim
                                 (ask-user-input (when print-continuation-options?
                                                       "(Press \"enter\" to continue or type \"q\" to quit)~%")))
                               '("exit" "quit" "q")
                               :test #'s=)
                       (uiop:quit 0))
                 ;; we only want to print continuation options once:
                 (when print-continuation-options?
                       (setf print-continuation-options? nil))))))))


(defun muz-cmd-args ()
  "Returns a list of command line arguments this script takes"
  (list
   (cli:make-option
     :flag
     :description "Enables simple notation of accidentals when specified"
     :short-name #\s
     :key :simple-notation)
   (clingon:make-option
     :flag ;; `:flag` is an alias for `:boolean/true`
     :description "Prints cheatsheet of scales"
     :short-name #\c
     :key :cheats)
   (clingon:make-option
     :integer
     :description "Specifies how many alternative notations should be printed for each scale if `-c` flag is used"
     :short-name #\a
     :key :alt-notation
     :initial-value 2)))


(defun muz-handler (cmd-context)
  (let* ((simple-notation-enabled? (let ((specified? (cli:getopt cmd-context :simple-notation)))
                                     (when specified?
                                           (format t "* Simple Notation - Enabled~%"))
                                     specified?))

         (get-cheats? (cli:getopt cmd-context :cheats)))

    (setf *accidental-notation* (if simple-notation-enabled?
                                    *simple-notation*
                                    *unicode-notation*))

    (let ((initialized-game-obj (init-game-resources)))
      (when get-cheats?
            (give-cheats cmd-context)
            (uiop:quit 0))
      (play (ask-gameplay-options initialized-game-obj)
            (ask-level-options initialized-game-obj)))))


(defun muz-command ()
  (cli:make-command
    :name "muz"
    :description "Anki-like program for learning music theory"
    :options (muz-cmd-args)
    :handler #'muz-handler))


(defun main ()
  (cli:run (muz-command)))
