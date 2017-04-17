(ql:quickload :split-sequence)

; parsers :: input -> (values input result)
(defun parse-ws (str)
  (let
    ((first-not-ws
       (position-if-not
         (lambda (c) (or
                       (char= c #\Space)
                       (char= c #\Tab)
                       (char= c #\Newline)))
         str)))
    (cond
      ((eql first-not-ws 0) ; first char not ws: error
       (values str :error "expected whitespace"))
      (first-not-ws       ; some not ws, but not first character
       (values (subseq str first-not-ws) (subseq str 0 first-not-ws)))
      (t                  ; all chars were ws
       (values nil str)))))

(defun parse-alpha (str)
  (let
    ((first-not
       (position-if-not #'alpha-char-p str)))
    (cond
      ((eql first-not 0)  ; first char not alpha error
       (values str :error "expected alphabetic"))
      (first-not          ; some not alpha, but not first character
       (values (subseq str first-not) (subseq str 0 first-not)))
      (t                  ; all chars were alpha
       (values nil str)))))

(defun parse-alphanum (str)
  (let
    ((first-not
       (position-if-not #'alphanumericp str)))
    (cond
      ((eql first-not 0)  ; first char not alphanum error
       (values str :error "expected alphanumeric"))
      (first-not          ; some not alphanum, but not first character
       (values (subseq str first-not) (subseq str 0 first-not)))
      (t                  ; all chars were alphanum
       (values nil str)))))

(defun parse-alpha-underscore (str)
  (let
    ((first-not
       (position-if-not
         (lambda (c) (or (alpha-char-p c) (char= c #\_)))
         str)))
    (cond
      ((eql first-not 0)  ; first char not alpha error
       (values str :error "expected alphabetic or _"))
      (first-not          ; some not alpha, but not first character
       (values (subseq str first-not) (subseq str 0 first-not)))
      (t                  ; all chars were alpha
       (values nil str)))))

(defun parse-alphanum-underscore (str)
  (let
    ((first-not
       (position-if-not
         (lambda (c) (or (alphanumericp c) (char= c #\_)))
         str)))
    (cond
      ((eql first-not 0)  ; first char not alphanum error
       (values str :error "expected alphanumeric or _"))
      (first-not          ; some not alphanum, but not first character
       (values (subseq str first-not) (subseq str 0 first-not)))
      (t                  ; all chars were alphanum
       (values nil str)))))

(defun parse-end (str)
  (if (= (length str) 0)
      (values nil nil)
      (values str :error "expected end of input")))

; parser combinators :: stuff -> parser
(defun parse-char (c)
  (lambda (input)
    (if (and (/= (length input) 0) (char= (char input 0) c))
        (values (subseq input 1) c)
        (values input :error (format nil "expected '~A'" c)))))

(defun parse-not-char (c)
  (lambda (input)
    (let
      ((first-pos (position-if (lambda (k) (char= k c)) input)))
    (cond
      ((eql first-pos 0)  ; first char is c: error
       (values input :error (format nil "expected not ~A" c)))
      (first-pos          ; some c, but not first character
       (values (subseq input first-pos) (subseq input 0 first-pos)))
      (t                  ; all chars were not c
       (values nil input))))))

(defun parse-keyword (kw)
  (lambda (input)
    (if (eql (search kw input) 0)
        (values (subseq input (length kw)) kw)
        (values input :error (format nil "expected \"~A\"" kw)))))

(defun parse-opt (parser)
  (lambda (input)
    (multiple-value-bind (remaining result) (funcall parser input)
      (if (eq result :error)
          (values input nil)
          (values remaining result)))))

(defun parse-alt (&rest parsers)
  (lambda (input)
    (loop for parser in parsers do
          (multiple-value-bind
            (remaining result) (funcall parser input)
            (when (not (eq result :error))
              (return (values remaining result))))
          finally (return (values input :error "no alternate succeeded")))))

(defun parse-enclosed (parser &key before after)
  (lambda (input)
    (block parsefn
      (let ((i input))
        (when before
          (multiple-value-bind
            (remaining result msg) (funcall before i)
            (when (eq result :error)
              (return-from parsefn (values input :error msg)))
            (setf i remaining)))
        (multiple-value-bind
          (remaining result msg) (funcall parser i)
          (when (eq result :error)
            (return-from parsefn (values input :error msg)))
          (setf i remaining)
          (when after
            (multiple-value-bind
              (remaining result msg) (funcall after i)
              (when (eq result :error)
                (return-from parsefn (values input :error msg)))
              (setf i remaining))) (values i result))))))

(defun parse-chain (&rest parsers)
  (lambda (input)
    (block parsefn
      (let ((i input) (res :error))
        (loop for parser in parsers do
              (multiple-value-bind
                (remaining result msg) (funcall parser i)
                (when (eq result :error)
                  (return-from parsefn (values input :error msg)))
                (setf i remaining)
                (setf res result)))
        (values i res)))))

(defun parse-parts (&rest parsers)
  (lambda (input)
    (block parsefn
      (let ((i input) (res nil))
       (loop for parser in parsers do
              (multiple-value-bind
                (remaining result msg) (funcall parser i)
                (when (eq result :error)
                  (return-from parsefn (values input :error msg)))
                (setf i remaining)
                (setf res (cons result res))))
       (values i (nreverse res))))))

(defun parse-require (parser)
  (lambda (input)
    (multiple-value-bind
      (remaining result msg) (funcall parser input)
      (if (eq result :error)
          (error msg)
          (values remaining result)))))

(defun parse-map (parser fn)
  (lambda (input)
    (multiple-value-bind
      (remaining result msg) (funcall parser input)
      (if (eq result :error)
          (values remaining :error msg)
          (values remaining (funcall fn result))))))

; parsers for SxE
(defun parse-dots (input)
  (funcall 
    (parse-map
      (parse-chain
        (parse-opt #'parse-ws)
        (parse-keyword "..."))
      (lambda (_) (declare (ignore _)) 'dots))
    input))

(defun parse-type (input)
  (funcall
    (parse-map
      (parse-enclosed
        (parse-alt
          (parse-keyword "int")
          (parse-keyword "double")
          (parse-keyword "string")
          (parse-keyword "date")
          (parse-keyword "variant"))
        :before (parse-opt #'parse-ws))
      (lambda (kw)
        (intern (string-upcase kw))))
    input))

(defun parse-ident (input)
  (funcall
    (parse-map
      (parse-parts
        (parse-opt #'parse-ws)
        #'parse-alpha-underscore
        (parse-opt #'parse-alphanum-underscore))
      (lambda (parts)
        (destructuring-bind (_ start rst) parts
          (declare (ignore _))
          (concatenate 'string start rst))))
    input))

(defun parse-vardecl (input)
  (funcall
    (parse-map
      (parse-parts
        #'parse-ident
        (parse-opt #'parse-ws)
        (parse-char #\:)
        #'parse-type)
      (lambda (parts)
        (destructuring-bind (ident _1 _2 ty) parts
          (declare (ignore _1 _2))
          (list ident ty))))
    input))

; this sucks, we'll make it better later
(defun parse-label (input)
  (funcall
    (parse-map
      (parse-enclosed
        (parse-not-char #\")
        :before (parse-char #\")
        :after (parse-char #\"))
      (lambda (label) (list label 'label)))
    input))

(defun parse-element (input)
  (funcall
    (parse-alt #'parse-vardecl #'parse-label)
    input))

(defun parse-cell-contents (input)
  (funcall
    (parse-alt
      (parse-map
        (parse-enclosed
          #'parse-element
          :before (parse-char #\<)
          :after (parse-chain
                   (parse-opt #'parse-ws)
                   (parse-char #\>)))
        (lambda (element)
          (append element (list :origin))))
      #'parse-element
      #'parse-dots)
    input))

; (handle invalid trailing content)
(defun parse-cell (input)
  (funcall
    (parse-enclosed
      #'parse-cell-contents
      :after (parse-chain
               (parse-opt #'parse-ws)
               #'parse-end))
    input))

(defun parse-name (input)
  (funcall
    (parse-enclosed
      #'parse-ident
      :after (parse-chain
               (parse-opt #'parse-ws)
               (parse-char #\=)
               (parse-opt #'parse-ws)
               #'parse-end))
    input))

; SxE: input processing
(defun newrowp (line)
  (not (find-if (lambda (c) (and
                              (char/= c #\-)
                              (char/= c #\|)
                              (char/= c #\Space)
                              (char/= c #\Tab)))
                line)))

(defun read-data-line (line)
  (mapcar
    (lambda (str) (string-trim '(#\Space #\Tab) str))
    (split-sequence:split-sequence #\| line)))

(defun read-row (&optional (input-stream *standard-input*))
  (let* ((last-row nil)
         (lines
           (loop for line = (read-line input-stream nil nil t)
                 while (and line (not (newrowp line)))
                 collecting (read-data-line line)
                 finally (when (string= line "")
                           (setf last-row t)))))
    (unless lines (return-from read-row (values nil t)))
    (if (apply #'= (mapcar #'list-length lines))
        (values
          (apply #'mapcar
                 (lambda (&rest strs)
                   (apply #'concatenate 'string strs))
                 lines)
          last-row)
        (error "multi-line row has differing cell count per line"))))

(defun read-grid (&optional (input-stream *standard-input*))
  (loop for (row last-row) =
          (multiple-value-list (read-row input-stream))
        while row
        collecting row
        do
        (when last-row (loop-finish))))

(defun read-name (&optional (input-stream *standard-input*))
  (let ((line (read-line input-stream nil nil)))
    (if (null line)
        nil
        (multiple-value-bind
          (_ result msg) (parse-name line)
          (declare (ignore _))
          (if (eq result :error)
              (error "Invalid name line: \"~A\" (~A)" line msg)
              result)))))

(defun read-sxe (&optional (input-stream *standard-input*))
  (let ((name (read-name input-stream)))
    (if (null name)
        nil ; end of input
        (let ((grid (read-grid input-stream)))
          (unless grid (error "no grid provided"))
          (unless (apply #'= (mapcar #'list-length grid))
            (error "rows have different column counts"))
          (values
            name
            (mapcar
              (lambda (row)
                (mapcar
                  (lambda (cell)
                    (multiple-value-bind
                      (_ result msg) (parse-cell cell)
                      (declare (ignore _))
                      (when (eq result :error)
                        (error "Invalid cell: \"~A\" (~A)" cell msg))
                      result))
                  row))
              grid))))))

; SxE: "pre-processed" structure
(defstruct sxe-type
  (name "" :type string)
  (origin '(0 . 0) :type cons)
  (labels nil :type list)
  (arrays nil :type list)
  (scalars nil :type list))

(defun dots-direction (grid)
  (flet ((dotsp (x) (eq x 'dots))
         (lastelt (lis) (car (last lis))))
    (cond
      ((every #'dotsp (car grid)) 'top)
      ((every #'dotsp (mapcar #'car grid)) 'left)
      ((every #'dotsp (lastelt grid)) 'bottom)
      ((every #'dotsp (mapcar #'lastelt grid)) 'right)
      (t nil)
    )))

(defun build-sxe (name grid)
  (let ((sxe (make-sxe-type :name name))
        (last-row (1- (length grid)))
        (last-col (1- (length (car grid))))
        (dots-dir (dots-direction grid)))
    (loop for row in grid
          for i = 0 then (1+ i)
          with origin-set = nil
          do
          (loop for cell in row
                for j = 0 then (1+ j)
                for loc = (cons i j)
                for should-be-dots = (case dots-dir
                    (top (= i 0))
                    (left (= j 0))
                    (bottom (= i last-row))
                    (right (= j last-col)))
                for should-expand-vars = (case dots-dir
                    (top (= i 1))
                    (left (= j 1))
                    (bottom (= i (1- last-row)))
                    (right (= j (1- last-col))))
                do
                (when (and (listp cell) (member :origin cell))
                  (if origin-set (error "multiple origin cells"))
                  (setf (sxe-type-origin sxe) loc
                        origin-set t))
                (cond
                  ((eq cell 'dots) 
                   (unless should-be-dots
                     (error "invalid dots at (~A, ~A)" i j)))
                  (should-be-dots
                    (error "expected dots at (~A, ~A)" i j))
                  ((eq (cadr cell) 'label)
                   (push (list (car cell) loc)
                         (sxe-type-labels sxe)))
                  (t ; must be a member defn
                   (if should-expand-vars
                       (push
                         (list (car cell) loc (cadr cell) dots-dir)
                         (sxe-type-arrays sxe))
                       (push (list (car cell) loc (cadr cell))
                         (sxe-type-scalars sxe)))))
                ))
    (setf (sxe-type-labels sxe) (nreverse (sxe-type-labels sxe))
          (sxe-type-scalars sxe) (nreverse (sxe-type-scalars sxe))
          (sxe-type-arrays sxe) (nreverse (sxe-type-arrays sxe)))
    sxe))

; SxE: code generation
(defun format-type (dest ty _colon _at)
  (declare (ignore _colon _at))
  (let ((ty-str (case ty
                   (int "Long")
                   (double "Double")
                   (string "String")
                   (date "Date")
                   (variant "Variant"))))
    (if (not ty-str) (error "Invalid type: ~A" ty))
    (format dest ty-str)))

(defun emit-typedef (sxe &key (public nil))
  (with-output-to-string (s)
    (format s "~:[Private~;Public~] Type ~A~%"
            public
            (sxe-type-name sxe))
    (format s "~{~{    ~A As ~*~/format-type/~%~}~}"
            (sxe-type-scalars sxe))
    (format s "~{~{    ~A() As ~*~/format-type/~*~%~}~}"
            (sxe-type-arrays sxe))
    (format s "End Type~%")))

(defun last-cell-expr (dir off-row off-col)
  (case dir
    (top
      (format nil
              "origin.Parent.Cells(~
               1, ~
               origin.Column + ~A)~
              .End(xlDown)"
              off-col))
    (left
      (format nil
              "origin.Parent.Cells(~
               origin.Row + ~A, ~
               1)~
              .End(xlToRight)"
              off-row))
    (bottom
      (format nil
              "origin.Parent.Cells(~
               origin.Parent.Rows.Count, ~
               origin.Column + ~A)~
              .End(xlUp)"
              off-col))
    (right
      (format nil
              "origin.Parent.Cells(~
               origin.Row + ~A, ~
               origin.Parent.Columns.Count)~
              .End(xlToLeft)"
              off-row))))

(defun tmp-dims-expr (dir off-row off-col)
  (case dir
    (top
      (format nil "(1 To origin.Offset(~A, ~A).Row ~
                   - last_cell.Row + 1, 1 To 1)"
              off-row off-col))
    (left
      (format nil "(1 To 1, 1 To origin.Offset(~A, ~A).Column ~
                   - last_cell.Column + 1)"
              off-row off-col))
    (bottom
      (format nil "(1 To last_cell.Row ~
                   - origin.Offset(~A, ~A).Row + 1, 1 To 1)"
              off-row off-col))
    (right
      (format nil "(1 To 1, 1 To last_cell.Column ~
                   - origin.Offset(~A, ~A).Column + 1)"
              off-row off-col))))

(defun result-dims-expr (dir)
  (case dir
    (top "LBound(tmp, 1) To UBound(tmp, 1)")
    (left "LBound(tmp, 2) To UBound(tmp, 2)")
    (bottom "LBound(tmp, 1) To UBound(tmp, 1)")
    (right "LBound(tmp, 2) To UBound(tmp, 2)")))

(defun tmp-index-expr (dir i)
  (case dir
    (top (format nil "tmp(~A, 1)" i))
    (left (format nil "tmp(1, ~A)" i))
    (bottom (format nil "tmp(~A, 1)" i))
    (right (format nil "tmp(1, ~A)" i))))

(defun format-array-parse (dest name mbr dir off-row off-col)
  (format dest
          "    Set last_cell = ~A~%"
          (last-cell-expr dir off-row off-col))
  (format dest
          "    ReDim tmp ~A~%"
          (tmp-dims-expr dir off-row off-col))
  (format dest
          "    If last_cell.Row = origin.Row + ~A ~
           And last_cell.Column = origin.Column + ~A Then~%"
          off-row off-col)
  (format dest
          "        tmp(LBound(tmp, 1), LBound(tmp, 2)) ~
           = origin.Parent.Range(~
           origin.Offset(~A, ~A), last_cell).Value~%"
          off-row off-col) 
  (format dest
          "    Else~%")
  (format dest
          "        tmp = origin.Parent.Range(~
           origin.Offset(~A, ~A), last_cell).Value~%"
          off-row off-col)
  (format dest
          "    End If~%")
  (format dest "    ReDim Parse~A.~A (~A)~%"
          name mbr (result-dims-expr dir))
  (format dest "    For i = ~A~%"
          (result-dims-expr dir))
  (format dest "        Parse~A.~A(i) = ~A~%"
          name mbr (tmp-index-expr dir "i"))
  (format dest "    Next i~%"))

(defun emit-parser (sxe &key (public nil) (error-code 66666))
  (let ((name (sxe-type-name sxe))
        (origin-x (car (sxe-type-origin sxe)))
        (origin-y (cdr (sxe-type-origin sxe))))
    (with-output-to-string (s)
      (format s
              "~:[Private~;Public~] Function Parse~A ~
               (ByVal origin As Range) As ~:*~A~%"
              public name)
      (loop for (lbl (x . y)) in (sxe-type-labels sxe)
            for off-row = (- x origin-x)
            for off-col = (- y origin-y)
            do
            (format s
                    "    If origin.Offset(~A, ~A).Value ~
                     <> \"~A\" Then~%"
                    off-row off-col lbl)
            (format s
                    "        Err.Raise ~A~%"
                    error-code)
            (format s "    End If~%"))

      (when (and (sxe-type-labels sxe) (sxe-type-scalars sxe))
        (format s "~%"))
      (loop for (mbr (x . y) ty) in (sxe-type-scalars sxe)
            for off-row = (- x origin-x)
            for off-col = (- y origin-y)
            do
            (format s
                    "    Parse~A.~A = origin.Offset(~A, ~A).Value~%"
                    name mbr off-row off-col))

      (when (and
              (or (sxe-type-labels sxe) (sxe-type-scalars sxe))
              (sxe-type-arrays sxe))
        (format s "~%    Dim tmp() As Variant~%")
        (format s "~%    Dim last_cell As Range~%")
        (format s "~%    Dim i As Long~%"))
      (loop for (mbr (x . y) ty dir) in (sxe-type-arrays sxe)
            for off-row = (- x origin-x)
            for off-col = (- y origin-y)
            do
            (format-array-parse s name mbr dir off-row off-col))

      (format s "End Function~%"))))

(defun sxe-translator (&optional (input-stream *standard-input*)
                                 (output-stream *standard-output*))
  (loop for (name grid) =
          (multiple-value-list (read-sxe input-stream))
        while name
        for sxe = (build-sxe name grid)
        collecting (list (emit-typedef sxe) (emit-parser sxe))
          into output
        finally 
        (loop for typedef in (mapcar #'car output)
              for i = 0 then (1+ i)
              do
              (when (/= i 0) (terpri output-stream))
              (princ typedef output-stream))
        (loop for parser in (mapcar #'cadr output)
              do
              (terpri output-stream)
              (princ parser output-stream))))

(defun output-name (file)
  (let ((filepath (pathname file)))
    (make-pathname
      :directory (pathname-directory filepath)
      :name (pathname-name filepath)
      :type "bas")))

(defun main ()
  (multiple-value-bind (_ err)
    (ignore-errors
      (if (<= (length *posix-argv*) 1)
          (sxe-translator)
          (loop for file in (subseq *posix-argv* 1)
                do
                (with-open-file (in file :direction :input)
                  (with-open-file (out
                                    (output-name file)
                                    :direction :output
                                    :if-exists :supersede)
                    (sxe-translator in out))))))
    (declare (ignore _))
    (when err 
      (format *error-output* "Error: ~A" err))))

; (save-lisp-and-die "sxe.exe" :toplevel #'main :executable t)
