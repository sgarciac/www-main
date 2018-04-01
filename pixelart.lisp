;; a pixmap, where 0,0 is the bottom left corner, and increases to the upper right corner
;; PIXELS

(defstruct pixel r g b)
(defstruct yuv-pixel y u v)

(defun rgb-to-yuv (pixel)
  (let ((y (+ (* 0.299 (pixel-r pixel)) (* 0.587 (pixel-g pixel)) (* 0.114 (pixel-b pixel)))))
    (make-yuv-pixel
     :y y
     :u (* 0.436 (/ (- (pixel-b pixel) y) (- 1 0.114)))
     :v (* 0.615 (/ (- (pixel-r pixel) y) (- 1 0.299))))))

(defun color-dissimilar-p (pixel1 pixel2)
  (let ((y1 (rgb-to-yuv pixel1))
	(y2 (rgb-to-yuv pixel2)))
    (or (> (abs (- (yuv-pixel-y y1) (yuv-pixel-y y2))) (/ 48 255))
	(> (abs (- (yuv-pixel-u y1) (yuv-pixel-u y2))) (/ 7 255))
	(> (abs (- (yuv-pixel-v y1) (yuv-pixel-v y2))) (/ 6 255)))))

(defun yuv-distance (pixel1 pixel2)
  (let ((y1 (rgb-to-yuv pixel1))
	(y2 (rgb-to-yuv pixel2)))
    (sqrt (+ 
	   (expt (- (yuv-pixel-y y1) (yuv-pixel-y y2)) 2)
	   (expt (- (yuv-pixel-u y1) (yuv-pixel-u y2)) 2)
	   (expt (- (yuv-pixel-v y1) (yuv-pixel-v y2)) 2)))))

(defun color-diff (pixel1 pixel2)
  (if (< (yuv-distance pixel1 pixel2) (/ 100 256))
      :LOW
      :HIGH))

;; XYMAPS
(defun get-dir-offset (direction)
  (case direction
    (tl '(-1 . 1))
    (t '(0 . 1))
    (tr '(1 . 1))
    (l '(-1 . 0))
    (r '(1 . 0))
    (bl '(-1 . -1))
    (b '(0 . -1))
    (br '(1 . -1))))
(defun get-inv-direction (direction)
  (case direction
    (tl 'br)
    (t 'b)
    (tr 'bl)
    (l 'r)
    (r 'l)
    (bl 'tr)
    (b 't)
    (br 'tl)))

(defun xymap-width (xymap)
  (array-dimension xymap 0))

(defun xymap-height (xymap)
  (array-dimension xymap 1))

(defun get-xy-item (xymap x y)
  (aref xymap x y))

(defun valid-directions (xymap x y)
  (let ((directions '()))
    (when (< x (1- (xymap-width xymap))) (setf directions (cons 'r directions)))
    (when (> x 0) (setf directions (cons 'l directions)))
    (when (< y (1- (xymap-height xymap))) (setf directions (cons 't directions)))
    (when (> y 0) (setf directions (cons 'b directions)))
    (when (and  (< x (1- (xymap-width xymap))) (< y (1- (xymap-height xymap)))) (setf directions (cons 'tr directions)))
    (when (and (> x 0) (> y 0)) (setf directions (cons 'bl directions)))
    (when (and (> x 0) (< y (1- (xymap-height xymap)))) (setf directions (cons 'tl directions)))
    (when (and (< x (1- (xymap-width xymap))) (> y 0)) (setf directions (cons 'br directions)))
    directions))



(defun valid-top-right-directions (xymap x y)
  (intersection (valid-directions xymap x y) '(tl t tr r)))

(defun valid-top-diagonal-directions (xymap x y)
  (intersection (valid-directions xymap x y) '(tl tr)))

(defun valid-direction-p (xymap x y direction)
  (find direction (valid-directions xymap x y)))

(defun get-xy-item-on-direction (xymap x y direction)
  (when (valid-direction-p xymap x y direction)
    (let ((offset (get-dir-offset direction)))
      (get-xy-item xymap (+ x (car offset)) (+ y (cdr offset))))))

(defun t-pixel-p (xymap y)
  (= y (1- (xymap-height xymap))))

(defun r-pixel-p (xymap x)
  (= x (1- (xymap-width xymap))))

(defun b-pixel-p (y)
  (zerop y)
  )

(defun l-pixel-p (x)
  (zerop x))

(defun tr-pixel-p (xymap x y)
  (and (t-pixel-p xymap y) (r-pixel-p xymap x)))

(defun tl-pixel-p (xymap x y)
  (and (l-pixel-p x) (t-pixel-p xymap y)))

(defun bl-pixel-p (x y)
  (and (b-pixel-p y) (l-pixel-p x)))

(defun br-pixel-p (xymap x y)
  (and (b-pixel-p y) (r-pixel-p xymap x))
  )
;; PIXMAPS
(defun make-pixmap (width height)
  (make-array `(,width ,height)))

(defun get-pixel (pixmap x y)
  (get-xy-item pixmap x y))

(defun set-pixel (pixmap x y pixel)
  (setf (aref pixmap x y) pixel))

(defun read-p6 (file)
  "read a binary ppm pixmap file"
  (let ((signature)
	(width)
	(height)
	(maxcolor)
	(file-position)
	(first-byte)
	)
    (with-open-file (stream file :direction :input )
      (setf signature (read stream))
      (setf width (read stream))
      (setf height (read stream))
      (setf maxcolor (read stream))
      (setf file-position (min (file-position stream) (- (file-length stream) (* 3 width height))))
      (when (or (not (eq signature 'P6)) (> maxcolor 255)) (error "we only support p6 formatted, 255 max color"))
      )
    (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
      (file-position stream file-position) 
      (loop 
	 with pixmap = (make-pixmap width height)
	 for row from 0 upto (1- height)
	      do (loop for column from 0 upto (1- width)
		      do (set-pixel pixmap column (1- (- height row)) ;; ppm starts from top-left corner
			    (make-pixel :r (/ (read-byte stream) maxcolor) :g (/ (read-byte stream) maxcolor)  :b (/ (read-byte stream) maxcolor) )))
	   finally (return pixmap)))))

(defun pixmap-to-png (pixmap pixel-size png-file)
  "draw a pixmap to a png file"
  (vecto:with-canvas (:width (* pixel-size (xymap-width pixmap))  :height (* pixel-size (xymap-height pixmap)))
    (loop for x from 0 upto (1- (xymap-width pixmap)) 
	 do (loop for y from 0 upto (1- (xymap-height pixmap)) 
		 do (let ((pixel (get-pixel pixmap x y)))
		      (vecto:set-rgb-fill (pixel-r pixel) (pixel-g pixel) (pixel-b pixel))
		      (vecto:set-rgb-stroke
		       (pixel-r pixel) (pixel-g pixel) (pixel-b pixel))
		      (vecto:rectangle (* x pixel-size) (* y pixel-size) pixel-size pixel-size)
		      (vecto:fill-and-stroke))))
    (vecto:save-png png-file)))

(defun same-color-in-area (pixmap x y)
  (loop 
     for i from (max 0 (- x 4))  upto (min (+ x 4) (1- (xymap-width pixmap)))  summing
       (loop for j from (max 0 (- y 4)) upto (min (+ y 4) (1- (xymap-height pixmap))) counting
	    (not (color-dissimilar-p (get-pixel pixmap x y) (get-pixel pixmap i j))))))

;;; vecinity
(defstruct pixel-vecinities tl t tr l r bl b br) ;top-left, top, top-right, center-left,...,bottom-left...

(defun make-pixmap-vecinities (width height default)
  "Create vecinities for a pixmap of widthxheight size. This is encoded as matrix of pixel-vecinities, where
   connections are initialised as default"
  (loop with vecinities = (make-array `(,width ,height))
     for row from 0 upto (1- height)
     do (loop for column from 0 upto (1- width)
	     do (setf (aref vecinities column row) 
		      (make-pixel-vecinities 
		       :tl (and (valid-direction-p vecinities column row 'tl) default) 
		       :t (and  (valid-direction-p vecinities column row 't) default) 
		       :tr (and  (valid-direction-p vecinities column row 'tr) default) 
		       :l (and  (valid-direction-p vecinities column row 'l) default) 
		       :r (and  (valid-direction-p vecinities column row 'r) default) 
		       :bl (and  (valid-direction-p vecinities column row 'bl) default) 
		       :b (and  (valid-direction-p vecinities column row 'b) default) 
		       :br (and  (valid-direction-p vecinities column row 'br) default) )))
     finally (return vecinities)))

(defun make-all-vecinities-for-pixmap (pixmap)
  (make-pixmap-vecinities (xymap-width pixmap) (xymap-height pixmap) t))

(defun get-pixel-vecinity (vecinities x y direction)
  (when (valid-direction-p vecinities x y direction)
    (let ((pv (get-xy-item vecinities x y)))
    (slot-value pv direction))))

(defun get-pixel-neighboors (vecinities x y)
  (loop for direction in '(tr t tl l r bl b br)
       appending (when (get-pixel-vecinity vecinities x y direction) (list (cons (+ x (car (get-dir-offset direction))) (+ y (cdr (get-dir-offset direction))))))))

(defun set-pixel-vecinity (vecinities x y direction value)
  (when (valid-direction-p vecinities x y direction)
    (let ((pv (get-xy-item vecinities x y))
	  (opv (get-xy-item-on-direction vecinities x y direction)))
      (setf (slot-value pv direction) value)
      (setf (slot-value opv (get-inv-direction direction)) value)
      )))

(defun eliminate-vecinities-dissimilar-color (pixmap vecinities)
  (loop for row from 0 upto (1- (xymap-height pixmap))
       do (loop for column from 0 upto (1- (xymap-width pixmap))
	       do (let ((pixel (get-pixel pixmap column row)))
		    (loop for direction in (valid-top-right-directions pixmap column row)
			 do (let ((candidate (get-xy-item-on-direction pixmap column row direction)))
			      (when (color-dissimilar-p pixel candidate)
				(set-pixel-vecinity vecinities column row direction nil)))))))
  vecinities)

(defun eliminate-redundant-diagonals (vecinities)
  (loop for row from 0 upto (- (xymap-height vecinities) 2)
       do (loop for column from 0 upto (- (xymap-width vecinities) 2)
	     do (when (and ;; all 4 connected 
		       (get-pixel-vecinity vecinities column row 't)
		       (get-pixel-vecinity vecinities column row 'r)
		       (get-pixel-vecinity vecinities (1+ column) (1+ row) 'l)
		       (get-pixel-vecinity vecinities (1+ column) (1+ row) 'b))
		  (set-pixel-vecinity vecinities column row 'tr nil)
		  (set-pixel-vecinity vecinities column (1+ row) 'br nil))))
  vecinities)

(defun find-crossings (vecinities)
  (loop for row from 0 upto (- (xymap-height vecinities) 2)
       appending (loop for column from 0 upto (- (xymap-width vecinities) 2)
		    when (and (get-pixel-vecinity vecinities column row 'tr) 
			      (get-pixel-vecinity vecinities column (1+ row) 'br)
			      (cons column row)) collecting it)))
;;; heuristics
(defun calculate-valence-2-curve (vecinities x1 y1 x2 y2)
  (let ((nodes (loop with head = (cons x1 y1)
		  with queue = (cons x2 y2)
		  with nodes = (list head queue)
		  do (let* ((head-next-candidates (set-difference (get-pixel-neighboors vecinities (car head) (cdr head)) nodes :test #'equal))
			    (head-next (when (= 1 (length head-next-candidates)) (car head-next-candidates))))
		       (cond (head-next
			      (setf nodes (cons head-next nodes))
			      (setf head head-next))
			     (t (return nodes)))))))
    (loop with queue = (cons x2 y2)
       do (let* ((queue-next-candidates (set-difference (get-pixel-neighboors vecinities (car queue) (cdr queue)) nodes :test #'equal))
		(queue-next (when (= 1 (length queue-next-candidates)) (car queue-next-candidates))))
	    (cond (queue-next
		   (setf nodes (append nodes (list queue-next)))
		   (setf queue queue-next))
		  (t (return nodes)))))))

(defun eliminate-crossings (pixmap vecinities crossings)
  (loop for crossing in crossings
     do (let* ((curve-length-1 (length (calculate-valence-2-curve vecinities (car crossing) (cdr crossing) (1+ (car crossing)) (1+ (cdr crossing)))))
	       (curve-length-2 (length (calculate-valence-2-curve vecinities (car crossing) (1+ (cdr crossing)) (1+ (car crossing)) (cdr crossing))))
	       (island-1 (or (= 1 (length (get-pixel-neighboors vecinities (car crossing) (cdr crossing))))
			     (= 1 (length (get-pixel-neighboors vecinities (1+ (car crossing)) (1+ (cdr crossing)))))))
	       (island-2 (or (= 1 (length (get-pixel-neighboors vecinities (car crossing) (1+ (cdr crossing)))))
			     (= 1 (length (get-pixel-neighboors vecinities (1+ (car crossing)) (cdr crossing))))))
	       (same-color-1 (same-color-in-area pixmap (car crossing) (cdr crossing)))
	       (same-color-2 (same-color-in-area pixmap (1+ (car crossing)) (cdr crossing)))
	       (weight-1 (+ (/ (- curve-length-1 curve-length-2) 2) (if island-1 5 0) (/ (- same-color-2 same-color-1) 2)))
	       (weight-2 (+ (/ (- curve-length-2 curve-length-1) 2) (if island-2 5 0) (/ (- same-color-1 same-color-2) 2))))

	  (if (> weight-1 weight-2)
	      (set-pixel-vecinity vecinities (car crossing) (1+ (cdr crossing)) 'br nil)
	      (set-pixel-vecinity vecinities (car crossing) (cdr crossing) 'tr nil)
	      )
	  
	  ))
  vecinities
  )

(defun pixmap-vecinities-to-png (pixmap vecinities pixel-size png-file crossings)
  "draw a pixmap to a png file"
  (let ((half-pixel-size (/ pixel-size 2)))
    (vecto:with-canvas (:width (* pixel-size (xymap-width pixmap)) :height (* pixel-size (xymap-height pixmap)))
      (loop for x from 0 upto (1- (xymap-width pixmap)) 
	 do (loop for y from 0 upto (1- (xymap-height pixmap)) 
	       do (let ((pixel (get-pixel pixmap x y)))
		    (vecto:set-rgb-fill (pixel-r pixel) (pixel-g pixel) (pixel-b pixel))
		    (vecto:set-rgb-stroke
		     (pixel-r pixel) (pixel-g pixel) (pixel-b pixel))
		    (vecto:rectangle (* x pixel-size) (* y pixel-size) pixel-size pixel-size)
		    (vecto:fill-and-stroke))))
      (vecto:set-rgb-stroke 0 1 0)
      (vecto:set-line-width 1)
      (loop for x from 0 upto (1- (xymap-width pixmap)) 
	 do (loop for y from 0 upto (1- (xymap-height pixmap)) 
	       do (loop for direction in (valid-top-right-directions pixmap x y)
		    do (when (get-pixel-vecinity vecinities x y direction)
			 (let ((offset (get-dir-offset direction))
			       (center-x (+ half-pixel-size (* x pixel-size)))
			       (center-y (+ half-pixel-size (* y pixel-size))))
			   (vecto:move-to center-x center-y)
			   (vecto:line-to (+ center-x (* pixel-size (car offset)))
					  (+ center-y (* pixel-size (cdr offset))))
			   (vecto:stroke))))))
      (vecto:set-rgb-stroke 0 0 1)
      (loop for crossing in crossings
	   do (let ((center-x (+ half-pixel-size (* (car crossing) pixel-size)))
		 (center-y (+ half-pixel-size (* (cdr crossing) pixel-size)))
		 (center-x2 (+ half-pixel-size (* (car crossing) pixel-size)))
		 (center-y2 (+ half-pixel-size (* (1+ (cdr crossing)) pixel-size))))
	     (vecto:move-to center-x center-y)
	     (vecto:line-to (+ center-x pixel-size)
			    (+ center-y pixel-size))
	     (vecto:stroke)
	     (vecto:move-to center-x2 center-y2)
	     (vecto:line-to (+ center-x2 pixel-size)
			    (- center-y2 pixel-size))
	     (vecto:stroke)))
      (vecto:save-png png-file))))

;;; voronoi
(defstruct voronoi-point
  x y visible-next visible-prev)

(defun left-rotate (list)
  (append (cdr list) (list (car list))))



(defun calculate-voronoi (pixmap vecinities x y)
  (let ((path '()))
    ;; top right corner:
    (let ((tv (get-pixel-vecinity vecinities x y 't))
	  (contrast 
	   (and (not (t-pixel-p pixmap y)) 
		(color-diff (get-pixel pixmap x y) 
			    (get-xy-item-on-direction pixmap x y 't)))))
      (cond ((or (t-pixel-p vecinities y) (r-pixel-p vecinities x) 
		 (and (not (get-pixel-vecinity vecinities x y 'tr))
		      (not (get-pixel-vecinity vecinities x (1+ y) 'br))
		      )) 
	     (push (make-voronoi-point :x 4 :y 4 :visible-prev (and 
			   (not (t-pixel-p vecinities y))
			   (not tv)
			   contrast
			   )) path))
	    ((get-pixel-vecinity vecinities x y 'tr)
	     (push (make-voronoi-point :x 3 :y 5 :visible-prev (and (not tv) contrast)) path)
	     (push (make-voronoi-point :x 5 :y 3 :visible-prev nil) path))
	    ((get-pixel-vecinity vecinities x (1+ y) 'br)
	     (push (make-voronoi-point :x 3 :y 3 :visible-prev (and (not tv) contrast)) path))))
    ;; bottom right corner:
    (let ((rv (get-pixel-vecinity vecinities x y 'r))
	  (contrast 
	   (and (not (r-pixel-p pixmap x)) 
		(color-diff (get-pixel pixmap x y) 
			    (get-xy-item-on-direction pixmap x y 'r)))))
      (cond ((or (b-pixel-p y) (r-pixel-p vecinities x) 
		 (and (not (get-pixel-vecinity vecinities x y 'br))
		      (not (get-pixel-vecinity vecinities x (1- y) 'tr)))) 
	     (push (make-voronoi-point :x 4 :y 0 :visible-prev (and (not (r-pixel-p vecinities x))
								    (not rv)
								    contrast)) path))
	    ((get-pixel-vecinity vecinities x y 'br)
	     (push (make-voronoi-point :x 5 :y 1 :visible-prev (and (not rv) contrast)) path)
	     (push (make-voronoi-point :x 3 :y -1 :visible-prev nil) path))
	    ((get-pixel-vecinity vecinities x (1- y) 'tr)
	     (push (make-voronoi-point :x 3 :y 1 :visible-prev (and (not rv) contrast)) path))))
    ;; bottom left corner:
    (let ((bv (get-pixel-vecinity vecinities x y 'b))
	  (contrast 
	   (and (not (b-pixel-p y)) 
		(color-diff (get-pixel pixmap x y) 
			    (get-xy-item-on-direction pixmap x y 'b)))))
      (cond ((or (b-pixel-p y) (l-pixel-p x) 
	       (and (not (get-pixel-vecinity vecinities x y 'bl))
		    (not (get-pixel-vecinity vecinities x (1- y) 'tl))))
	   (push (make-voronoi-point :x 0 :y 0 :visible-prev (and (not (b-pixel-p y))
								  (not bv) contrast)) path))
	  ((get-pixel-vecinity vecinities x y 'bl)
	   (push (make-voronoi-point :x 1 :y -1 :visible-prev (and (not bv) contrast)) path)
	   (push (make-voronoi-point :x -1 :y  1 :visible-prev nil) path))
	  ((get-pixel-vecinity vecinities x (1- y) 'tl)
	   (push (make-voronoi-point :x 1 :y 1 :visible-prev (and (not bv) contrast)) path))
	  ))
    
    ;; top left corner:
    (let ((lv (get-pixel-vecinity vecinities x y 'l))
	  (contrast 
	   (and (not (l-pixel-p x)) 
		(color-diff (get-pixel pixmap x y) 
			    (get-xy-item-on-direction pixmap x y 'l)))))
      (cond ((or (t-pixel-p vecinities y) (l-pixel-p x) 
		 (and (not (get-pixel-vecinity vecinities x y 'tl))
		      (not (get-pixel-vecinity vecinities x (1+ y) 'bl))))
	     (push (make-voronoi-point :x 0 :y 4 :visible-prev (and (not (l-pixel-p x))
			       (not lv) contrast)) path))
	    ((get-pixel-vecinity vecinities x y 'tl)
	     (push (make-voronoi-point :x -1 :y 3 :visible-prev  (and (not lv) contrast)) path)
	     (push (make-voronoi-point :x 1 :y 5 :visible-prev nil) path))
	    ((get-pixel-vecinity vecinities x (1+ y) 'bl)
	     (push (make-voronoi-point :x 1 :y 3 :visible-prev (and (not lv) contrast)) path))
	    ))
    (reverse path)
    (let ((clockwise-path (reverse path)))
      (mapcar (lambda (x y) (setf (voronoi-point-visible-next x) 
				  (voronoi-point-visible-prev y))) 
	      clockwise-path (left-rotate clockwise-path))
      clockwise-path
      )))

(defun voronoi-point-to-x (pixel-size voronoi-point)
  (* (voronoi-point-x voronoi-point) (/ pixel-size 4)))

(defun voronoi-point-to-y (pixel-size voronoi-point)
  (* (voronoi-point-y voronoi-point) (/ pixel-size 4)))

(defun calculate-pixmap-voronoi (pixmap vecinities )
  (let ((width (xymap-width pixmap))
	(height (xymap-height pixmap)))
    (loop with voronoi = (make-array `(,width ,height))
       for y from 0 upto (1- height)
       do (loop for x from 0 upto (1- width)
	     do (setf (aref voronoi x y) 
		      (calculate-voronoi pixmap vecinities x y)))
       finally (return voronoi))))



(defun pixmap-to-voronoi-png (pixmap voronoi-matrix pixel-size png-file &optional (cell-border nil) (visible-edge nil) (distinguish-contrast nil) (catmull nil))
  "draw a pixmap to a png file"
  (vecto:with-canvas (:width (* pixel-size (xymap-width pixmap))  :height (* pixel-size (xymap-height pixmap)))
    (vecto:set-line-width 1)
    (loop for x from 0 upto (1- (xymap-width pixmap)) 
	 do (loop for y from 0 upto (1- (xymap-height pixmap)) 
		 do (let* ((pixel (get-pixel pixmap x y))
			   (voronoi (get-xy-item voronoi-matrix x y)))
		      (vecto:set-rgb-fill (pixel-r pixel) (pixel-g pixel) (pixel-b pixel))
		      (cond (cell-border
			     (vecto:set-rgb-stroke
			      0 0 0))
			    (t (vecto:set-rgb-stroke
			      (pixel-r pixel) (pixel-g pixel) (pixel-b pixel))))
		      ;;; Trace the voronoi cell:
		      (vecto:with-graphics-state
			(vecto:translate (* x pixel-size) (* y pixel-size))
			(vecto:move-to (voronoi-point-to-x pixel-size (car (last voronoi))) (voronoi-point-to-y pixel-size (car (last voronoi))))
			(loop for vp in voronoi
			     for i = 0 then (1+ i)
			     do 
			     (if 
			      (and catmull
				   (voronoi-point-visible-prev vp)
				   (= 2 (count-if #'identity (calculate-outside-nodes voronoi-matrix x y (prev-pos voronoi i)))))
			      (let ((exits (calculate-outside-nodes voronoi-matrix x y (prev-pos voronoi i)))) 
				(vecto::catmull-rom-to (voronoi-point-to-x pixel-size (first exits)) 
						       (voronoi-point-to-y pixel-size (first exits))
						       (voronoi-point-to-x pixel-size (second exits))
						       (voronoi-point-to-y pixel-size (second exits))
						       (voronoi-point-to-x pixel-size vp) (voronoi-point-to-y pixel-size vp)
						       ))
			      (vecto:line-to (voronoi-point-to-x pixel-size vp) (voronoi-point-to-y pixel-size vp)))
			     )
			(vecto:close-subpath)
			(vecto:fill-and-stroke)
			
			;; display the visible arcs
			(vecto:with-graphics-state  
			  (when visible-edge
			    (vecto:set-line-width 2)
			    (loop for vp in voronoi 
				 for nvp in (left-rotate voronoi) 
			       for i = 0 then (1+ i)
				 do (progn 
				      (when (and (voronoi-point-visible-next vp)
						 (= 2 (count-if #'identity (calculate-outside-nodes voronoi-matrix x y i)))
						 )
					
					(if (or (not distinguish-contrast) (eq (voronoi-point-visible-next vp) :HIGH))
					    (vecto:set-rgb-stroke
					     0 0 1)
					    (vecto:set-rgb-stroke
					     0 1 0))
					
					(vecto:move-to (voronoi-point-to-x pixel-size vp) (voronoi-point-to-y pixel-size vp))
					(if catmull 
					    (let ((exits (calculate-outside-nodes voronoi-matrix x y i))) 
					      (vecto::catmull-rom-to (voronoi-point-to-x pixel-size (first exits)) 
								     (voronoi-point-to-y pixel-size (first exits))
								     (voronoi-point-to-x pixel-size (second exits))
								     (voronoi-point-to-y pixel-size (second exits))
								     (voronoi-point-to-x pixel-size vp) (voronoi-point-to-y pixel-size vp)
								     ))
					    (vecto:line-to (voronoi-point-to-x pixel-size nvp) (voronoi-point-to-y pixel-size nvp)))
					(vecto:stroke))))
			    ))))))
    (vecto:save-png png-file)))

;;; splines
(defun prev-pos (list pos)
  (if (zerop pos)
      (1- (length list))
      (1- pos)))

(defun next-pos (list pos)
  (if (= pos (1- (length list)))
      0
      (1+ pos)))

(defun find-node-in-cell (cell x y)
  (position-if (lambda (node) (and (= x (voronoi-point-x node)) (= y (voronoi-point-y node)))) cell))



(defun calculate-exits-helper (voronoi xc yc x y direction xadj yadj)
  (let* ((cell (get-xy-item-on-direction voronoi xc yc direction))
	 (node-pos  (progn (find-node-in-cell cell (+ x xadj) (+ y yadj))))
	 (node-prev  (nth (prev-pos cell node-pos) cell))
	 (node-next (nth (next-pos cell node-pos) cell)))
    (list (make-voronoi-point :x (+ (* -1 xadj) (voronoi-point-x node-prev)) :y (+ (* -1 yadj) (voronoi-point-y node-prev)) 
			      :visible-prev (voronoi-point-visible-next node-prev))
	  (make-voronoi-point :x (+ (* -1 xadj) (voronoi-point-x node-next)) :y (+ (* -1 yadj) (voronoi-point-y node-next)) 
			      :visible-prev (voronoi-point-visible-prev node-next)))))




(defun calculate-exits (voronoi xc yc index)
  (let* ((cell (get-xy-item voronoi xc yc))
	 (node (nth index cell))
	 (x (voronoi-point-x node))
	 (y (voronoi-point-y node))
	 (previous (nth (prev-pos cell index) cell))
	 (next (nth (next-pos cell index) cell))
	 (exits `(,(make-voronoi-point :x (voronoi-point-x previous)
				     :y (voronoi-point-y previous)
				     :visible-prev (voronoi-point-visible-next previous)
				     )
		   ,(make-voronoi-point :x (voronoi-point-x next)
				     :y (voronoi-point-y next)
				     :visible-prev (voronoi-point-visible-prev next)
				     ))))
    

    ;; share points with top
    (when (and (> y 2) (> x -1) (< x 5) (not (t-pixel-p voronoi yc)))
      (setf exits (union (calculate-exits-helper voronoi xc yc x y 't 0 -4) exits :test #'equalp)))
    
    ; right
    (when (and (> x 2) (> y -1) (< y 5) (not (r-pixel-p voronoi xc)))
      (setf exits (union (calculate-exits-helper voronoi xc yc x y 'r -4 0) exits :test #'equalp))
      )
    ; bottom
    (when (and (< y 2)  (> x -1) (< x 5) (not (b-pixel-p yc)))
      (setf exits (union (calculate-exits-helper voronoi xc yc x y 'b 0 4) exits :test #'equalp))
      )
    ; left
    (when (and (< x 2) (> y -1) (< y 5) (not (l-pixel-p xc)))
      (setf exits (union (calculate-exits-helper voronoi xc yc x y 'l 4 0) exits :test #'equalp)) 
      )
    exits))

(defun choose-exit (exits)
  (let ((visible-exits
	 (mapcan (lambda (exit) (when (voronoi-point-visible-prev exit) (list exit))) exits)))
    (cond ((= 1 (length visible-exits)) (car visible-exits))
	  ((= 1 (count-if (lambda (exit) (eq (voronoi-point-visible-prev exit) :HIGH)) visible-exits)) (find-if (lambda (exit) (eq (voronoi-point-visible-prev exit) :HIGH)) visible-exits))
	  (t nil))))

(defun calculate-outside-nodes (voronoi x y index)
  "calculate the outside  nodes for a catmull spline, for a given arc 
defined by its first node clockwise in a voronoi cell"
  (let* ((cell (get-xy-item voronoi x y))
	 (node1 (nth index cell))
	 (next-index (next-pos cell index))
	 (node2 (nth next-index cell))
	 (exits-1 (remove-if (lambda (c) (and (= (voronoi-point-x node2) (voronoi-point-x c))
					      (= (voronoi-point-y node2) (voronoi-point-y c)))) (calculate-exits voronoi x y index)))
	 (exits-2 (remove-if (lambda (c) (and (= (voronoi-point-x node1) (voronoi-point-x c))
					      (= (voronoi-point-y node1) (voronoi-point-y c)))) (calculate-exits voronoi x y next-index)))
	 (final1 (choose-exit exits-1))
	 (final2 (choose-exit exits-2))
	 )
    (list final1 final2)))

;;; example:
(defun example0 () 
  (let* ((pixmap (read-p6 "capitanhead.pnm"))
	 (vecinities (eliminate-redundant-diagonals (eliminate-vecinities-dissimilar-color pixmap (make-all-vecinities-for-pixmap pixmap))))
	 (crossings (find-crossings vecinities)))
    (setf vecinities (eliminate-crossings pixmap vecinities crossings))
    (pixmap-vecinities-to-png pixmap vecinities 20 "test1.png" (find-crossings vecinities))
					;(length crossings)
    ))

;;catmull rom example:
(vecto:with-canvas (:width 100 :height 100)
  (vecto:move-to 25 25)
  (vecto::catmull-rom-to 75 25 25 75 25 25 25 75 75 75 75 25)
  (vecto:close-subpath)
  (vecto:stroke)
  (vecto:save-png "pepe.png"))


;;;example:
(defun create-example-pixmap ()
  (let ((pixmap (make-pixmap 5 5)))
    (loop for x from 0 upto 4 do (loop for y from 0 upto 4 do (set-pixel pixmap x y (make-pixel :r 0 :g 0 :b 0))))
    (loop for (x y) in '((0 0) (1 1) (2 2) (3 3) (4 4) (4 3) (4 2) (4 1) (4 0) (3 0)) do (set-pixel pixmap x y (make-pixel :r 0 :g 1 :b 0)))
    (loop for (x y) in '((2 3) (3 2) (4 1) (4 0)) do (set-pixel pixmap x y (make-pixel :r 1 :g 1 :b 1)))
    pixmap))

(defun example1 ()  
  (let ((pixmap (create-example-pixmap))
			  (vecinities nil)
			  (crossings nil))
					;first curve
		      (setf vecinities (eliminate-redundant-diagonals (eliminate-vecinities-dissimilar-color pixmap (make-all-vecinities-for-pixmap pixmap))))
		      (setf crossings (find-crossings vecinities))
		      (eliminate-crossings pixmap vecinities crossings)
		      (setf crossings (find-crossings vecinities))
		      (pixmap-vecinities-to-png pixmap vecinities 20 "test.png" crossings)
		      ))

(defun example2 () 
  (let ((pixmap (create-example-pixmap))
	(vecinities nil))
					;first curve
    (setf vecinities (eliminate-redundant-diagonals 
		      (eliminate-vecinities-dissimilar-color pixmap (make-all-vecinities-for-pixmap pixmap))))
    (eliminate-crossings pixmap vecinities (find-crossings vecinities))
    (print vecinities)
    (pixmap-to-voronoi-png pixmap (calculate-pixmap-voronoi pixmap vecinities) vecinities 20 "test.png")
    ))

(defun example3 (pixel-size)  
  (let ((pixmap (read-p6 "capitanbody.pnm"))
	(vecinities nil)
	(voronoi nil)
	(crossings nil))
					;first curve
    (setf vecinities (eliminate-redundant-diagonals (eliminate-vecinities-dissimilar-color pixmap (make-all-vecinities-for-pixmap pixmap))))
    (setf crossings (find-crossings vecinities))
    (setf vecinities (eliminate-crossings pixmap vecinities crossings))
    (setf voronoi (calculate-pixmap-voronoi pixmap vecinities))
    ;(pixmap-vecinities-to-png pixmap vecinities pixel-size "test1.png" crossings)
    (pixmap-to-png pixmap pixel-size "test0.png")
    ;(pixmap-to-voronoi-png pixmap voronoi pixel-size "test2.png" t nil)
    ;(pixmap-to-voronoi-png pixmap voronoi  pixel-size "test3.png")
;    (pixmap-to-voronoi-png pixmap voronoi  pixel-size "test4.png" nil nil nil t)
    (pixmap-to-voronoi-png pixmap voronoi pixel-size "test5.png" nil nil nil t)
		      1)) 

(example3 8)

