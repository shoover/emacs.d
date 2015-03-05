;; This started at https://gist.github.com/ieure/80638

(defun screen-unusable-height (&optional display)
  "Return the unusable height of the display, an estimate of the
height of the OS X (menu?) and Windows 7 taskbar."
  (cond ((eq window-system 'ns) 22)
        ((eq window-system 'w32) 40)
        (t 0)))

(defun screen-usable-height (&optional display)
  "Return the usable height of the display.
 
Some window-systems have portions of the screen which Emacs
cannot address. This function should return the height of the
screen, minus anything which is not usable."
  (- (display-pixel-height display)
     (screen-unusable-height)))

(defun screen-usable-width (&optional display)
  "Return the usable width of the display.
 
This works like `screen-usable-height', but for the width of the display."
  (display-pixel-width display))

(defun frame-box-get-center (w h cw ch)
  "Center a box inside another box.
 
Returns a list of `(TOP LEFT)' representing the centered position
of the box `(w h)' inside the box `(cw ch)'."
  (list (/ (- cw w) 2) (/ (- ch h) 2)))

(defun frame-get-center (frame)
  "Return the center position of FRAME on its display."
  (let ((disp (frame-parameter frame 'display)))
    (frame-box-get-center (frame-pixel-width frame) (frame-pixel-height frame)
                          (screen-usable-width disp) (screen-usable-height disp))))

(defun frame-get-selected-frame (&optional frame)
  (or (and (boundp 'frame) frame)
      (selected-frame)))

(defun frame-center (&optional frame)
  "Center a frame on the screen."
  (interactive)
  (apply 'set-frame-position
         (let* ((frame (frame-get-selected-frame frame))
                (center (frame-get-center frame)))
           `(,frame ,@center))))

(defun display-get-workarea-width ()
  "Get the work area pixel width of the main display."
  (nth 3 (assq 'workarea (car (display-monitor-attributes-list)))))

(defun display-get-workarea-height ()
  "Get the work area pixel height of the main display."
  (nth 4 (assq 'workarea (car (display-monitor-attributes-list)))))

(defun frame-bottom-right (&optional frame)
  "Position a frame at the bottom right of the screen."
  (interactive)
  ;; (-1,-1) used to work, but in Emacs 24.4 it seems negative numbers are
  ;; taken relative to the total size of all monitors, so we're back to doing
  ;; the math relative to the main monitor's size.
  (let ((frame (frame-get-selected-frame frame)))
    (set-frame-position frame
                        (- (display-get-workarea-width)
                           (frame-pixel-width frame)
                           16) ; no clue why this constant is needed
                        (- (display-get-workarea-height)
                           (frame-pixel-height frame)
                           60)))) ; no clue why this constant is needed

(provide 'frame-center)
