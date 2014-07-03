;;; compile with cv.scm and utils.scm
(c-declare "#pragma warning( disable : 4101 4102 )")
(include "cv-blackjack/cv.sch")

(define (slideshow file-list)
  (if (pair? file-list)
      (let ((file-name (car file-list)))
        (display file-name)
        (newline)
        (cv:show-image "slideshow" (cv:load-image file-name))
        (if (!= (cv:wait-key 0) 27)
            (slideshow (cdr file-list))))))

(define (show-video)
  (let ((cap (cv:capture-from-cam 0)))
    (define (display-frames)
      (cv:show-image "frame" (cv:query-frame cap))
      (if (!= (cv:wait-key 20) 27)
          (display-frames)))
    (display-frames)
    (cv:release-capture cap)))

(define (show-video-with-reflections)
  (cv:with-capture cap
    (let ((first-frame (cv:query-frame cap)))
      (using-cv:images ((buf1 (cv:clone-image first-frame))
                        (buf2 (cv:clone-image first-frame)))
          (cv:run-videostream-transformer cap frame
            (cv:flip frame buf1 0)
            (cv:add-weighted frame 0.8 buf1 0.2 0.0 buf2)
            buf2)))))

(define (show-video-with-cvt)
  (cv:with-capture cap
    (let ((first-frame (cv:query-frame cap)))
      (using-cv:images ((buf1 (cv:new-image-based-on first-frame depth: cv:IPL_DEPTH_8U channels: 1))
                        (buf2 (cv:clone-image first-frame)))
          (cv:run-videostream-transformer cap frame
            (cv:cvt-color frame buf1 cv:RGB2GRAY)
            ;;(cv:add-weighted frame 0.8 buf1 0.2 0.0 buf2)
            buf1)))))

(define (show-bgproc alpha)
  (cv:with-capture cap
    (let ((first-frame (cv:query-frame cap)))
      (using-cv:images ((bg-frame (cv:clone-image first-frame)))
        (cv:run-videostream-transformer cap frame
          (cv:add-weighted frame alpha bg-frame (- 1.0 alpha) 0.0 bg-frame)
          bg-frame)))))

(define (show-bgproc-morph alpha wing)
  (cv:with-capture cap
    (let ((first-frame (cv:query-frame cap)))
      (using ((struct-elem (cv:rect-structuring-element wing wing) cv:release-structuring-element))
        (using-cv:images ((bg-frame (cv:clone-image first-frame))
                          (adf-frame (cv:clone-image first-frame)))
            (cv:run-videostream-transformer cap frame
              (cv:add-weighted frame alpha bg-frame (- 1.0 alpha) 0.0 bg-frame)
              (cv:abs-diff frame bg-frame adf-frame)
              (cv:dilate adf-frame adf-frame struct-elem 1)
              (cv:erode adf-frame adf-frame struct-elem 1)
              adf-frame))))))
            
; (slideshow (cdr (command-line)))

(using ((r (cv:rect.new) c-free))
       (println (cv:rect.set-width r 1457))
       (println r)
       (println (cv:rect.get-x r))
       (println (cv:rect.get-y r))
       (println (cv:rect.get-width r))
       (println (cv:rect.get-height r)))

(show-video)
(show-video-with-reflections)
(show-video-with-cvt)
(show-bgproc 0.01)
(show-bgproc-morph 0.01 10)