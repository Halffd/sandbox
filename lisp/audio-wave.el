(require 'svg)

;; Variables to manage the process and state
(defvar audio-wave-process nil "The process capturing and processing audio.")
(defvar audio-wave-points (make-list 800 0.0) "List of amplitude values for the wave.")
(defvar audio-wave-buffer "*audio-wave*" "Buffer to display the audio wave SVG.")

;; Start the audio visualization
(defun start-audio-wave ()
  "Start capturing microphone input and visualizing it as a wave."
  (interactive)
  (when audio-wave-process
    (error "Audio wave process is already running"))
  (setq audio-wave-process
        (start-process "audio-wave" (get-buffer-create "*audio-wave-output*")
                       "sh" "-c"
                       "arecord -f S16_LE -r 44100 -c 1 -t raw | sox -t raw -r 44100 -e signed -b 16 -c 1 - -t dat - rate 1000"))
  (set-process-filter audio-wave-process 'audio-wave-filter)
  (set-process-sentinel audio-wave-process 'audio-wave-sentinel)
  (setq audio-wave-points (make-list 800 0.0))
  (message "Audio wave visualization started"))

;; Stop the audio visualization
(defun stop-audio-wave ()
  "Stop the audio wave visualization."
  (interactive)
  (when audio-wave-process
    (delete-process audio-wave-process)
    (setq audio-wave-process nil)
    (message "Audio wave visualization stopped")))

;; Process filter to handle audio data
(defun audio-wave-filter (proc output)
  "Process output from the audio capture, updating the wave visualization."
  (with-current-buffer (process-buffer proc)
    (insert output)
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (let ((line (buffer-substring (point-min) (1- (point)))))
        (delete-region (point-min) (point))
        (when (and (not (string-prefix-p ";" line)) (string-match "\\S-" line))
          (let* ((parts (split-string line))
                 (amplitude (string-to-number (nth 1 parts))))
            (setq audio-wave-points (append (cdr audio-wave-points) (list amplitude)))
            (update-audio-wave-svg)))))))

;; Update the SVG visualization
(defun update-audio-wave-svg ()
  "Generate and display the SVG wave based on current amplitude data."
  (with-current-buffer (get-buffer-create audio-wave-buffer)
    (erase-buffer)
    (let* ((svg (svg-create 800 400))
           (points (mapcar (lambda (i amp) (list i (- 200 (* amp 200))))
                           (number-sequence 0 799)
                           audio-wave-points)))
      (svg-polyline svg points :stroke "black" :stroke-width 2)
      (insert-image (svg-image svg)))))

;; Sentinel to handle process termination
(defun audio-wave-sentinel (proc event)
  "Handle the termination of the audio process."
  (when (string-match "finished" event)
    (setq audio-wave-process nil)
    (message "Audio wave process ended")))
