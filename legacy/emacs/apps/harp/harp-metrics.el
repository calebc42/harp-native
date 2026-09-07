;;; harp-metrics.el --- Harp metrics view -*- lexical-binding: t; -*-

;; The Metrics tab: each metric definition as a card with its unit and
;; healthy range, plus a line chart of the readings harvested from journal
;; entries and documents (the #id(value) tokens).  Read-only for now.
;; Mirrors harp-kmp's MetricsView / MetricsPlot.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'harp-ui)

(defun harp-metrics--bound (n)
  "Format numeric bound N compactly, or nil."
  (when (numberp n)
    (number-to-string n)))

(defun harp-metrics--range-label (range)
  "A \"lo–hi\" label for a (LO . HI) RANGE cons, honouring nil bounds."
  (let ((lo (harp-metrics--bound (car range)))
        (hi (harp-metrics--bound (cdr range))))
    (cond ((and lo hi) (format "%s–%s" lo hi))
          (lo (format "≥ %s" lo))
          (hi (format "≤ %s" hi))
          (t nil))))

(defun harp-metrics--chart (metric values)
  "A line chart node for METRIC's VALUES, or nil when there are none."
  (when (and values (jetpacs-node-supported-p "chart"))
    (let ((points (mapcar (lambda (mv) (alist-get 'value mv)) values))
          (range (alist-get 'range metric)))
      (jetpacs-chart
       (list (jetpacs-chart-series points :label (alist-get 'name metric)))
       :kind "line"
       :height 160
       :y-range (when (and (numberp (car range)) (numberp (cdr range)))
                  (list (car range) (cdr range)))
       :summary (format "%s readings" (alist-get 'name metric))))))

(defun harp-metrics--card (metric profile)
  "A card for METRIC within PROFILE: header, healthy range, chart."
  (let* ((values (harp-profile-metric-values profile (alist-get 'id metric)))
         (latest (car (last values)))
         (healthy (harp-metrics--range-label (alist-get 'healthy-range metric)))
         (chart (harp-metrics--chart metric values)))
    (jetpacs-card
     (list (apply #'jetpacs-column
                  (delq nil
                        (list
                         (apply #'jetpacs-row
                                (append
                                 (delq nil
                                       (list
                                        (jetpacs-box
                                         (list (jetpacs-text (alist-get 'name metric) 'title))
                                         :weight 1)
                                        (when latest
                                          (jetpacs-text
                                           (format "%s %s"
                                                   (alist-get 'value latest)
                                                   (alist-get 'unit metric))
                                           'headline))))
                                 '(:align "center")))
                         (when healthy
                           (jetpacs-text (format "Healthy: %s %s"
                                                 healthy (alist-get 'unit metric))
                                         'caption))
                         (harp-ui-tag-chips (alist-get 'tags metric))
                         (or chart
                             (unless values
                               (jetpacs-text "No readings yet." 'caption)))))))
     :on-tap (jetpacs-action "harp.form.open"
                             :args `((kind . "metric") (id . ,(alist-get 'id metric)))
                             :when-offline "drop"))))

(defun harp-metrics--view (snackbar)
  "The Metrics screen for the current profile."
  (jetpacs-shell-tab-view
   "harp.metrics"
   (let ((profile (harp-ui-current)))
     (if (null profile)
         (harp-ui-no-profile-body)
       (let ((metrics (alist-get 'metrics profile)))
         (if metrics
             (apply #'jetpacs-lazy-column
                    (mapcar (lambda (m) (harp-metrics--card m profile)) metrics))
           (jetpacs-empty-state
            :icon "monitoring"
            :title "No metrics"
            :caption "Define metrics in the profile's Metadata section.")))))
   :snackbar snackbar))

(with-jetpacs-owner "harp"
  (jetpacs-shell-define-view "harp.metrics"
    :builder #'harp-metrics--view
    :tab '(:icon "monitoring" :label "Metrics")
    :when #'harp-ui-profile-open-p
    :order 30))

(provide 'harp-metrics)
;;; harp-metrics.el ends here
