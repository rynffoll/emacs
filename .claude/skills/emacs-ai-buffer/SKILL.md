---
name: emacs-ai-buffer
description: Show something to the user in a buffer of the running Emacs - a live dashboard of a running process (reindex, deploy, queue lag) or a one-off report (benchmarks), as SVG cards, tables or text. Use when asked to show, visualize or track something in Emacs or a buffer, or for a dashboard or progress view, in any language. Builds on the `ai-buffer` library of the user's Emacs config.
---

# Emacs AI buffer

The library `ai-buffer`, on the `load-path` of the running Emacs, does
the plumbing; you write the data side and a render function.  Read its
Commentary and docstrings first: `(locate-library "ai-buffer")`.

## 1. Which kind

Tell from the request; ask only if it is unclear:

- **Process dashboard**: data changes while the user watches; a
  collector feeds it, the buffer refreshes on a timer.
- **One-off display**: fixed data (benchmark results); no timer, no
  collector.

## 2. The buffer

Make it only with `ai-buffer-display`: its `*ai: TITLE*` name tells the
user it is agent-made.  To change anything, call it again; add no
refresh command, key, timer, title or status line of your own.

A dashboard: one `.el` file in the session scratchpad, loaded with
`(load FILE nil t)`, and again after an Emacs restart:

```elisp
;;; -*- lexical-binding: t; -*-
(require 'ai-buffer)
(require 'svg)

(defun ai-reindex--render (state)
  "Insert the contents for STATE (alist or nil)."
  ...)

(ai-buffer-display "reindex" #'ai-reindex--render
                   :state-file "/…/scratchpad/reindex/state.json"
                   :interval 5)
```

`:interval` 5–10 s, not shorter than the collector's period: each render
of a large SVG allocates.  STATE is nil until the collector writes the
file: render a placeholder, never signal.

A one-off display can go inline:

```sh
timeout 10 emacsclient -e '(ai-buffer-display "bench" (lambda (_) (insert …)))'
```

Always:

- Probe the live Emacs only with bounded forms under `timeout`; a hang
  needs SIGUSR2 (see emacs-unfreeze).
- Never kill or touch buffers you did not create.
- No xwidget-webkit: it can crash the user's Emacs.

## 3. Rendering

Window sizes, faces, fonts and vtable widths are those of the buffer's
window and its frame.  Mix freely:

- **SVG** (cards, progress): only in a graphic frame; otherwise insert a
  text fallback.

  ```elisp
  (defun ai-reindex--render (state)
    (if (not (display-images-p))
        (insert (propertize "Needs a graphic frame." 'face 'shadow))
      (let ((svg (svg-create (window-body-width nil t)
                             (window-body-height nil t))))
        ...
        (insert-image (svg-image svg :ascent 'center)))))
  ```

- **Table**: `make-vtable` (sortable columns, keys on rows); it
  inserts at point itself, so no `vtable-insert`.  Each refresh builds
  it anew, so a sort the user picked resets; for a live table keep it
  short or sorted as the user wants it by default.
- **Text**: lines with faces; `insert-text-button` or `buttonize` for
  actions and `file:line` links.

Text and tables also work in a TTY frame.

### Layout

- **One screen if possible**: aim for everything to fit the window body.
  Summarize, aggregate, show top-N with a count of the rest; drop what
  the user will not act on.  Most important first.  When it cannot fit
  (a long report), scrolling is fine.
- **Size-adaptive**: `window-body-width` and `window-body-height`,
  PIXELWISE for SVG, in columns and lines for text and tables.  Have a
  narrow layout (truncate, drop secondary columns, wrap) and a minimum
  width; neither cramped nor sparse.
- Put each detail next to what it describes, e.g. a source's staleness
  next to its value.
- Times and sizes: `iso8601-parse`, `format-seconds`,
  `file-size-human-readable`.

### Colors and font: from faces, never constants

- `(ai-buffer-color FACE)`: `default` for text, `shadow` muted, `link`
  accent, `success`/`warning`/`error` for statuses; `(ai-buffer-color
  'default :background)` for the background.
- `(ai-buffer-shade PERCENT)` for cards (6), borders (15) and progress
  tracks (25).
- Font: `(face-attribute 'default :family)`; size in pixels from
  `(aref (font-info (face-font 'default)) 2)`, not `frame-char-height`
  (the line height).
- Read colors in the render function, not once at load: after a theme
  change the next refresh (the timer or `g`) picks them up.

## 4. Collector (dashboard only)

A separate process, in any language, that the buffer only reads.  It
lives in the session scratchpad, runs in the background, survives Emacs
restarts and writes its PID to `collector.pid` next to the state file;
stop it when the task is done or the user asks.

- Writes **one JSON file** atomically (`state.json.tmp`, then rename),
  else the view reads half a file.
- When a source fails, keeps its last value with its age, marked stale.

## 5. Verify

After each change, before reporting:

- Measure the contents against the window that shows the buffer:

  ```elisp
  (let ((w (get-buffer-window "*ai: TITLE*" t)))
    (list (window-text-pixel-size w nil nil t)
          (window-body-width w t) (window-body-height w t)))
  ```

  A width over the body width means a wrapped line, table rows too: fix
  it.  A height over it is fine only for a long report.
- Save the SVG the buffer shows, `(image-property (get-text-property POS
  'display) :data)`, to `out.svg`, render it with `rsvg-convert -w WIDTH
  -o out.png out.svg` at the real window width (`screencapture` has no
  permission) and look at it: no overlaps, no clipping.
- Dashboard: stop the collector or break a source once; the view must
  show stale data, not fail.
