# Generert

Tests:

```
clj -Xtest
```

## ClojureScript

Build the ClojureScript bundle and open `cljs.html`:

```
clj -M:cljs
```

Then open `cljs.html` in a browser. The CLJS sketch uses the `#app` host div.
`cljs.html` pulls in `processing.js` from a CDN, which Quil needs for CLJS sketches.

GIT LFS is used.
https://docs.github.com/en/repositories/working-with-files/managing-large-files/configuring-git-large-file-storage

## Shortcut in Emacs

In my Cider configuration I have this.

```elisp
  (defun run-sketch ()
    (interactive)
    (cider-interactive-eval "(require 'template.dynamic)(reset! template.dynamic/draw-width w)(reset! template.dynamic/draw-height h)(template.dynamic/sketch #'draw)"))

  (keymap-set cider-mode-map "C-c C-å" 'run-sketch)
```

## Linting

Linted with [clj-kondo](https://github.com/clj-kondo/clj-kondo).

## Links

[Quil API](http://quil.info/api)

[Using Quil for artwork](https://tylerxhobbs.com/essays/2015/using-quil-for-artwork)

[Running processing without a display](https://github.com/processing/processing/wiki/Running-without-a-Display)

[Chaikin curves - Sighack](https://sighack.com/post/chaikin-curves)

[Voronoi and beyond](https://www.alanzucconi.com/2015/02/24/to-voronoi-and-beyond/)

## Create GIF

```
ffmpeg -framerate 10 -i circle-%03d.png -vf "scale=320:-1:flags=lanczos" -c:v gif output.gif
```

## Check out

Preset colors
https://github.com/thi-ng/color/blob/master/src/presets.org
