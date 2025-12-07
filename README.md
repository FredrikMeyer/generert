# Generert

Tests:

```
clj -Xtest
```

GIT LFS is used.
https://docs.github.com/en/repositories/working-with-files/managing-large-files/configuring-git-large-file-storage

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
