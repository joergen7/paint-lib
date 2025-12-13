# paint-lib

A very simple paint library.

## Installation

    make install

## Example

```racket
#lang racket/base

(require
 racket/math
 paint-lib)

(define p
  (with-paint ()
    (walk 5)
    (turn (* -1/2 pi))
    (walk 5)
    (turn (* -1/4 pi))
    (walk (* 5/2 (sqrt 2)))
    (turn (* -1/2 pi))
    (walk (* 5/2 (sqrt 2)))
    (turn (* -1/4 pi))
    (walk 5)
    (turn (* -3/4 pi))
    (walk (* 5 (sqrt 2)))
    (turn (* -3/4 pi))
    (walk 5)
    (turn (* -3/4 pi))
    (walk (* 5 (sqrt 2)))))

(define image
  (make-image p))

(get-bitmap image 400)
```

## See Also

- [https://docs.racket-lang.org/pict/](Pict: Functional Pictures)
- [https://docs.racket-lang.org/draw/](Racket Drawing Toolkit)
- [https://docs.racket-lang.org/turtles/index.html](Turtle Graphics)
	
## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0)

