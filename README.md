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

(define path
  (with-path ()
    (forward 5)
    (turn (* -1/2 pi))
    (forward 5)
    (turn (* -1/4 pi))
    (forward (* 5/2 (sqrt 2)))
    (turn (* -1/2 pi))
    (forward (* 5/2 (sqrt 2)))
    (turn (* -1/4 pi))
    (forward 5)
    (turn (* -3/4 pi))
    (forward (* 5 (sqrt 2)))
    (turn (* -3/4 pi))
    (forward 5)
    (turn (* -3/4 pi))
    (forward (* 5 (sqrt 2)))))

(define image
  (make-image path))

(get-bitmap image 400)
```

## See Also

- [Pict: Functional Pictures](https://docs.racket-lang.org/pict/)
- [Racket Drawing Toolkit](https://docs.racket-lang.org/draw/)
- [Turtle Graphics](https://docs.racket-lang.org/turtles/index.html)
	
## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0)

