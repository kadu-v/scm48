# scm48

This repository is an implementation of [Scheme 48](https://ja.wikibooks.org/wiki/48時間でSchemeを書こう) for Hakskell excecise.

## How to build

```
git clone git@github.com:ikemorikazuki/scm48.git
cd scm48
stack build
```
## How to run 

- REPL
```
stack run
scm48> (define x 1)
1
scm48> (+ x 2)
3
scm48> (define plus (lambda (x y) (+ x y)))
(lambda ("x" "y")..)
scm48> (plus x 3)
4
```

- read file and output stderr
```
stack run ./sample/simple.scm
...
3
```