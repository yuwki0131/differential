# differential

## 概要

- Haskell入門ハンズオンもくもく会 continued. (作りきれなかったので)
- 簡単な多項式(x^2+3*x+2)をxで微分します。
- 負、括弧、x以外の変数等々には非対応。

先日、Haskell入門ハンズオンに行ってきたのですが、作りたいものが作りきれなかったので、
その続きで作ったものです。

## 入出力

```
~ $  stack exec differential-exe
x^2+3*x+2
2*x + 3
4*x^3+2*x^2+x+100
4*3*x^2 + 2*2*x + 1
4-2
(´・_・`) < そんな名前の式知らない！
exit
Bye! (´・_・`)/
```

## BNF

こんな式を想定 : 4*x^2 + 3x + 2

```
exp  ::= pexp eof
pexp ::= mexp | mexp "+" pexp   // addition
mexp ::= texp | texp "*" mexp   // multiplication
texp ::= "x" "^" n | "x" | n    // term
n    ::= <natural number>
```

## 今後の課題

- xでしか微分できないのと、非対応部分を追加すること。
- 今回作ったデータ構造が、ちょっと、適当すぎたかも知れない。。。
- 微分そのものよりも相変わらず、式の簡約(simplify)が難しい。。。
