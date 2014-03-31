marokani
========

かにチャット

### 言語について

Haskellに色々と影響を受けている

#### 例

;で区切る /* */がコメント

    print "v('ω')v"; print "かに"; /* ∠( 'ω')／ */

:=で宣言 =で代入

    x := 42; x = x + 2; print (x+2)

::=は定数

    x ::= 0; x = 2 /* エラー */

lambdaとカリー化

    print (((\x y{x}) 2) 3)

演算子の優先順位は先頭の文字による

    print (1 - 2 * 3 + 4 * 5)

()でくくれば二項演算子が関数に

    print ((+) 2 3)

``でくくれば式が二項演算子に

    print (4 `\x y{x*y*y}` 5)

単項演算子

    print (- 42)

[]でくくれば単項演算子

    [$] := \x{print x}; $ 4

if

    print (if 1 < 2 then false else 5)

乱数 引数に意味は無い ()はfalseと同じ

    print (randInt ())

配列

    print ([1,2,3] ! 0)

範囲

    print [1,,5]

関数などは比較できないから常に偽

    print (print == print); print (print != print)

オブジェクト

    ob := { a := 42, c ::= "hello" }; print ob.c; ob.a = 0; print ob.a
