marokani
========

かにチャット

### 言語について
Haskellに色々と影響を受けている

    print "v('ω')v"; print "かに"; /* ;で区切る */
    x := 42; x = x + 2; print (x+2) /* :=で宣言 =で代入 */
    print ((\x{x+2}) 5) /* lambda */
    print (((\x y{x}) 2) 3) /* 全てカリー化される */
    print ((+) 2 3) /* ()でくくれば二項演算子が関数に */
    print (4 `\x y{x*y+1}` 5) /* ``でくくれば式が二項演算子に */
    print (- 42) /* 単項演算子 */
    [$] := \x{print x}; $ 4 /* []でくくれば単項演算子 */
    print (if 1 < 2 then false else 5) /* if */
    print (randInt ()) /* 乱数 引数に意味は無い ()はfalseと同じ */
    print ([1,2,3] ! 0) /* 配列 */
    print [1,,5]
    print (print == print) /* 関数は比較できないから常に偽 */