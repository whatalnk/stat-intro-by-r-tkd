# 間違っていそうなとこ

第1版 第1刷（2017-01-27）

## 5章
* p.66 スクリプト 括弧が足りない

誤
```r
group <- factor(c(rep(1, 10), rep(2, 10))
```

正
```r
group <- factor(c(rep(1, 10), rep(2, 10)))
```

* p.68 演習問題5・3

問題文が途中で終わっているように見える．

問題文にはペアにしたと書いてあるが，解答は対応のないt検定になっている．

## 6章

* p.79 F値計算の部分，括弧の対応

誤
```r
for (i in 1:m) {
  SS.w <- SS.w + sum((d1[(n*(i-1)+1):(n*i))] - mean(d1[(n*(i-1) + 1):(n*i))]))^2)
  SS.b <- SS.b + sum(n * (mean(d1[(n*(i-1)+1):(n*i))]) - mean(d1))^2)
}
```

正
```r
for (i in 1:m) {
  SS.w <- SS.w + sum((d1[(n*(i-1)+1):(n*i)] - mean(d1[(n*(i-1) + 1):(n*i)]))^2)
  SS.b <- SS.b + sum(n * (mean(d1[(n*(i-1)+1):(n*i)]) - mean(d1))^2)
}
```
* テキスト表6.7 のデータは（ほぼ）配布ファイルの`table6-5.csv`
    * g1 と g2 を逆にするとテキストのデータになる
* 表6.8は `table6-6.csv`で，表6.9は `table6-7.csv`

## 7章

p.98

誤

```r
summary(aov(y ~ size + student + size:student))
```

正

```r
summary(aov(kaiteki ~ size + student + size:student))
```

## 9章
* p128 l13: 回帰直線への適合度が高い -> 低い
* p136 サンプルコード: age <- c() の最後の `+` が不要

## 10章
* p147 サンプルコードとその説明
    * `live` 列があるのに使わずに `1 - d$dead` としている．p148の出力は `d$live` になっている
* p155 Aakaike's information criterion -> Akaike's information criterion

## 11章
* p176 `glmer()` の結果で，Random effects の値（Variance，Std.Dev）がやってみると違った．
* `glmmML {glmmML}` はデフォルトではラプラス近似．`method="ghq"` を指定すると，ガウス・エルミート積分法を使用．
