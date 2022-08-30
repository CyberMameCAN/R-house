# Statistics R Rビギナーのお勉強場所

ちょっとRが気になりだしたので、統計検定２級の勉強の足しになればと思い、勉強してみます。

## 参照

- [Rによる統計入門](https://htsuda.net/stats/)
- [R programming for beginners – statistic with R (t-test and linear regression) and dplyr and ggplot](https://www.youtube.com/watch?v=ANMuuq502rE)
- [高校生のためのRによる回帰分析](http://www.f.waseda.jp/takezawa/math/joho/regression.pdf)
- [ハンバーガー統計学にようこそ！](http://kogolab.chillout.jp/elearn/hamburger/index.html)

## 使い方

### Docker

    docker-compose build
    docker-compose up -d

### ブラウザでR Studio

    http://localhost:8787

## トラブルシュート

### semPlot

    > library(semPlot)
    Error: package or namespace load failed for ‘semPlot’ in dyn.load(file, DLLpath = DLLpath, ...):
    unable to load shared object '/usr/local/lib/R/site-library/igraph/libs/igraph.so':
    libglpk.so.40: cannot open shared object file: No such file or directory

Docker Imageを作り直すのが面倒だったので、とりあえずサーバに入って足りないパッケージをインストールした。

    $ docker container exec -it r_rstudio_1 bash

    # sudo apt update
    # apt install -y libglpk-dev
    