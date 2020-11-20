# 行動データの計算論モデリング(6章)
* Original code: https://github.com/kkatahira/cmbd-book/tree/master/chapter6
* `generate_data.R`: データ生成(健常群と疾患群で学習率が異なる)
* `maximum_likelihood.R`: 最尤推定で学習率と逆温度を推定 -> t検定で学習率を比較
* `hierarchical_bayes.R`: 階層ベイズで学習率と逆温度を推定 -> 学習率の95%信頼区間を可視化し比較
* `smodel_qlearning_multiple_group.stan`: Stanのコード
