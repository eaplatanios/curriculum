Competence-based Curriculum Learning
====================================

This repository provides code for computing the sentence
scores used for the curriculum learning method presented
in [Competence-based Curriculum Learning for Neural Machine
Translation](https://arxiv.org/abs/1903.09848).

A simple command-line interface is provided that uses a
default setup. Let us assume there is a folder called
`example` which contains text files where each line
corresponds to a sentence that has already been tokenized
(and tokens are separated by spaces). Then, the following
command can be used to compute difficulty scores for these
sentences (this command can actually be run from the root
folder of this repository as there is a provided `example`
directory):
```bash
sbt "run example sentence-rarity"
```
The `sentence-rarity` option specifies which scoring
function to use. Valid options are:

  - `sentence-length`
  - `sentence-rarity`
  - `sentence-rarity-product-pooling`
  - `sentence-rarity-min-pooling`
  - `sentence-rarity-max-pooling`
  - `sentence-rarity-mean-pooling`

The above command will create two directories inside the
provided directory (i.e., `example`):

  - `sentence-scores`: Contains sentence-level scores for
    each file in `example` (note that the example
    directory is not explored recursively). These are all
    sentence-level scores that were computed while 
    computing the final CDF scores specified in the paper.
    The CDF scores are stored in files whose name ends
    with `.histogram.cdf.score` and each line correponds
    to a sentence in the corresponding file in `example`.
  - `summary-scores`: Contains all summary scores that
    were computed while computing the final CDF scores
    specified in the paper. Each summary score can have
    its own format but typically you would see one score
    file containing word counts over all text files and
    one containing a serialized form of a histogram over
    these counts. You should not generally need to use
    any of these files directly.
The code provides more functionality than the command-line
interface and it is generally easy to walk through. The
main entrypoint for computing scores is the `Score.compute`
function in the `Score.scala` file.
