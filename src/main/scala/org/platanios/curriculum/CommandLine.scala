/* Copyright 2019, Emmanouil Antonios Platanios. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

package org.platanios.curriculum

import better.files._

object CommandLine {
  @throws[IllegalArgumentException]
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      throw new IllegalArgumentException(
        s"Expected two arguments (data directory and scoring function) but less were provided.")
    }
    val dataDir = File(args(0))
    if (!dataDir.isDirectory)
      throw new IllegalArgumentException(s"The provided data directory ($dataDir) is invalid.")
    val score: SentenceScore = args(1) match {
      case "sentence-length" => SentenceLength
      case "sentence-rarity" => SentenceRarity(SentenceRarity.ProductPooling)
      case "sentence-rarity-product-pooling" => SentenceRarity(SentenceRarity.ProductPooling)
      case "sentence-rarity-min-pooling" => SentenceRarity(SentenceRarity.MinPooling)
      case "sentence-rarity-max-pooling" => SentenceRarity(SentenceRarity.MaxPooling)
      case "sentence-rarity-mean-pooling" => SentenceRarity(SentenceRarity.MeanPooling)
      case scoreArg => throw new IllegalArgumentException(
        s"Invalid scoring method: '$scoreArg'.")
    }
    Score.compute(
      files = dataDir.children.filter(_.isRegularFile).toSeq,
      score = SentenceScoreHistogram(score, maxNumBins = 1000).cdfScore,
      scoresDir = dataDir,
      alwaysRecompute = true)
  }
}
