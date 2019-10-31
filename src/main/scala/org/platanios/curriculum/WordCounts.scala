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

import org.platanios.curriculum.utilities.{newReader, newWriter, TrieWordCounter}

import better.files._

import scala.collection.mutable
import scala.util.matching.Regex

/**
  * @author Emmanouil Antonios Platanios
  */
class WordCounts(val caseSensitive: Boolean = false) extends SummaryScore {
  protected val whitespaceRegex: Regex = "\\s+".r

  protected var counter: TrieWordCounter = TrieWordCounter()

  override def name: String = {
    if (caseSensitive) "cs-wc" else "wc"
  }

  override def processSentence(
    sentence: String,
    requiredValues: Seq[Float],
    requiredSummaries: Seq[SummaryScore]
  ): Unit = {
    whitespaceRegex.split(sentence).foreach(word => {
      if (caseSensitive)
        counter.insertWord(word)
      else
        counter.insertWord(word.toLowerCase)
    })
  }

  override def resetState(): Unit = {
    counter = TrieWordCounter()
  }

  override def saveStateToFile(file: File): Unit = {
    val writer = newWriter(file)
    for ((count, word) <- counter.words().toSeq.sortBy(-_._1).distinct)
      writer.write(s"$word\t$count\n")
    writer.flush()
    writer.close()
  }

  override def loadStateFromFile(file: File): Unit = {
    reset()
    newReader(file).lines().toAutoClosedIterator.foreach(line => {
      val lineParts = line.split('\t')
      counter.insertWordWithCount(word = lineParts(0), count = lineParts(1).toLong)
    })
  }

  def count(word: String): Long = {
    counter(word)
  }

  def totalCount: Long = {
    counter.totalCount
  }
}

object WordCounts {
  def apply(caseSensitive: Boolean = false): WordCounts = {
    new WordCounts(caseSensitive)
  }
}
