package it.polimi.genomics.GMQLSync

import java.io._

import org.slf4j.LoggerFactory

import scala.collection.immutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by Olga Gorlova on 08/07/2019.
  */
class VocabularyCount extends Serializable{
  private final val logger = LoggerFactory.getLogger(this.getClass)
  var vocabularyMap: Map[(String, String), Int] = new HashMap

  //key value count
  def addVocabulary(stream: InputStream): InputStream = {
    //piped into the output. The cache size is small in order to send quickly
    val pis = new PipedInputStream(1024)
    val pos = new PipedOutputStream(pis)

    //getting the vocabulary counts.
    val pis2 = new PipedInputStream(1024*1024)
    val pos2 = new PipedOutputStream(pis2)

    Future {
      Iterator
        .continually(stream.read)
        .takeWhile(-1 !=)
        .foreach { a =>
          pos.write(a)
          pos2.write(a)
        }
      pos2.close
      pos.close
    }
    Future {
      val br = new BufferedReader(new InputStreamReader(pis2))
      Iterator
        .continually(br.readLine())
        .takeWhile(null !=)
        .foreach { temp =>
        val list = temp.split("\\t")
        if (list.size == 2) {
          val tuple = (list.head, list.last)
          val count = vocabularyMap.get(tuple).getOrElse(0) + 1
          vocabularyMap = vocabularyMap + (tuple -> count)
        }
        else
          logger.warn(s"Cannot parse $temp -> ${list.mkString(", ")}")
      }
    }
    pis
  }

//  //key value count
  def addVocabulary(meta: String) = {

//    meta.split("\n").foreach { temp =>
//      val list = temp.trim.split("\\t").filter(f=> !f.isEmpty)
//      if (list.size == 2) {
//        val tuple = (list.head, list.last)
//        val count = vocabularyMap.get(tuple).getOrElse(0) + 1
//        vocabularyMap = vocabularyMap + (tuple -> count)
//      }
//      else
//        logger.warn(s"Cannot parse $temp -> ${list.mkString(", ")}")
//    }

    meta.split("\n").foreach { temp =>
      val list = temp.trim.split("\\t").filter(f=> !f.isEmpty)
      if (list.size == 3) {
        val tuple = (list(1), list(2))
        val count = vocabularyMap.get(tuple).getOrElse(0) + 1
        vocabularyMap = vocabularyMap + (tuple -> count)
      }
      else
        logger.warn(s"Cannot parse $temp -> ${list.mkString(", ")}")
    }
  }

  /**
    * stream the vocabularyMap as stream
    * @return
    */
  def getStream = {
    val pis = new PipedInputStream()
    val pos = new BufferedWriter(new OutputStreamWriter(new PipedOutputStream(pis)))
    Future {
      var tempKey: String = null
      //order by lowercase key and lowercase value.
      for (((key, value), count) <- vocabularyMap.toList.sortBy(x => (x._1._1.toLowerCase,x._1._2.toLowerCase))) {
        if (tempKey != key){//print first appearance the key
          pos.append(key)
          pos.newLine()
          tempKey = key
        }
        //print tab then value then count
        pos.append(s"\t$value\t$count")
        pos.newLine()
      }
      pos.close
    }
    pis
  }

}
