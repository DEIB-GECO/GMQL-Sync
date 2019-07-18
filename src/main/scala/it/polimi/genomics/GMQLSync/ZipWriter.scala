package it.polimi.genomics.GMQLSync

import java.io.{FileInputStream, FileOutputStream, IOException}
import java.nio.file.attribute.FileTime
import java.nio.file.{DirectoryStream, Files, Path, Paths}
import java.util.zip.{ZipEntry, ZipOutputStream}

import org.apache.log4j.{Level, Logger}

import scala.collection.JavaConversions._

/**
  * Created by Olga Gorlova on 09/07/2019.
  */
object ZipWriter {

  private val LOG = Logger.getLogger(this.getClass)

  var ZIP_DIR = "c:/Dev/temp/zipwrite"
  var OUTPUT_ZIP = "c:/dev/temp/output.zip"

  /**
    * This method creates the zip archive and then goes through
    * each file in the chosen directory, adding each one to the
    * archive. Note the use of the try with resource to avoid
    * any finally blocks.
    */
  def createZip(dirName: String): Unit = { // the directory to be zipped
    LOG.info(s"Zipping '$dirName' ...")
    val directory = Paths.get(dirName)
    ZIP_DIR = dirName
    OUTPUT_ZIP = s"$dirName.zip"
    // the zip file name that we will create
    val zipFileName = Paths.get(OUTPUT_ZIP).toFile
    // open the zip stream in a try resource block, no finally needed
    try {
      val zipStream = new ZipOutputStream(new FileOutputStream(zipFileName))
      try { // traverse every file in the selected directory and add them
        // to the zip file by calling addToZipFile(..)
        val dirStream: DirectoryStream[Path] = Files.newDirectoryStream(directory)
        for (path <- dirStream) yield {
          if (!path.toFile.isDirectory && !path.toFile.getName.startsWith("."))
            addToZipFile(path, zipStream, false)
          else{
            addToZipFile(path, zipStream, true)
            val subDirStream: DirectoryStream[Path] = Files.newDirectoryStream(path)
            for (subPath <- subDirStream) yield {
              if (!subPath.toFile.getName.startsWith("."))
              addToZipFile(subPath, zipStream, true)
            }
          }
        }
//        dirStream.forEach((path: Path) => addToZipFile(path, zipStream))
        LOG.info("Zip file created in " + directory.toFile.getPath)
      } catch {
        case e: IOException =>
          LOG.log(Level.FATAL, s"Error while zipping: '$dirName'.", e)
          throw new ZipParsingException(s"Error while zipping: '$dirName'.", e)
      } finally if (zipStream != null) zipStream.close()
    }
  }

  /**
    * Adds an extra file to the zip archive, copying in the created
    * date and a comment.
    *
    * @param file      file to be archived
    * @param zipStream archive to contain the file.
    */
  def addToZipFile(file: Path, zipStream: ZipOutputStream, subFolder : Boolean): Unit = {
    val inputFileName = file.toFile.getPath
    try {
      if (file.toFile().isDirectory) {
        val entry = new ZipEntry("files/")
        entry.setCreationTime(FileTime.fromMillis(file.toFile.lastModified))
        zipStream.putNextEntry(entry)
      }
      else {
        val inputStream = new FileInputStream(inputFileName)
        try { // create a new ZipEntry, which is basically another file
          // within the archive. We omit the path from the filename
          val entry = if (subFolder) new ZipEntry("files/"+file.toFile.getName) else new ZipEntry(file.toFile.getName)
          entry.setCreationTime(FileTime.fromMillis(file.toFile.lastModified))
          zipStream.putNextEntry(entry)
//          LOG.info("Generated new entry for: " + inputFileName)
          // Now we copy the existing file into the zip archive. To do
          // this we write into the zip stream, the call to putNextEntry
          // above prepared the stream, we now write the bytes for this
          // entry. For another source such as an in memory array, you'd
          // just change where you read the information from.
          val readBuffer = new Array[Byte](2048)
          var amountRead = 0
          var written = 0
          amountRead = inputStream.read(readBuffer)
          while (amountRead > 0) {
            zipStream.write(readBuffer, 0, amountRead)
            written += amountRead
            amountRead = inputStream.read(readBuffer)
          }
//          LOG.info("Stored " + written + " bytes to " + inputFileName)
        } catch {
          case e: IOException =>
            throw new ZipParsingException("Unable to process " + inputFileName, e)
        } finally if (inputStream != null) inputStream.close()
      }
    }
  }

}

/**
  * We want to let a checked exception escape from a lambda that does not
  * allow exceptions. The only way I can see of doing this is to wrap the
  * exception in a RuntimeException. This is a somewhat unfortunate side
  * effect of lambda's being based off of interfaces.
  */
class ZipParsingException(val reason: String, val inner: Exception) extends RuntimeException(reason, inner) {
}