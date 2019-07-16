package it.polimi.genomics.GMQLSync

import java.io.File

import it.polimi.genomics.repository.FSRepository.FS_Utilities
import it.polimi.genomics.repository.FSRepository.datasets.GMQLDataSetXML
import it.polimi.genomics.repository.{GMQLRepository, Utilities}
import org.apache.commons.io.FileUtils
import org.apache.log4j.{Level, Logger}

import scala.reflect.io.Directory
import scala.sys.process.Process
import com.github.fracpete.rsync4j.RSync
import com.github.fracpete.processoutput4j.output.CollectingProcessOutput

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
/**
  * Created by Olga Gorlova on 08/07/2019.
  */
object DSBackup {

  private final val logger = Logger.getLogger(this.getClass)
  Utilities.confFolder = new File("../gmql_conf/").getAbsolutePath /*"/Users/olha/IdeaProjects/GMQL-Sync/src/main/resources/gmql_conf"*/
  val ut: Utilities = Utilities()
  val repository: GMQLRepository = ut.getRepository()
  var username: String = "public"
  var localDir = ""

  val usage ="\n"+
    "\t.........................................................\n" +
      "\t.........................................................\n" +
      "\t...........GMQL Dataset Backup help..................\n" +
      "\t.........................................................\n" +
      "\t.........................................................\n\n" +
      "\tDSBackup $COMMAND\n\n" +
      "\tAllowed Commands are :\n\n" +
      "\tZip DS_NAME LOCAL_DIRECTORY\n\n" +
      "\t\tThis command zip a dataset named DS_NAME to local folder.\n" +
      "\t\tLOCAL_DIRECTORY is the full path to the local location. \n\n" +
      "\tZipALL DS_LIST_FILE LOCAL_DIRECTORY\n\n" +
      "\t\tThis command zip all datasets listed in the file DS_LIST_FILE to local directory.\n\n" +
      "\tBackupTo DS_NAME TMP_DIR BACKUP_PATH\n\n" +
      "\t\tThis command backup a dataset named DS_NAME to backup folder.\n" +
      "\t\tTMP_DIR is the full path to the temporary local location. \n" +
      "\t\tBACKUP_PATH is the full path to the backup location on the remote server, should be in the following format: \n" +
      "\t\t\tBACKUP_SERVER:BACKUP_DIRECTORY. \n\n" +
      "\tBackupALL DS_LIST_FILE TMP_DIR BACKUP_PATH\n\n" +
      "\t\tThis command backup all datasets from the DS_LIST_FILE to backup folder.\n" +
      "\t\tTMP_DIR is the full path to the temporary local location. \n" +
      "\t\tBACKUP_PATH is the full path to the backup location on the remote server, should be in the following format: \n" +
      "\t\t\tBACKUP_SERVER:BACKUP_DIRECTORY. \n\n" +
      "\tExportDSToLocal DS_NAME LOCAL_DIRECTORY\n\n" +
      "\t\tThis command export all the samples of DS_NAME to local folder.\n" +
      "\t\tThe samples will be copied with its metadata.\n" +
      "\t\tThe 'info.txt' and 'vocabulary.txt' files will also be added.\n" +
      "\t\tLOCAL_DIRECTORY is the full path to the local location. \n\n" +
      "\tDeleteDS DS_NAME LOCAL_DIRECTORY\n\n" +
      "\t\tTo Delete a dataset named DS_NAME in the LOCAL_DIRECTORY folder on the Local File System.\n\n";

  def main(args: Array[String]): Unit = {

    println("RepoDir: " + ut.RepoDir)
    if (args.length > 0 && ("h" == args(0) || "help" == args(0))) {
      logger.warn(usage)
      System.exit(0)
    }
    if (args.length == 0) {
      System.out.println("WARN:\tThe specified command is not supported.. \n" + "\tType { DSBackup h | help } for help...")
      return
    }

    val Command = args(0)
    Command.toLowerCase match {
      case "deleteds" =>
        var DatasetName = ""
        var Locallocatoin = ""
        if (args.length < 2) {
          logger.warn(usage)
          return
        }
        DatasetName = args(1)
        Locallocatoin = args(2)
        System.out.println(args(1))
        deleteDS(args(1), Locallocatoin)
      case "exportdstolocal" =>
        var DatasetName = ""
        var Locallocatoin = ""
        if (args.length < 3 || args.length > 4) {
          System.out.println(usage)
          return
        }
        DatasetName = args(1)
        Locallocatoin = args(2)
        if (args.length == 4) username = args(3)
        if (repository.DSExists(DatasetName, username))
          exportDsToLocal(DatasetName, username, Locallocatoin)
        else
          {
            logger.warn(s"Dataset '$DatasetName' is not found for '$username' user")
          }
      case "zip" =>
        var DatasetName = ""
        var Locallocatoin = ""
        if (args.length < 3 || args.length > 4) {
          System.out.println(usage)
          return
        }
        DatasetName = args(1)
        Locallocatoin = args(2)
        if (args.length == 4) username = args(3)
        if (repository.DSExists(DatasetName, username))
          zip(DatasetName, username, Locallocatoin)
        else
        {
          logger.warn(s"Dataset '$DatasetName' is not found for '$username' user")
        }
      case "zipall" =>
        var DatasetsList = ""
        var Locallocatoin = ""
        if (args.length < 3 || args.length > 4) {
          System.out.println(usage)
          return
        }
        DatasetsList = args(1)
        Locallocatoin = args(2)
        if (args.length == 4) username = args(3)
        scala.io.Source.fromFile(DatasetsList).getLines().foreach{ ds =>
          if (repository.DSExists(ds, username))
            zip(ds, username, Locallocatoin)
          else
            {
              logger.warn(s"Dataset '$ds' is not found for '$username' user")
            }
        }
      case "backupto" =>
        var DatasetName = ""
        var Locallocatoin = ""
        var backuplocatoin = ""
        if (args.length < 4 || args.length > 5) {
          System.out.println(usage)
          return
        }
        DatasetName = args(1)
        Locallocatoin = args(2)
        backuplocatoin = args(3)
        if (args.length == 5) username = args(4)
        if (repository.DSExists(DatasetName, username))
          backupTo(DatasetName, username, Locallocatoin, backuplocatoin)
        else
        {
          logger.warn(s"Dataset '$DatasetName' is not found for '$username' user")
        }
      case "backupall" =>
        var DatasetsList = ""
        var Locallocatoin = ""
        var backuplocatoin = ""
        if (args.length < 4 || args.length > 5) {
          System.out.println(usage)
          return
        }
        DatasetsList = args(1)
        Locallocatoin = args(2)
        backuplocatoin = args(3)
        if (args.length == 5) username = args(4)
        Await.result(Future.sequence( scala.io.Source.fromFile(DatasetsList).getLines().map{ ds =>
          Future {
            if (repository.DSExists(ds, username))
              backupTo(ds, username, Locallocatoin, backuplocatoin)
            //            backup(ds, username, Locallocatoin, backuplocatoin)
            else {
              logger.warn(s"Dataset '$ds' is not found for '$username' user")
            }
          }
        }), Duration.Inf)
      case _ =>
        logger.error("The Command is not defined....")
        logger.warn(usage)
    }
  }

  /**
    *  export the dataset from Hadoop Distributed File system to Local File system including additional files like 'vocabulary.txt' and 'info.txt'
    *
    * @param dataSetName String of the dataset name
    * @param userName String of the owner of the dataset
    * @param localDir  String of the local directory path
    */
  def exportDsToLocal(dataSetName: String, userName: String, localDir:String): Unit = {

    logger.info(s"Exporting '$dataSetName' to local ...")
    // export the schema and the script files
    FileUtils.copyInputStreamToFile(repository.getInfoStream(dataSetName, username), new File(s"$localDir/$dataSetName/info.txt"))
    FileUtils.copyInputStreamToFile(repository.getSchemaStream(dataSetName, username), new File(s"$localDir/$dataSetName/files/schema.xml"))

    val gMQLDataSetXML = new GMQLDataSetXML(dataSetName, userName).loadDS()
//    val dest = new File(localDir)
//    dest.mkdir()

    // export vocabulary
    val vocabularyCount = new VocabularyCount
    vocabularyCount.addVocabulary(new GMQLDataSetXML(dataSetName, userName).getMeta())
    FileUtils.copyInputStreamToFile(vocabularyCount.getStream, new File(s"$localDir/$dataSetName/vocabulary.txt"))

    //copy samples/meta files to local file system
    gMQLDataSetXML.samples.map { x =>
        FS_Utilities.copyfiletoLocal(ut.getHDFSRegionDir(userName) + x.name, s"$localDir/$dataSetName/files/" + new File(x.name).getName)
      }

    logger.info(s"Dataset was exported to: $localDir/$dataSetName")

  }

  /**
    *  Delete the dataset from the Local File System
    *
    * @param dataSetName String of the dataset name
    * @param localDir  String of the local directory path
    */
  def deleteDS(dataSetName: String, localDir:String): Unit ={
    val directory = new Directory(new File(s"$localDir/$dataSetName"))
    directory.deleteRecursively()
  }

  /**
    *  zip the dataset and store it to Local File system
    *
    * @param dataSetName String of the dataset name
    * @param userName String of the owner of the dataset
    * @param localDir  String of the local directory path
    */
  def zip(dataSetName: String, userName: String, localDir:String): Unit ={
    exportDsToLocal(dataSetName, username, localDir)

    ZipWriter.createZip(s"$localDir/$dataSetName")

    deleteDS(dataSetName, localDir)
  }

  /**
    *  zip the dataset using shell API and store it to Local File system
    *
    * @param dataSetName String of the dataset name
    * @param userName String of the owner of the dataset
    * @param localDir  String of the local directory path
    */
  def zipShell(dataSetName: String, userName: String, localDir:String): Unit ={
    exportDsToLocal(dataSetName, username, localDir)

    Process(s"zip -r $dataSetName.zip $dataSetName -x */.*", new java.io.File(localDir)).!

    deleteDS(dataSetName, localDir)
  }

  /**
    *  backup the dataset to the backup folder
    *
    * @param dataSetName String of the dataset name
    * @param userName String of the owner of the dataset
    * @param localDir  String of the local directory path
    */
  def backupTo(dataSetName: String, userName: String, localDir:String, backupDir:String): Unit ={
    zip(dataSetName, userName, localDir)

    logger.info(s"Sending '$dataSetName' to '$backupDir' ...")
    val rsync: RSync = new RSync()
      .source(s"$localDir/$dataSetName.zip")
      .destination(backupDir)
      .recursive(true)
      .archive(true)
      .verbose(true)
      .compress(true)

    val output = rsync.execute
//    System.out.println(output.getStdOut)
//    System.out.println("Exit code: " + output.getExitCode)
    if (output.getExitCode > 0) System.err.println(output.getStdErr)
    else {
      logger.info(s"RSYNC: '$dataSetName.zip' is sent to $backupDir")
      new File(s"$localDir/$dataSetName.zip").delete()
    }
  }

}
