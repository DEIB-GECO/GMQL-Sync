# GMQL-Sync
The script is designed for synchronizing GMQL public repository between two servers

## Requirements
The following tools are required to be installed on both servers for the correct work of the script:
   - Apache Hadoop.
      - Guide for Apache Hadoop installation can be found in [Hadoop installation page](https://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-common/SingleCluster.html).
   - Make sure that `rsync`, `ssh` and `xpath` are installed.
      - You can use this command in terminal (Ubuntu/Debian) to install them:
          ```sh
         $ sudo apt-get install rsync
         $ sudo apt-get install libxml-xpath-perl
         $ sudo apt-get install ssh
         ```
## Installation
```sh
git clone https://github.com/DEIB-GECO/GMQL-Sync.git
cd GMQL-Sync
mvn clean install
```

# Synchronization script

## Usage
```sh
$ ./bin/gmqlsync.sh [<options>] <SOURCE> <DEST>
```
`gmqlsync.sh` **MUST BE** run on `<SOURCE>` server

<!--### Defaults
It can also be used with no parameters:
   ```sh
   $ ./bin/gmqlsync.sh
   ```
In that case, the `<SOURCE>` and `<DEST>` are set to the following:
  - `<SOURCE>`=/home/gmql/gmql_repository/data/public
  - `<DEST>`=cineca:/gmql-data/gmql_repository/data/public
-->

### Options
| Option            | Description |
|-------------------|-------------|
|  `--delete`       | delete extraneous files from destination dirs|
|  `--dry-run`      | perform a trial run with no changes made, this only generates a list of changed datasets|
|  `--user`         | set user name (hdfs folder name) to synchronize|
|  `--tmpDir`       | set temporary directory for local script output files. Default value is: <br/> `"/share/repository/gmqlsync/tmpSYNC"`|
|  `--tmpHdfsSource`| set temporary directory for hdfs files movement on source server. Default value is: <br/> `"/share/repository/gmqlsync/tmpHDFS"`|
|  `--tmpHdfsDest`  | set temporary directory for hdfs files movement on destination server. Default value is: <br/> `"/hadoop/gmql-sync-temp"`|
|  `--logsDir`      | logging directory on source server. Default value is  `"/share/repository/gmqlsync/logs/"`|
|  `--help, (-h)`   | show help|

### Examples

1. To synchronize cineca server with genomic, run the following command:
```sh
$ ./bin/gmqlsync.sh /home/gmql/gmql_repository/data/public cineca:/gmql-data/gmql_repository/data/public
```
2. To get a list of datasets that exists on genomic server, but are missed on cineca server, run the following:
```sh
$ ./bin/gmqlsync.sh --dry-run /home/gmql/gmql_repository/data/public cineca:/gmql-data/gmql_repository/data/public
```
   With `--dry-run` option, the script only checks for differences on the serves, and generates a file with a list of new     datasets.

3. To allow deletion of datasets that we removed from genomic server, use the following command:
```sh
$ ./bin/gmqlsync.sh --delete /home/gmql/gmql_repository/data/public cineca:/gmql-data/gmql_repository/data/public
```

## Description
The script is build for synchronizing gmql repository of public datasets.
It first checks if there are any difference in local FS GMQL repository by running `rsync` tool in dry-run mode.
The result of `rsync` is then stored in `rsync_out.txt` file and parse into two arrays: one is for all the datasets that should be added in `<DEST>`, and the other one is for datasets to delete from `<DEST>`. Datasets list to add and delete are then saved to `rsync_add.txt` and `rsync_del.txt` files respectivelly.

**NOTE:** Dataset names are file names in local FS GMQL repository. If tool was run with `--dry-run` option, it will exit after generating datasets list.

After getting the datasets lists, it retrieves hdfs path of every DS, and copy the datasets from hdfs repository to temporary hdfs directory in local FS.
Then, using `rsync`, the files from temporary hdfs dir on `<SOURCE>` are copied to temporary hdfs dir on `<DEST>`.
After that, on `<DEST>`, all files in temporary hdfs directory are copied to hdfs repository on `<DEST>`.
Then, compare the dataset sizes in hdfs repositories on both, `<SOURCE>` and `<DEST>`, to make sure that the copy was successful.
If copying of hdfs files finnished successfully, then copy files in local FS gmql repository by running `rsync` in normal mode.
If the script was run with `--delete` option, then perform removing of datasets on `<DEST>`.
Finally, clean up temporary hdfs directories.

The script also generates a `.log` file in `logs` folder.

## Script files
The tool consists of several script files to make less ssh connections:
- `gmqlsync.sh` is the main script file to be used
- `gmqlsyncCheckHdfsDest.sh` gets dataset size in hdfs on the destination server
- `gmqlsyncDelHdfsDest.sh` removes datasets on the destination server

**NOTE:** `ssh` connection to the remote server should be passwordless.


# Backup tool
The tool is designed for backing up GMQL public repository after the synchronization.

## Description
The script performs the backup of the datasets from a given list to a given destination server. It first exports a dataset to a temporary folder on local file system, then zip it and sends to the destination backup server.

## Usage
```sh
$ bin/DSBackup $COMMAND 
```

### Allowed Commands are:
- `BackupTo <DS_NAME> <TMP_DIR> <BACKUP_PATH>`

   This command backup a dataset named `DS_NAME` to backup folder. </br>
   `TMP_DIR` is the full path to the temporary local location. </br>
   `BACKUP_PATH` is the full path to the backup location on the remote server

- `BackupALL <DS_LIST_FILE> <TMP_DIR> <BACKUP_PATH>`

   This command backup all datasets from the `DS_LIST_FILE` to backup folder.</br>
	`TMP_DIR` is the full path to the temporary local location. </br>
	`BACKUP_PATH` is the full path to the backup location on the remote server.
   
- `Zip <DS_NAME> <LOCAL_DIRECTORY>`
   
   This command zip a dataset named `DS_NAME` to local folder.</br>
	`LOCAL_DIRECTORY` is the full path to the local location.
   
- `ZipALL <DS_LIST_FILE> <LOCAL_DIRECTORY>`
   
   This command zip all datasets listed in the file `DS_LIST_FILE` to local directory.
   
- `h` or `help`

   Shows usage.

### Examples

1. After running the synchronization, you can find the datasets that were added in the following file:
`"/share/repository/gmqlsync/tmpSYNC/rsync_add.txt"`
You can use this file to backup the newly added datasets:
```sh
$ bin/DSBackup BackupAll /share/repository/gmqlsync/tmpSYNC/rsync_add.txt /share/repository/gmqlsync/tmpHDFS geco:/home/hdfs/gmql_repo_backup/
```
**NOTE:** Datasets will be first copied to local folder `"/share/repository/gmqlsync/tmpHDFS"`, then zipped and sent to `"geco:/home/hdfs/gmql_repo_backup/"`. After that, the temporary folder will be cleaned up.

2. If you want to do the backup of only one dataset, use the following command:
```sh
$ bin/DSBackup BackupAll <DS_NAME> /share/repository/gmqlsync/tmpHDFS geco:/home/hdfs/gmql_repo_backup/
```

3. If the backup directory is on the local server, you can use `Zip` or `ZipAll` commands:
```sh
$ bin/DSBackup Zip <DS_NAME> /home/hdfs/gmql_repo_backup/
```
   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; OR
```sh
$ bin/DSBackup ZipAll /share/repository/gmqlsync/tmpSYNC/rsync_add.txt /home/hdfs/gmql_repo_backup/
```

