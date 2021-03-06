+++
date = "2011-09-21T17:21:00.000000+02:00"
title = "Skytools3: walmgr"
tags = ["PostgreSQL", "Skytools", "WalMgr", "Replication", "Archiving"]
categories = ["PostgreSQL","Skytools"]
thumbnailImage = "/img/old/londiste_logo.gif"
thumbnailImagePosition = "left"
coverImage = "/img/old/londiste_logo.gif"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/09/21-skytools-walmgr-part-1",
           "/blog/2011/09/21-skytools-walmgr-part-1.html"]
+++

Let's begin the 
[Skytools 3](http://wiki.postgresql.org/wiki/SkyTools) documentation effort, which is long overdue.  The
code is waiting for you over at 
[github](https://github.com/markokr/skytools), and is stable and working.  Why is
it still in 
*release candidate* status, I hear you asking?  Well because it's
missing updated documentation.

[WalMgr](http://packages.debian.org/experimental/skytools3-walmgr) is the Skytools component that manages 
*WAL shipping* for you, and
archiving too.  It knows how to prepare your master and standby setup, how
to take a base backup and push it to the standby's system, how to archive
(at the satndby) master's WAL files as they are produced and have the
standby restore from this archive.

What's new in 
`walmgr` from Skytools 3 is its support for 
*Streaming
Replication* that made its way into PostgreSQL 9.0 and is even more useful in
PostgreSQL 9.1 (better monitoring, synchronous replication option).


# Getting ready

Now, I'm using debian here, and a build virtual machine where I'm doing the
*backporting* work.  As 
[PostgreSQL 9.1](http://www.postgresql.org/about/news.1349) is now out, let's use that.

~~~
:~$ pg_lsclusters 
Version Cluster   Port Status Owner    Data directory               
8.4     main      5432 online postgres /var/lib/postgresql/8.4/main ...
9.0     main      5433 online postgres /var/lib/postgresql/9.0/main ...
9.1     main      5434 online postgres /var/lib/postgresql/9.1/main ...
~~~


After some editing of the configuration files (enabling 
*hot standby* and
switching 
`pg_hba.conf` to 
`trust` for the sake of this example), we can see
that the cluster is ready to be abused:

~~~
:~$ sudo pg_ctlcluster 9.1 main restart
:~$ psql --cluster 9.1/main  -U postgres \
-c "select name, setting from pg_settings where name in ('max_wal_senders', 'wal_level')"
      name       |   setting   
-----------------+-------------
 max_wal_senders | 1
 wal_level       | hot_standby
(2 rows)

:~$ sudo mkdir -p /etc/walshipping/9.1/main /var/lib/postgresql/walshipping
:~$ sudo chown -R postgres:postgres /etc/walshipping /var/lib/postgresql/walshipping

:~$ ssh-keygen -t dsa
:~/.ssh$ cp id_dsa.pub authorized_keys
:~$ ssh localhost
~~~


So the order of operations is to prepare a standby, then have it restore
from the archives, then activate the wal streaming and check that the setup
allows the standby to switch back and forth between the streaming and the
archives.


# Setting walmgr 

To prepare the standby, we will do a 
*base backup* of the master.  That step
is handled by 
`walmgr`, so we first need to set it up.  Here's the sample
`master.ini` file:

~~~
[walmgr]
job_name             = wal-master
logfile              = /var/log/postgresql/%(job_name)s.log
pidfile              = /var/run/postgresql/%(job_name)s.pid
use_skylog           = 0

master_db            = port=5434 dbname=template1
master_data          = /var/lib/postgresql/9.1/main/
master_config        = /etc/postgresql/9.1/main/postgresql.conf
master_bin           = /usr/lib/postgresql/9.1/bin

# set this only if you can afford database restarts during setup and stop.
master_restart_cmd   = pg_ctlcluster 9.1 main restart

slave = 127.0.0.1
slave_config = /etc/walshipping/9.1/main/standby.ini

walmgr_data          = /var/lib/postgresql/walshipping/9.1/main
completed_wals       = %(walmgr_data)s/logs.complete
partial_wals         = %(walmgr_data)s/logs.partial
full_backup          = %(walmgr_data)s/data.master
config_backup        = %(walmgr_data)s/config.backup

# syncdaemon update frequency
loop_delay           = 10.0
# use record based shipping available since 8.2
use_xlog_functions   = 0

# pass -z to rsync, useful on low bandwidth links
compression          = 0

# keep symlinks for pg_xlog and pg_log
keep_symlinks        = 1

# tell walmgr to set wal_level to hot_standby during setup
#hot_standby          = 1

# periodic sync
#command_interval     = 600
#periodic_command     = /var/lib/postgresql/walshipping/periodic.sh
~~~


And the 
`/etc/walshipping/9.1/main/standby.ini` companion:

~~~
[walmgr]
job_name             = wal-standby
logfile              = /var/log/postgresql/%(job_name)s.log
use_skylog           = 0

slave_data           = /var/lib/postgresql/9.1/standby
slave_bin            = /usr/lib/postgresql/9.1/bin
slave_stop_cmd       = pg_ctlcluster 9.1 standby stop
slave_start_cmd      = pg_ctlcluster 9.1 standby start
slave_config_dir     = /etc/postgresql/9.1/standby/

walmgr_data          = /var/lib/postgresql/walshipping/9.1/main
completed_wals       = %(walmgr_data)s/logs.complete
partial_wals         = %(walmgr_data)s/logs.partial
full_backup          = %(walmgr_data)s/data.master
config_backup        = %(walmgr_data)s/config.backup

backup_datadir       = no
keep_backups         = 0
# archive_command =

# primary database connect string for hot standby -- enabling
# this will cause the slave to be started in hot standby mode.
primary_conninfo     = host=127.0.0.1 port=5434 user=postgres
~~~


And let's get started:

~~~
:~$ cp standby.ini /etc/walshipping/9.1/main/

:~$ walmgr3 -v master.ini setup
2011-09-21 16:57:05,685 30450 INFO Configuring WAL archiving
2011-09-21 16:57:05,687 30450 DEBUG found 'archive_mode' in config -- enabling it
2011-09-21 16:57:05,687 30450 DEBUG found 'wal_level' in config -- setting to 'archive'
2011-09-21 16:57:05,688 30450 DEBUG modifying configuration: {'archive_mode': 'on', 'wal_level': 'archive', 'archive_command': '/usr/bin/walmgr3 /var/lib/postgresql/master.ini xarchive %p %f'}
2011-09-21 16:57:05,688 30450 DEBUG found parameter archive_mode with value ''off''
2011-09-21 16:57:05,690 30450 DEBUG found parameter wal_level with value ''minimal''
2011-09-21 16:57:05,690 30450 DEBUG found parameter archive_command with value ''''
2011-09-21 16:57:05,691 30450 INFO Restarting postmaster
2011-09-21 16:57:05,691 30450 DEBUG Execute cmd: 'pg_ctlcluster 9.1 main restart'
2011-09-21 16:57:09,404 30450 DEBUG Execute cmd: 'ssh' '-Tn' '-o' 'Batchmode=yes' '-o' 'StrictHostKeyChecking=no' '127.0.0.1' '/usr/bin/walmgr3' '/etc/walshipping/9.1/main/standby.ini' 'setup'
2011-09-21 16:57:09,712 30450 INFO Done

postgres@squeeze64:~$ walmgr3 master.ini backup
2011-09-21 17:00:17,259 30702 INFO Backup lock obtained.
2011-09-21 17:00:17,277 30692 INFO Execute SQL: select pg_start_backup('FullBackup'); [port=5434 dbname=template1]
2011-09-21 17:00:17,791 30712 INFO Removing expired backup directory: /var/lib/postgresql/walshipping/9.1/main/data.master
2011-09-21 17:00:18,200 30692 INFO Checking tablespaces
2011-09-21 17:00:18,202 30692 INFO pg_log does not exist, skipping
2011-09-21 17:00:18,259 30692 INFO Backup conf files from /etc/postgresql/9.1/main
2011-09-21 17:00:18,590 30731 INFO First useful WAL file is: 000000010000000200000092
2011-09-21 17:00:19,901 30759 INFO Backup lock released.
2011-09-21 17:00:19,919 30692 INFO Full backup successful

:~$ walmgr3 /etc/walshipping/9.1/main/standby.ini listbackups

List of backups:

Backup set      Timestamp                Label       First WAL               
--------------- ------------------------ ----------- ------------------------
data.master     2011-09-21 17:00:17 CEST FullBackup  000000010000000200000092
~~~


Following articles will show how to manage that archive and how to go from
that to an 
*Hot Standby* fed by either 
*Streaming Replication* or 
*Archives*.
