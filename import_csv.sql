-- allow advanced options to be changed
EXEC sp_configure 'show advanced options', 1
GO
RECONFIGURE
GO

EXEC sp_configure 'xp_cmdshell', 1
GO
RECONFIGURE
GO

EXEC sp_configure 'Ad Hoc Distributed Queries', 1;
GO
RECONFIGURE;
GO

--BULK INSERT MULTIPLE FILES From a Folder 

--a table of filenames
IF OBJECT_ID('ALLFILENAMES', 'U') IS NOT NULL 
  DROP TABLE ALLFILENAMES;

CREATE TABLE ALLFILENAMES(WHICHPATH VARCHAR(255),WHICHFILE varchar(255))

--some variables
DECLARE @filename varchar(255),
        @path     varchar(255),
        @sql      varchar(8000),
        @cmd      varchar(1000)

--insert filenames into table ALLFILENAMES
SET @path = 'H:R\data_Q3_2016\data_Q3_2016\'
SET @cmd = 'dir ' + @path + '2016-*.csv /b'
INSERT INTO ALLFILENAMES(WHICHFILE) 
EXEC Master..xp_cmdShell @cmd
UPDATE ALLFILENAMES SET WHICHPATH = @path WHERE WHICHPATH IS NULL

--cursor loop through the filenames
DECLARE c1 CURSOR FOR SELECT WHICHPATH,WHICHFILE FROM ALLFILENAMES WHERE WHICHFILE LIKE '%.csv%'
OPEN c1
FETCH NEXT FROM c1 INTO @path,@filename
WHILE @@fetch_status <> -1
  BEGIN
    --bulk insert won't take a variable name, so make a sql and execute it instead:
    SET @sql = 'BULK INSERT HardDisk.dbo.HardDriveTable FROM ''' + @path + @filename + ''' '
        + '     WITH ( 
                FIELDTERMINATOR = '','', 
                ROWTERMINATOR = ''\n'', 
                FIRSTROW = 2 
                ) '
    PRINT @sql
    EXEC (@sql)

    FETCH NEXT FROM c1 INTO @path,@filename
  END
CLOSE c1
DEALLOCATE c1
