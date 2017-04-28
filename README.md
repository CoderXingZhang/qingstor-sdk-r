# QingStor Client Package


Prepare step

``` R
install.packages('httr') \
install.packages('xml2') \
install.packages('aws.signature') \
install.packages('data.table') \
install.packages('path\\to\\QingStorRSDK_0.1', repos = NULL, type="source") \

library('Qingstor')
```
First set as follows in R environment: \
``` R
Sys.setenv("Qstor_ACCESS_KEY_ID" = "...", "Qstor_SECRET_ACCESS_KEY" = "...", "Qstor_DEFAULT_REGION" = "pek3a")
```

## get_object
 get an object in memory(now only support csv data, read data as dataframe)
``` R
get_object(object = 'object_name', bucket = 'bucket_name') \
```
the object_name could be a name under a folder like 'some_folder/object_name'

## put_object
  put an object in Qingstor bucket
  
  ``` R 
  tempPath <- file.path("path","to","folder","xxxx.csv") \
  put_object(file = tempPath , object = basename(tempPath),folder = 'folder_name', bucket = 'bucket_name') \
  ```
  
  folder is optional

## delete_object
  delete an object in Qingstor bucket 

  ``` R
 delete_object(object = 'object_name', bucket = 'bucket_name') \ 
  ```
 the object_name could be a name under a folder like 'some_folder/object_name'


## get_bucket
 list the object in Qingstor bucket

 Note : folder is must, even in the root of the bucket .
 ``` R
 get_bucket(bucket = 'bucket_name',folder = "/") \
 ``` 

 prefix is support ,example: \
 
 ``` R
 get_bucket(bucket = 'bucket_name',folder = "/",prefix = 'prefix_name')
 ``` 

>**Now there is no operation on bucket ,for example create bucket, delete bucket.**