How to deploy onepf.repository on your server
-------------

blah blah blah about **onepf.repository**.

Step 1. Setup enviroment 
-------------
You need to install following tools on your server to work with **onepf.repository**. 

- at least Java 1.6
- Servlet Container (Tomcat 6.0 recomended)
- MySQL Server
- MySQL drivers (should be installed to tomcat's /lib folder, if you used it)

Step 2. Compile and install onepf.repository 
-------------
- get sources from github
- compile .war package with idea or any other way
- deploy .war package with your servlet container
- deploy database by launch .sql file with your MySQL server


Step 3. Setup onepf.repository with property files
-------------
####Setup DataService (only SqlDataService is functional now).
To setup SqlDataService you need to change following options in SqlOptions.java:

- dbUrl - url to your database instance
- dbUser - database username
- dbPassword - database user password
- driverClassName - classname of database drivers. (form Mysql database leave unchanged)

####Setup StorageService.
there are two options to sue StorageService:

- local filesystem of server where webapplication is launched 
- Amazon S3 Storage

To choose one of them you should go to the RepositoryConfigurator.java -> getRepositoryFactory(). Initialize storageService variable with:

- new FilesystemStorageService(new FilesystemOptions(context)); - for local filesystem.
- new AmazonStorageService(new AmazonOptions()); - for amazon S3 storage

after any changes you need to recompile and redeploy project



