How to deploy Repository application to AWS.
-------------------
The following AWS services shoud be configured to run repository on AWS:
	- EC2 instance 
	- RDS instance (MySQL)
	- Amazon S3 
Bellow you find detailed description how to setup every service.

## 1. Setup EC2 instance
#### a) Launch new EC2 instance
To run onepf_repository you need to any servlet container (tomcat is recommened). The instruction how to launch EC2 instance with tomcat servlet container 
   
   - Go to EC2 Dashboard
   - Press `Launch instance` button
   - Choose corresponding AMI (for easy start use Tomcat ready AMI - `Tomcat powered by Bitnami` for example)
   - select corresponding for your needs instance (m1-m3 should be enought).
   - If AMI is without preinstalled Java, Tomcat, then you should to install them by yourself (Java 6, Tomcat 6 are recommended)
	
#### b) Setup tomcat (don't need for `Tomcat powered by Bitnami`)
If you want to deploy onepf-repository via tomcat's manager app, then you should add an user for it:

- edit tomcat-users.xml file in the tomcat's configuration directory (/usr/share/tomcat6/conf for ElasticBeanstalk-Tomcat6-64bit installation)
- add following lines: 

```
<role rolename="manager-gui"/>
<user username="username" password="password" roles="manager_gui"/>
```

## 2. Setup RDS instance

#### a) Launch new RDS instance 
To run onepf_repository also need MySQL database. Below the instruction how to launch RDS instance with MySQL database instance.

- Go To RDS Dashboard
- Press `Launch a DB Instance`
- Select `MySQL Community Edition`
- At the next screen, select `Yes`, if you need `Multi-AZ Deployment` and `Provisioned IOPS Storage`, otherwise select `No`
- Choose db parameters, and instance class you needed. Also choose your DB Identifier and Master user name & password. (remember that you will need it later to setup repository web application)
- at the next page input name of your database (Name is not matter, new database will be created when you import database schema at the next step).
- on the `Management Options` choose options you needed.
- Launch instance and wait till it create.

#### b) Import onepf database schema
- find repository schema in /sql/onepf.repository.sql.create.sql
- import schema to your created instance. (For example you can do it with MySQL workbench)
- Import with MySQL workbench:
- Install & launch MySQL workbench
- add new SQL connection, and fill following fields
	- Connection Name: anything you want
	- Hostname: enter name of your endpoint name (RDS Dashboard -> instance details -> `Endpoint` field)
	- Port: port from `Endpoint`
	- Username: enter your Master username entered at previous step
	- Password: enter your Master password entered at previous step
- Press 'Ok'
- when you are connected, go to `Server` -> `Data Import`. Select `Import from Self-Contained File` and choose `onepf.repository.sql.create.sql` file
- Press `Start Import`
- when import is completed, refresh all and you should see new `onepf_repository` database. 

#### c) Create new database user (with MySQL workbench):
Master user can be used to access database, but it is recommended to create special user with restricted access.

- Go to `Users and Priveleges` press `Add Account`
- Go to `Login` tab, enter username, password
- Go to `Schema Privileges` tab and add new entry with access only to `onepf_repositry` database with following rights: `SELECT, INSERT, UPDATE, DELETE`. 

## 3. Setup File Storage Options
To store appdf files from other stores you have two options:

- local file system (then all appdf files will be stored in folder in your Web Application directory)
- Amazon S3 - then files will be stored in bucket on Amazon cloud.

#### a) Setup Amazon S3
- Go to Services -> S3
- Press `Create Bucket`
- Enter name of the bucket and press `Create`
- Wait till bucket will be created
- To access amazon S3 from repository web application you should create IAM user and use it's credentials (see below how to do that)
- Give access to the bucket to authorized users:
	- Go to Services -> S3
	- Press `Properties` button
	- Press `Add more permissions`
	- fill fields:
		- Grantee: `Authenticated Users`
		- Select `List` and `Upload/Delete` checkboxes
	- Press `Save`
	
#### b) Setup IAM user
- Go to Services -> IAM -> Users
- Press `Create New Users`
- Enter user name and press `Create`
- Press `Download Credentials` and save it (Don't lose it - you will need it later)
- Also you need to attach Policy to give user access to Amazon S3
	- select created user
	- go to `Permissions` tab and press `Attach User Policy`
	- select `Custom Policy` and press `Select`
	- enter any `Policy Name` and following text as `Policy Document`. Change ***<bucket_name>*** to the name of the bucket you created:
	
```
			{
 		 		"Version": "2012-10-17",
  				"Statement": [
    					{
      						"Effect": "Allow",
      						"Action": ["s3:ListAllMyBuckets"],
      						"Resource": ["arn:aws:s3:::*"]
   					},
    					{
      						"Effect": "Allow",
      						"Action": ["s3:ListBucket"],
      						"Resource": ["arn:aws:s3:::<bucket_name>"]
    					},
    					{
      						"Effect": "Allow",
      						"Action": ["s3:*"],
      						"Resource": ["arn:aws:s3:::<bucket_name>/*"]
    					}
  				]
			}
```
- press `Apply Policy` and wait till it is added to the created user.
			
		
## 4. Configure onepf.repository web application 
#### a) Setup database connection:
- open `/src/resources/hibernate.cfg.xml` in text editor
- change following properties:
	- hibernate.connection.username - change it to username entered in ***(2.c)***
	- hibernate.connection.password - change it to password entered in ***(2.c)***
	- hibernate.connection.url - change it to your endpoint (RDS Dashboard -> instance details -> 'Endpoint' field)
	
#### b) Setup Amazon S3 connection:
You will need csv file with credentials downloaded in ***(3.b)***

- open `/web/WEB-INF/settings/AwsCredentials.properties`
- change following properties:
	- accessKey - change to 'Access Key Id' from downloaded file
	- secretKey - change to 'Secret Access Key' from downloaded file
	
#### c) Generic Setup
- open `/web/WEB-INF/settings/configuration.properties` and change following properties:
	- storageService - where to save appdf files. Two options `amazon` and `filesystem`
	- amazon-region - there should be id of region where your bucket deployed (such as `us-east-1`)
	- amazon-bucket - name of the bucket created at step ***(3.a)***
- open `/web/WEB-INF/web.xml` and change following properties
	- pollStores - switch to `false` if you want repository to NOT query other stores (from appstores table) for applications and statistics 

## 6. Add Appstore to repository
At this moment the only one way to add Appstore to the repository is manually add it to `appstores` table in `onepf_repository` database. You could do it with MySQL Workbench for example. After App Store is added to the table, you should restart `onepf.repository` web application.

## 7. Compile war file and deploy
Deployment procedure depends what AMI you selected at the 1 step. Usually you could use tomcat's applications manager to deploy your web application. 