
CREATE DATABASE onepf_repository;

CREATE TABLE `appstores` (
  `appstoreId` varchar(255) NOT NULL,
  `description` varchar(255) DEFAULT NULL,
  `openaepUrl` varchar(255) NOT NULL,
  `repositoryAccessToken` varchar(64) NOT NULL,
  `appstoreAccessToken` varchar(64) NOT NULL,
  `publicKey` text,
  PRIMARY KEY (`appstoreId`)
);

CREATE TABLE `applications` (
  `id` INT AUTO_INCREMENT,
  `package` varchar(255) NOT NULL,
  `appstoreId` varchar(255) DEFAULT NULL,
  `devContact` varchar(255) DEFAULT NULL,
  `lastUpdate` datetime DEFAULT NULL,
  `version` varchar(255) DEFAULT NULL,
  `build` int(11) DEFAULT NULL,
  `appdfLink` text,
  `descrLink` text,
  `appdfMD5Hash` text,
  `currPageHash` INT,
  `prevPageHash` INT,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`appstoreId`) REFERENCES `appstores`(`appstoreId`)
);

CREATE table purchases (
	id varchar(255) NOT NULL,
	package varchar(255) NOT null,
	opDate datetime,
	version varchar(255),
	build int,
	lastUpdate datetime,
	country varchar(2), /* as in ISO 3166-1 alpha-2 (US, DE, RU, etc.)*/
	deviceModel varchar(255),
	deviceName varchar(255),
	innerCurrency varchar(3), /* as in ISO 4217 (USA, EUR, RUR, etc.) */
	innerPrice varchar(20), /* is 20 characters enought? */
	userCurrency varchar(3),
	userPrice varchar(20),
	`currPageHash` INT,
  	`prevPageHash` INT,
	PRIMARY KEY (id)
);

CREATE table downloads (
	id varchar(255) NOT NULL,
	package varchar(255) NOT NULL,
	opDate datetime,
	version varchar(255),
	build int,
	lastUpdate datetime,
	country varchar(2), /* as in ISO 3166-1 alpha-2 (US, DE, RU, etc.)*/
	deviceModel varchar(255),
	deviceName varchar(255),
	isUpdate bool,
	`currPageHash` INT,
  	`prevPageHash` INT,
	PRIMARY KEY (id)
);

CREATE table reviews (
	id varchar(255) NOT NULL,
	package varchar(255) NOT null,
	version varchar(255),
	build int,
	lastUpdate datetime,
	country varchar(2), /* as in ISO 3166-1 alpha-2 (US, DE, RU, etc.)*/
	deviceModel varchar(255),
	deviceName varchar(255),
	stars tinyint(1), /* 1,2,3,4,5 */
	userName varchar(255),
	userUrl varchar(255),
	title varchar(255),
	textBody text,
	`currPageHash` INT,
  	`prevPageHash` INT,
	PRIMARY KEY (id)
);

CREATE TABLE `appstoreupdates` (
  `appstoreId` varchar(255) NOT NULL,
  `lastUpdateHash` varchar(32) NOT NULL,
  `lastUpdateDateTime` datetime NOT NULL,
  `lastUpdateOffset` varchar(255) DEFAULT NULL,
  FOREIGN KEY (`appstoreId`) REFERENCES `appstores`(`appstoreId`)
);
	
	
	