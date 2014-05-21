
CREATE DATABASE `onepf_repository`;

CREATE TABLE `onepf_repository`.`appstores` (
  `appstoreId` varchar(255) NOT NULL,
  `description` varchar(255) DEFAULT NULL,
  `openaepUrl` varchar(255) NOT NULL,
  `repositoryAccessToken` varchar(64) NOT NULL,
  `appstoreAccessToken` varchar(64) NOT NULL,
  `publicKey` text,
  PRIMARY KEY (`appstoreId`)
);

CREATE TABLE `onepf_repository`.`appstoreupdates` (
  `appstoreId` varchar(255) NOT NULL,
  `lastUpdateHash` varchar(32) NOT NULL,
  `lastUpdateDateTime` datetime NOT NULL,
  `lastUpdateOffset` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`appstoreId`),
  UNIQUE KEY `appstoreId_UNIQUE` (`appstoreId`),
  FOREIGN KEY (`appstoreId`) REFERENCES `onepf_repository`.`appstores` (`appstoreId`)
);

CREATE TABLE `onepf_repository`.`statistics_updates` (
  `appstoreId` varchar(255) NOT NULL,
  `feedType` varchar(255) NOT NULL,
  `lastUpdateCount` int(11) NOT NULL,
  `lastUpdateDateTime` datetime NOT NULL,
  `lastUpdateOffset` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`appstoreId`,`feedType`),
  KEY `appstoreId` (`appstoreId`),
  FOREIGN KEY (`appstoreId`) REFERENCES `onepf_repository`.`appstores` (`appstoreId`)
);

CREATE TABLE `onepf_repository`.`applications` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `package` varchar(255) NOT NULL,
  `appstoreId` varchar(255) NOT NULL,
  `datetime` datetime DEFAULT NULL,
  `version` varchar(255) DEFAULT NULL,
  `versionCode` int(11) DEFAULT NULL,
  `appdfLink` text NOT NULL,
  `hash` varchar(32) NOT NULL,
  `currPageHash` int(11) NOT NULL DEFAULT '0',
  `prevPageHash` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  FOREIGN KEY (`appstoreId`) REFERENCES `onepf_repository`.`appstores` (`appstoreId`)
);

CREATE TABLE `onepf_repository`.`downloads` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `package` varchar(255) NOT NULL,
  `distributorStoreId` varchar(255) NOT NULL,
  `homeStoreId` varchar(45) NOT NULL,
  `version` varchar(255) DEFAULT NULL,
  `versionCode` int(11) NOT NULL,
  `datetime` datetime NOT NULL,
  `deviceModel` varchar(255) NOT NULL,
  `deviceName` varchar(255) DEFAULT NULL,
  `country` varchar(2) NOT NULL,
  `isUpdate` varchar(3) NOT NULL,
  `currPageHash` int(11) NOT NULL DEFAULT '0',
  `prevPageHash` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
);

CREATE TABLE `onepf_repository`.`purchases` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `purchaseId` varchar(255) NOT NULL,
  `package` varchar(255) NOT NULL,
  `distributorStoreId` varchar(255) NOT NULL,
  `homeStoreId` varchar(255) NOT NULL,
  `version` varchar(255) DEFAULT NULL,
  `versionCode` int(11) NOT NULL,
  `datetime` datetime NOT NULL,
  `deviceModel` varchar(255) NOT NULL,
  `deviceName` varchar(255) DEFAULT NULL,
  `country` varchar(2) NOT NULL,
  `innerCurrency` varchar(3) NOT NULL,
  `innerPrice` varchar(20) NOT NULL,
  `userCurrency` varchar(3) DEFAULT NULL,
  `userPrice` varchar(20) DEFAULT NULL,
  `signature` varchar(255) NOT NULL,
  `currPageHash` int(11) NOT NULL DEFAULT '0',
  `prevPageHash` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
);

CREATE TABLE `onepf_repository`.`reviews` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `package` varchar(255) NOT NULL,
  `distributorStoreId` varchar(255) NOT NULL,
  `homeStoreId` varchar(255) NOT NULL,
  `version` varchar(255) DEFAULT NULL,
  `versionCode` int(11) NOT NULL,
  `rating` decimal(4,2) NOT NULL,
  `datetime` datetime NOT NULL,
  `deviceModel` varchar(255) NOT NULL,
  `deviceName` varchar(255) DEFAULT NULL,
  `country` varchar(2) NOT NULL,
  `userName` varchar(255) DEFAULT NULL,
  `title` varchar(255) DEFAULT NULL,
  `textBody` text,
  `reviewUrl` varchar(255) DEFAULT NULL,
  `currPageHash` int(11) NOT NULL DEFAULT '0',
  `prevPageHash` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
);

CREATE TABLE `onepf_repository`.`receipts` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `receiptData` text,
  `distributorStoreId` varchar(45) DEFAULT NULL,
  `distributorSignature` text,
  `developerStoreId` varchar(45) DEFAULT NULL,
  `developerSignature` text,
  `datetime` datetime DEFAULT NULL,
  PRIMARY KEY (`id`)
);


/* Create test appstore*/
INSERT INTO `onepf_repository`.`appstores`
(`appstoreId`,
`description`,
`openaepUrl`,
`repositoryAccessToken`,
`appstoreAccessToken`,
`publicKey`)
VALUES
("com.appstore.test",
"Test Appstore",
"http://test.appstore.com",
"TESTTESTTEST",
"TESTTESTTEST",
"1234567890");

/* Create test appstore*/
INSERT INTO `onepf_repository`.`appstores`
(`appstoreId`,
`description`,
`openaepUrl`,
`repositoryAccessToken`,
`appstoreAccessToken`,
`publicKey`)
VALUES
("onepf.repository",
"Onepf repository",
"http://ec2-54-186-134-103.us-west-2.compute.amazonaws.com:8080/web_war",
"TESTTESTTEST",
"W2K423WUYFm16SvY3d0Qh6iRu6uEZ7NQ",
"0912345678");

	
	