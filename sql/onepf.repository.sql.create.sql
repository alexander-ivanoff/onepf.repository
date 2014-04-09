
CREATE DATABASE `onepf_repository_4`;

CREATE TABLE `onepf_repository_4`.`appstores` (
  `appstoreId` varchar(255) NOT NULL,
  `description` varchar(255) DEFAULT NULL,
  `openaepUrl` varchar(255) NOT NULL,
  `repositoryAccessToken` varchar(64) NOT NULL,
  `appstoreAccessToken` varchar(64) NOT NULL,
  `publicKey` text,
  PRIMARY KEY (`appstoreId`)
);

CREATE TABLE `onepf_repository_4`.`appstoreupdates` (
  `appstoreId` varchar(255) NOT NULL,
  `lastUpdateHash` varchar(32) NOT NULL,
  `lastUpdateDateTime` datetime NOT NULL,
  `lastUpdateOffset` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`appstoreId`),
  FOREIGN KEY (`appstoreId`) REFERENCES `onepf_repository_4`.`appstores` (`appstoreId`)
);

CREATE TABLE `onepf_repository_4`.`statistics_updates` (
  `appstoreId` varchar(255) NOT NULL,
  `feedType` varchar(255) NOT NULL,
  `lastUpdateCount` int(11) NOT NULL,
  `lastUpdateDateTime` datetime NOT NULL,
  `lastUpdateOffset` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`appstoreId`),
  FOREIGN KEY (`appstoreId`) REFERENCES `onepf_repository_4`.`appstores` (`appstoreId`)
);

CREATE TABLE `onepf_repository_4`.`applications` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `package` varchar(255) NOT NULL,
  `appstoreId` varchar(255) DEFAULT NULL,
  `devContact` varchar(255) DEFAULT NULL,
  `lastUpdate` datetime DEFAULT NULL,
  `version` varchar(255) DEFAULT NULL,
  `build` int(11) DEFAULT NULL,
  `appdfLink` text,
  `descrLink` text,
  `appdfMD5Hash` text,
  `currPageHash` int(11) DEFAULT NULL,
  `prevPageHash` int(11) DEFAULT NULL,
  `hash` varchar(32) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `appstoreId` (`appstoreId`),
  FOREIGN KEY (`appstoreId`) REFERENCES `onepf_repository_4`.`appstores` (`appstoreId`)
);

CREATE TABLE `onepf_repository_4`.`downloads` (
  `id` varchar(255) NOT NULL,
  `package` varchar(255) NOT NULL,
  `opDate` datetime DEFAULT NULL,
  `version` varchar(255) DEFAULT NULL,
  `build` int(11) DEFAULT NULL,
  `lastUpdate` datetime DEFAULT NULL,
  `country` varchar(2) DEFAULT NULL,
  `deviceModel` varchar(255) DEFAULT NULL,
  `deviceName` varchar(255) DEFAULT NULL,
  `isUpdate` tinyint(1) DEFAULT NULL,
  `currPageHash` int(11) DEFAULT NULL,
  `prevPageHash` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`)
);

CREATE TABLE `onepf_repository_4`.`purchases` (
  `id` varchar(255) NOT NULL,
  `package` varchar(255) NOT NULL,
  `opDate` datetime DEFAULT NULL,
  `version` varchar(255) DEFAULT NULL,
  `build` int(11) DEFAULT NULL,
  `lastUpdate` datetime DEFAULT NULL,
  `country` varchar(2) DEFAULT NULL,
  `deviceModel` varchar(255) DEFAULT NULL,
  `deviceName` varchar(255) DEFAULT NULL,
  `innerCurrency` varchar(3) DEFAULT NULL,
  `innerPrice` varchar(20) DEFAULT NULL,
  `userCurrency` varchar(3) DEFAULT NULL,
  `userPrice` varchar(20) DEFAULT NULL,
  `currPageHash` int(11) DEFAULT NULL,
  `prevPageHash` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`)
);

CREATE TABLE `onepf_repository_4`.`reviews` (
  `id` varchar(255) NOT NULL,
  `package` varchar(255) NOT NULL,
  `version` varchar(255) DEFAULT NULL,
  `build` int(11) DEFAULT NULL,
  `lastUpdate` datetime DEFAULT NULL,
  `country` varchar(2) DEFAULT NULL,
  `deviceModel` varchar(255) DEFAULT NULL,
  `deviceName` varchar(255) DEFAULT NULL,
  `stars` tinyint(1) DEFAULT NULL,
  `userName` varchar(255) DEFAULT NULL,
  `userUrl` varchar(255) DEFAULT NULL,
  `title` varchar(255) DEFAULT NULL,
  `textBody` text,
  `currPageHash` int(11) DEFAULT NULL,
  `prevPageHash` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`)
);

/* Create test appstore*/
INSERT INTO `onepf_repository_4`.`appstores`
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
INSERT INTO `onepf_repository_4`.`appstores`
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

	
	