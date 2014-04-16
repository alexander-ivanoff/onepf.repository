
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
  FOREIGN KEY (`appstoreId`) REFERENCES `onepf_repository`.`appstores` (`appstoreId`)
);

CREATE TABLE `onepf_repository`.`applications` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `package` varchar(255) NOT NULL,
  `appstoreId` varchar(255) DEFAULT NULL,
  `datetime` datetime DEFAULT NULL,
  `version` varchar(255) DEFAULT NULL,
  `versionCode` int(11) DEFAULT NULL,
  `appdfLink` text,
  `descrLink` text,
  `hash` varchar(32) DEFAULT NULL,
  `currPageHash` int(11) DEFAULT NULL,
  `prevPageHash` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`appstoreId`) REFERENCES `onepf_repository`.`appstores` (`appstoreId`)
);

CREATE TABLE `onepf_repository`.`downloads` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `package` varchar(255) NOT NULL,
  `distributorStoreId` varchar(255) DEFAULT NULL,
  `homeStoreId` varchar(45) DEFAULT NULL,
  `version` varchar(255) DEFAULT NULL,
  `versionCode` int(11) DEFAULT NULL,
  `datetime` datetime DEFAULT NULL,
  `deviceModel` varchar(255) DEFAULT NULL,
  `deviceName` varchar(255) DEFAULT NULL,
  `country` varchar(2) DEFAULT NULL,
  `isUpdate` tinyint(1) DEFAULT NULL,
  `currPageHash` int(11) DEFAULT NULL,
  `prevPageHash` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ;

CREATE TABLE `onepf_repository`.`purchases` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `purchaseId` varchar(255) DEFAULT NULL,
  `package` varchar(255) NOT NULL,
  `distributorStoreId` varchar(255) DEFAULT NULL,
  `homeStoreId` varchar(255) DEFAULT NULL,
  `version` varchar(255) DEFAULT NULL,
  `versionCode` int(11) DEFAULT NULL,
  `datetime` datetime DEFAULT NULL,
  `deviceModel` varchar(255) DEFAULT NULL,
  `deviceName` varchar(255) DEFAULT NULL,
  `country` varchar(2) DEFAULT NULL,
  `innerCurrency` varchar(3) DEFAULT NULL,
  `innerPrice` varchar(20) DEFAULT NULL,
  `userCurrency` varchar(3) DEFAULT NULL,
  `userPrice` varchar(20) DEFAULT NULL,
  `signature` varchar(255) DEFAULT NULL,
  `currPageHash` int(11) DEFAULT NULL,
  `prevPageHash` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`)
);


CREATE TABLE `onepf_repository`.`reviews` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `package` varchar(255) NOT NULL,
  `distributorStoreId` varchar(255) DEFAULT NULL,
  `homeStoreId` varchar(255) DEFAULT NULL,
  `version` varchar(255) DEFAULT NULL,
  `versionCode` int(11) DEFAULT NULL,
  `rating` decimal(4,2) DEFAULT NULL,
  `datetime` datetime DEFAULT NULL,
  `deviceModel` varchar(255) DEFAULT NULL,
  `deviceName` varchar(255) DEFAULT NULL,
  `country` varchar(2) DEFAULT NULL,
  `userName` varchar(255) DEFAULT NULL,
  `title` varchar(255) DEFAULT NULL,
  `textBody` text,
  `reviewUrl` varchar(255) DEFAULT NULL,
  `currPageHash` int(11) DEFAULT NULL,
  `prevPageHash` int(11) DEFAULT NULL,
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

	
	