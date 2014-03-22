package org.onepf.repository.model.amazon;

import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.auth.ClasspathPropertiesFileCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;

/**
 * Created by ivanoff on 18.03.14.
 */
class AmazonServices {

    private AmazonS3 amazonS3;
    private AmazonDynamoDB amazonDynamoDB;

    public AmazonServices(AmazonRepositoryFactory.RepositoryOptions repositoryOptions) {
        // load amazon credentials and region
        AWSCredentialsProvider awsCredentialsProvider = new ClasspathPropertiesFileCredentialsProvider(repositoryOptions.credentialsFile);
        Region region = Region.getRegion(repositoryOptions.region);

        // init amazon s3 service
        amazonS3 = new AmazonS3Client(awsCredentialsProvider);
        amazonS3.setRegion(region);

        // init amazon dynamoDB service
        amazonDynamoDB = new AmazonDynamoDBClient(awsCredentialsProvider);
        amazonDynamoDB.setRegion(region);

    }

    public AmazonS3 getAmazonS3() {
        return amazonS3;
    }

    public AmazonDynamoDB getAmazonDynamoDB() {
        return amazonDynamoDB;
    }
}
