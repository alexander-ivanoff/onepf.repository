package org.onepf.repository.model.services.amazon;

import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.auth.ClasspathPropertiesFileCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.GetObjectRequest;
import com.amazonaws.services.s3.model.ObjectMetadata;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.s3.model.S3Object;
import org.onepf.repository.model.services.StorageException;
import org.onepf.repository.model.services.StorageObject;
import org.onepf.repository.model.services.StorageService;

import java.io.InputStream;

/**
 * Created by ivanoff on 25.03.14.
 */
public class AmazonStorageService implements StorageService {

    public static class AmazonStorageObject implements StorageObject {

        S3Object s3Object;

        protected AmazonStorageObject(S3Object s3Object) {
            this.s3Object = s3Object;
        }

        @Override
        public InputStream asStream() throws StorageException {
            return s3Object.getObjectContent();
        }

        @Override
        public long size() throws StorageException {
            return s3Object.getObjectMetadata().getContentLength();
        }
    }

    private AmazonS3 amazonS3;

    private AmazonOptions options;

    public AmazonStorageService(AmazonOptions options) {

        this.options = options;

        // load amazon credentials and region
        AWSCredentialsProvider awsCredentialsProvider = new ClasspathPropertiesFileCredentialsProvider(this.options.credentialsFile);
        Region region = Region.getRegion(this.options.region);

        // init amazon s3 service
        amazonS3 = new AmazonS3Client(awsCredentialsProvider);
        amazonS3.setRegion(region);
    }

    @Override
    public void storeObject(String objectKey, InputStream is, long contentLength) throws StorageException {

        long time = System.currentTimeMillis();
        ObjectMetadata metadata = new ObjectMetadata();
        metadata.setContentLength(contentLength);
        PutObjectRequest putRequest = new PutObjectRequest(options.bucket, objectKey, is, metadata);
        amazonS3.putObject(putRequest);
    }

    @Override
    public StorageObject getObject(String objectKey) throws StorageException {
        return new AmazonStorageObject(amazonS3.getObject(new GetObjectRequest(options.bucket, objectKey)));
    }

}
