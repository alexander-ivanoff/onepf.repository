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
import org.onepf.repository.model.FileType;
import org.onepf.repository.model.services.StorageException;
import org.onepf.repository.model.services.StorageService;

import java.io.InputStream;

/**
 * Created by ivanoff on 25.03.14.
 */
public class AmazonStorageService implements StorageService {

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
    public void storeObject(String packageName, InputStream is, FileType fileType, long contentLength) throws StorageException {

        long time = System.currentTimeMillis();
        ObjectMetadata metadata = new ObjectMetadata();
        metadata.setContentLength(contentLength);
        PutObjectRequest putRequest = new PutObjectRequest(options.bucket, amazonS3Key(packageName, fileType), is, metadata);
        amazonS3.putObject(putRequest);
    }

    @Override
    public InputStream getObjectAsStream(String packageName, FileType fileType) throws StorageException {
        String amazonS3Key = amazonS3Key(packageName, fileType);
        S3Object amazonS3Object = amazonS3.getObject(new GetObjectRequest(options.bucket, amazonS3Key));
        return amazonS3Object.getObjectContent();
    }

    @Override
    public long getObjectSize(String packageName, FileType fileType) throws StorageException {
        String amazonS3Key = amazonS3Key(packageName, fileType);
        S3Object amazonS3Object = amazonS3.getObject(new GetObjectRequest(options.bucket, amazonS3Key));
        return amazonS3Object.getObjectMetadata().getContentLength();
    }

    private String amazonS3Key(String packageName, FileType fileType) {
        return  packageName + "/" + fileType.addExtention(packageName);
    }
}
