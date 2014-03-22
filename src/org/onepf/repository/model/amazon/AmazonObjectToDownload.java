package org.onepf.repository.model.amazon;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.model.GetItemRequest;
import com.amazonaws.services.dynamodbv2.model.GetItemResult;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.*;
import org.onepf.repository.model.ObjectToDownload;
import org.onepf.repository.model.amazon.db.AmazonAppEntity;

import java.io.FileNotFoundException;
import java.io.InputStream;

/**
 * Created by ivanoff on 12.03.14.
 */
public class AmazonObjectToDownload extends ObjectToDownload {

    private AmazonS3 amazonS3;
    private AmazonDynamoDB amazonDynamoDB;

    private AmazonRepositoryFactory.RepositoryOptions repositoryOptions;
    private ObjectOptions options;

    private AmazonAppEntity appEntity;
    private S3Object amazonS3Object;

    public AmazonObjectToDownload(AmazonServices amazonServices, AmazonRepositoryFactory.RepositoryOptions repositoryOptions) {
        this.amazonS3 = amazonServices.getAmazonS3();
        this.amazonDynamoDB = amazonServices.getAmazonDynamoDB();
        this.repositoryOptions = repositoryOptions;
    }

    @Override
    public InputStream getAsStream() throws FileNotFoundException {
        if (amazonS3Object == null) {
            throw new FileNotFoundException("No such file in package");
        }
        return amazonS3Object.getObjectContent();
    }

    @Override
    public String getName() throws FileNotFoundException {
        if (amazonS3Object == null) {
            throw new FileNotFoundException("No such file in package");
        }
        String name = options.packageName + '.' + options.fileType.extention();
        return name;
    }

    @Override
    public int getSize() throws FileNotFoundException {
        if (amazonS3Object == null) {
            throw new FileNotFoundException("No such file in package");
        }
        return (int) amazonS3Object.getObjectMetadata().getContentLength();
    }

    @Override
    public void init(ObjectOptions objectOptions) throws FileNotFoundException {
        if (objectOptions == null || objectOptions.packageName == null ) {
            throw new FileNotFoundException("Incorrect Options");
        }
        options = objectOptions;

        long time = System.currentTimeMillis();

        GetItemRequest getItemRequest = AmazonAppEntity.getRequestByPackageName(objectOptions.packageName)
                .withTableName(repositoryOptions.packageTable);

        GetItemResult result = amazonDynamoDB.getItem(getItemRequest);

        appEntity = new AmazonAppEntity(result.getItem());

        String amazonS3Key = null;
        switch (options.fileType) {
            case APK:
                amazonS3Key = appEntity.getApks().get(0);
                break;
            case APPDF:
                amazonS3Key = appEntity.getAppdf();
                break;
            case DESCRIPTION:
                amazonS3Key = appEntity.getDescription();
                break;
        }
        amazonS3Object = amazonS3.getObject(new GetObjectRequest(repositoryOptions.bucket, amazonS3Key));

        System.out.println("Object to download time: " + (System.currentTimeMillis() - time));  // TODO move to Log4J
    }

}
