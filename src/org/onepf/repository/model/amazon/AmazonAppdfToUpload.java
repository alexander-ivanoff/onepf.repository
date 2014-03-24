package org.onepf.repository.model.amazon;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.model.PutItemRequest;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.ObjectMetadata;
import com.amazonaws.services.s3.model.PutObjectRequest;
import org.onepf.appdf.model.Application;
import org.onepf.appdf.parser.AppdfFileParser;
import org.onepf.appdf.parser.ParseResult;
import org.onepf.repository.model.AppdfToUpload;
import org.onepf.repository.model.FileType;
import org.onepf.repository.model.amazon.db.AmazonAppEntity;
import org.onepf.repository.model.auth.AppstoreDescriptor;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Created by ivanoff on 12.03.14.
 */
public class AmazonAppdfToUpload extends AppdfToUpload {

    private AmazonS3 amazonConnector;
    private AmazonDynamoDB amazonDynamoDB;
    private AmazonRepositoryFactory.RepositoryOptions repositoryOptions;

    public AmazonAppdfToUpload(AmazonServices amazonServices, AmazonRepositoryFactory.RepositoryOptions repositoryOptions) {
        this.amazonConnector = amazonServices.getAmazonS3();
        this.amazonDynamoDB = amazonServices.getAmazonDynamoDB();
        this.repositoryOptions = repositoryOptions;
    }

    @Override
    public void processFile(File file, String developersContact, AppstoreDescriptor appstoreDescriptor) throws IOException {
        AppdfFileParser parser = new AppdfFileParser(file);
        ParseResult parseResult = parser.parse();

        //Default application
        Application application = parseResult.getApplication();

        final String packageName = application.getPackageName();

        long time = System.currentTimeMillis();

        String appdfS3Key = buildAmazonS3Key(packageName, FileType.APPDF.addExtention(packageName));
        String descrS3Key = buildAmazonS3Key(packageName, FileType.DESCRIPTION.addExtention(packageName));
        sendAppDFFile(appdfS3Key, file);
        sendDescription(descrS3Key, parseResult.getFile());


        AmazonAppEntity appEntity = new AmazonAppEntity()
                .withPackageName(packageName)
                .withLastUpdate(System.currentTimeMillis())
                .withLastReview(System.currentTimeMillis()) // TODO what is last-review time
                .withAppdf(appdfS3Key)
                .withDescription(descrS3Key)
                .withDevelopersContact(developersContact)
                .withAppstore(appstoreDescriptor.appstoreName);

        time = System.currentTimeMillis();
        PutItemRequest itemRequest = new PutItemRequest().withTableName(repositoryOptions.packageTable).withItem(appEntity.getItem());
        amazonDynamoDB.putItem(itemRequest);
        System.out.println("Put Request to DynamoDB: " + (System.currentTimeMillis() - time)); // TODO move to Log4J


    }

    private boolean sendDescription(String descrS3key, ZipFile zipFile) throws IOException {

        Enumeration<? extends ZipEntry> entries = zipFile.entries();

        while ( entries.hasMoreElements()){
            ZipEntry elem = entries.nextElement();
            String name = elem.getName();

            if (name.equals(APPDF_DESCRIPTION_FILE_NAME)) {
                extractFile(descrS3key, zipFile, elem);
                return true;
            }
        }
        return false;
    }

    public void extractFile(String apkS3Key, ZipFile zipFile, ZipEntry zipEntry) throws IOException {
        InputStream zis = null;
        try {
            zis = zipFile.getInputStream(zipEntry);
            sendToAmazon(apkS3Key, zipEntry.getSize(), zis);
        } finally {
            if (zis != null) {
                zis.close();
            }
        }
    }

    public void sendAppDFFile(String appdfKey, File appdfFile) throws IOException {
        InputStream zis = null;
        try {
            zis = new FileInputStream(appdfFile);
            sendToAmazon(appdfKey, appdfFile.length(), zis);
        } finally {
            if (zis != null) {
                zis.close();
            }
        }
    }

    public void sendToAmazon(String amazonKey, long contentLength, InputStream is) {
        long time = System.currentTimeMillis();
        ObjectMetadata metadata = new ObjectMetadata();
        metadata.setContentLength(contentLength);
        PutObjectRequest putRequest = new PutObjectRequest(repositoryOptions.bucket, amazonKey, is, metadata);
        amazonConnector.putObject(putRequest);
        System.out.println(Thread.currentThread().getName() + ">  Uploading to S3: " + amazonKey + " = " + (System.currentTimeMillis() - time)); // TODO move to Log4J

    }

    private String buildAmazonS3Key(String packageName, String name) {
        return packageName + "/" + name;
    }

}
