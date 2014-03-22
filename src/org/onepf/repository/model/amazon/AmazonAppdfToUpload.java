package org.onepf.repository.model.amazon;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.model.PutItemRequest;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.ObjectMetadata;
import com.amazonaws.services.s3.model.PutObjectRequest;
import org.onepf.appdf.model.ApkFilesInfo;
import org.onepf.appdf.model.Application;
import org.onepf.appdf.parser.AppdfFileParser;
import org.onepf.appdf.parser.ParseResult;
import org.onepf.repository.model.AppdfToUpload;
import org.onepf.repository.model.FileType;
import org.onepf.repository.model.amazon.db.AmazonAppEntity;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
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
    public void processFile(File file) throws IOException {
        AppdfFileParser parser = new AppdfFileParser(file);
        ParseResult parseResult = parser.parse();

        //Default application
        Application application = parseResult.getApplication();

        final String packageName = application.getPackageName();
        final List<ApkFilesInfo.ApkFile> apkFiles = application.getFilesInfo().getApkFiles();
        List<String> apkFilenames= new ArrayList<String>(apkFiles.size());
        for (ApkFilesInfo.ApkFile apkfile : apkFiles) {
            apkFilenames.add(apkfile.getFileName());
        }
        long time = System.currentTimeMillis();
        List<String> apkS3Keys = extractFiles(packageName, parseResult.getFile(), apkFiles);

        String appdfS3Key = buildAmazonS3Key(packageName, FileType.APPDF.addExtention(packageName));
        sendAppDFFile(appdfS3Key, file);

        System.out.println(System.currentTimeMillis() - time);

        AmazonAppEntity appEntity = new AmazonAppEntity()
                .withPackageName(packageName)
                .withLastUpdate(System.currentTimeMillis())
                .withLastReview(System.currentTimeMillis()) // TODO what is last-review time
                .withAppdf(appdfS3Key)
                .withApks(apkS3Keys)
                .withDescription(buildAmazonS3Key(packageName, FileType.DESCRIPTION.addExtention(packageName)));

        time = System.currentTimeMillis();
        PutItemRequest itemRequest = new PutItemRequest().withTableName(repositoryOptions.packageTable).withItem(appEntity.getItem());
        amazonDynamoDB.putItem(itemRequest);
        System.out.println("Put Request to DynamoDB: " + (System.currentTimeMillis() - time)); // TODO move to Log4J


    }

    private List<String> extractFiles(String packageName, ZipFile zipFile, List<ApkFilesInfo.ApkFile> apkFiles) throws IOException {
        List<String> apkS3Keys = new ArrayList<String>();
        Enumeration<? extends ZipEntry> entries = zipFile.entries();
        List<String> apkFilenames = new ArrayList<String>(apkFiles.size());
        for (ApkFilesInfo.ApkFile apkFile: apkFiles) {
            apkFilenames.add(apkFile.getFileName());
        }

        int apkIndex = 0;
        while ( entries.hasMoreElements()){
            ZipEntry elem = entries.nextElement();
            String name = elem.getName();
            String apkS3Key = null;

            if (name.equals(APPDF_DESCRIPTION_FILE_NAME)) {
                extractFile(buildAmazonS3Key( packageName, FileType.DESCRIPTION.addExtention(packageName)), zipFile, elem);
            } else if (apkFilenames.contains(name)) {
                apkS3Key = buildAmazonS3Key(packageName, FileType.APK.addExtention(packageName + (++apkIndex != 1 ? "." + apkIndex : "")));
                apkS3Keys.add(apkS3Key);
                extractFile(apkS3Key, zipFile, elem);
            }
        }
        return  apkS3Keys;
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
        System.out.println(Thread.currentThread().getName() + ">  Uploading to S3: " + amazonKey);
        long time = System.currentTimeMillis();
        ObjectMetadata metadata = new ObjectMetadata();
        metadata.setContentLength(contentLength);
        PutObjectRequest putRequest = new PutObjectRequest(repositoryOptions.bucket, amazonKey, is, metadata);
        amazonConnector.putObject(putRequest);
        System.out.println(Thread.currentThread().getName() + ">  Uploading finished: " + amazonKey + " = " + (System.currentTimeMillis() - time));

    }

    private String buildAmazonS3Key(String packageName, String name) {
        return packageName + "/" + name;
    }

}
