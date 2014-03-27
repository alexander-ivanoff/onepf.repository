package org.onepf.repository.model.services.amazon;

import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.auth.ClasspathPropertiesFileCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.PutItemRequest;
import com.amazonaws.services.dynamodbv2.model.QueryRequest;
import com.amazonaws.services.dynamodbv2.model.QueryResult;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.amazon.entities.AmazonAppEntity;
import org.onepf.repository.model.services.amazon.entities.AmazonAppstoreEntity;
import org.onepf.repository.model.services.amazon.entities.AmazonDownloadEntity;
import org.onepf.repository.model.services.amazon.entities.AmazonPurchaseEntity;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.PurchaseDescriptor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by ivanoff on 25.03.14.
 */
public class AmazonDataService implements DataService {


    private AmazonDynamoDB amazonDynamoDB;

    private AmazonOptions options;

    public AmazonDataService(AmazonOptions options) {

        this.options = options;

        // load amazon credentials and region
        AWSCredentialsProvider awsCredentialsProvider = new ClasspathPropertiesFileCredentialsProvider(this.options.credentialsFile);
        Region region = Region.getRegion(this.options.region);

        // init amazon dynamoDB service
        amazonDynamoDB = new AmazonDynamoDBClient(awsCredentialsProvider);
        amazonDynamoDB.setRegion(region);
    }

    @Override
    public void store(ApplicationDescriptor applicationDescriptor) {
        AmazonAppEntity appEntity = new AmazonAppEntity()
                .withPackageName(applicationDescriptor.packageName)
                .withLastUpdate(applicationDescriptor.lastUpdated)
                .withDevelopersContact(applicationDescriptor.developerContact)
                .withAppstore(applicationDescriptor.appstoreId);

        PutItemRequest itemRequest = new PutItemRequest().withTableName(options.packageTable).withItem(appEntity.getItem());
        amazonDynamoDB.putItem(itemRequest);
    }

    @Override
    public List<ApplicationDescriptor> getApplications() {

        QueryRequest queryRequest = AmazonAppEntity.searchRequestByLastUpdatedTime("1").withTableName(options.packageTable);
        QueryResult result = amazonDynamoDB.query(queryRequest);

        ArrayList<ApplicationDescriptor> apps = new ArrayList<ApplicationDescriptor>();
        for (Map<String, AttributeValue> item : result.getItems()) {
            apps.add(AmazonAppEntity.getDescriptor(item));
        }
        return apps;
    }

    @Override
    public Map<String, AppstoreDescriptor> getAppstores() {

        QueryRequest queryRequest = AmazonAppstoreEntity.searchRequestAllAppstores().withTableName(options.appstoreTable);
        QueryResult result = amazonDynamoDB.query(queryRequest);

        Map<String, AppstoreDescriptor> apps = new HashMap<String, AppstoreDescriptor>();
        AppstoreDescriptor appstore = null;
        for (Map<String, AttributeValue> item : result.getItems()) {
            appstore = AmazonAppstoreEntity.getDescriptor(item);
            apps.put(appstore.authToken, appstore);
        }
        return apps;
    }

    @Override
    public ArrayList<DownloadDescriptor> getDownloads(String packageName, long updateTime) {
        QueryRequest queryRequest = AmazonDownloadEntity.queryRequestByPackageAndDate(packageName, updateTime)
                .withTableName(options.downloadTable);

        QueryResult result = amazonDynamoDB.query(queryRequest);

        ArrayList<DownloadDescriptor> downloads = new ArrayList<DownloadDescriptor>();
        for (Map<String, AttributeValue> item : result.getItems()) {
            downloads.add(AmazonDownloadEntity.getDescriptor(item));
        }
        return downloads;
    }

    @Override
    public ArrayList<PurchaseDescriptor> getPurchases(String packageName, long updateTime) {
        QueryRequest queryRequest = AmazonPurchaseEntity.queryRequestByPackageAndDate(packageName,updateTime)
                .withTableName(options.purchaseTable);

        QueryResult result = amazonDynamoDB.query(queryRequest);

        ArrayList<PurchaseDescriptor> purchases = new ArrayList<PurchaseDescriptor>();
        for (Map<String, AttributeValue> item : result.getItems()) {
            purchases.add(AmazonPurchaseEntity.getDescriptor(item));
        }
        return purchases;
    }
}