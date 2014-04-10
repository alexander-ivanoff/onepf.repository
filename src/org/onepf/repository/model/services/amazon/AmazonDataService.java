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
import org.onepf.repository.appstorelooter.LastUpdateDescriptor;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.amazon.entities.AmazonAppEntity;
import org.onepf.repository.model.services.amazon.entities.AmazonAppstoreEntity;
import org.onepf.repository.model.services.amazon.entities.AmazonDownloadEntity;
import org.onepf.repository.model.services.amazon.entities.AmazonPurchaseEntity;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.DownloadDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.PurchaseDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.ReviewDescriptor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Amazon DynamoDB database wrapper.
 * DON'T USE IT!!! Not working now!!!
 *
 * @author Alexander Ivanoff
 */
@Deprecated
public class AmazonDataService implements DataService {


    // TODO refactoring: move method in different requests (Maybe Entities), here should be only generic requests


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
                .withAppstore(applicationDescriptor.appstoreId)
                .withAppdf(applicationDescriptor.appdfLink)
                .withDescription(applicationDescriptor.descriptionLink);

        PutItemRequest itemRequest = new PutItemRequest().withTableName(options.packageTable).withItem(appEntity.getItem());
        amazonDynamoDB.putItem(itemRequest);
    }

    @Override
    public void saveLastUpdate(LastUpdateDescriptor lastUpdateDescriptor) throws DataException {

    }

    @Override
    public void addDownload(DownloadDescriptor downloadDescriptor) throws DataException {

    }

    @Override
    public List<ApplicationDescriptor> getApplicationsLog() {

        QueryRequest queryRequest = AmazonAppEntity.searchRequestByLastUpdatedTime("1").withTableName(options.packageTable);
        QueryResult result = amazonDynamoDB.query(queryRequest);

        ArrayList<ApplicationDescriptor> apps = new ArrayList<ApplicationDescriptor>();
        for (Map<String, AttributeValue> item : result.getItems()) {
            apps.add(AmazonAppEntity.getDescriptor(item));
        }
        return apps;
    }

    @Override
    public List<ApplicationDescriptor> getApplicationsLog(String packageName, int pageHash) {

        QueryRequest queryRequest = AmazonAppEntity.searchRequestByPackageName(packageName).withTableName(options.packageTable);
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
            apps.put(appstore.repositoryAccessToken, appstore);
        }
        return apps;
    }

    @Override
    public List<LastUpdateDescriptor> getLastUpdate(String appstoreId) throws DataException {
        return null;
    }

    @Override
    public List<ApplicationDescriptor> getApplicationByHash(String packageName, String hash) throws DataException {
        return null;
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

    @Override
    public ArrayList<ReviewDescriptor> getReviews(String packageName, int pageHash) throws DataException {
        return null;
    }
}
