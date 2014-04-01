package org.onepf.repository.model.services.amazon;

import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.auth.ClasspathPropertiesFileCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.model.*;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.model.services.DBEntity;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.amazon.entities.*;
import org.onepf.repository.utils.responsewriter.descriptors.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by ivanoff on 25.03.14.
 */
public class AmazonDataService implements DataService<Map<String, AttributeValue>, Map<String, AttributeValue>> {


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
    public ArrayList<DownloadDescriptor> getDownloads(String packageName, int currPageHash) throws DataException {
        return null;
    }

    @Override
    public ArrayList<PurchaseDescriptor> getPurchases(String packageName, int currPageHash) throws DataException {
        return null;
    }


    @Override
    public <T extends AbstractDescriptor> ArrayList<T> query(DBEntity<T, Map<String, AttributeValue>, Map<String, AttributeValue>> entity, String selection, String[] selectionArgs, String order) throws DataException {
        QueryRequest queryRequest = queryRequestByPackageAndDate()
                .withTableName(options.purchaseTable);

        QueryResult result = amazonDynamoDB.query(queryRequest);

        ArrayList<PurchaseDescriptor> purchases = new ArrayList<PurchaseDescriptor>();
        for (Map<String, AttributeValue> item : result.getItems()) {
            purchases.add(AmazonPurchaseEntity.getDescriptor(item));
        }
        return purchases;
    }

    @Override
    public void storeApplication(ApplicationDescriptor applicationDescriptor) throws DataException {
        AmazonAppEntity appEntity = new AmazonAppEntity()
                .withPackageName(applicationDescriptor.packageName)
                .withLastUpdate(applicationDescriptor.lastUpdated)
                .withDevelopersContact(applicationDescriptor.developerContact)
                .withAppstore(applicationDescriptor.appstoreId)
                .withAppdf(applicationDescriptor.appdfLink)
                .withDescription(applicationDescriptor.descriptionLink);
        store(appEntity);

    }

    @Override
    public <T extends AbstractDescriptor> void store(DBEntity<T, Map<String, AttributeValue>, Map<String, AttributeValue>> entity) throws DataException {
        PutItemRequest itemRequest = new PutItemRequest().withTableName(entity.getTableName()).withItem(entity.getItem());
        amazonDynamoDB.putItem(itemRequest);
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

    @Override
    public ArrayList<ReviewDescriptor> getReviews(String packageName, int pageHash) throws DataException {
        return null;
    }

    public static QueryRequest queryRequestByPackageAndDate(String packageName, long dateTime) {
        Condition hashKeyCondition = new Condition()
                .withComparisonOperator(ComparisonOperator.EQ.toString())
                .withAttributeValueList(new AttributeValue().withS(packageName));

        Condition rangeKeyCondition = new Condition()
                .withComparisonOperator(ComparisonOperator.GT.toString())
                .withAttributeValueList(new AttributeValue().withN(String.valueOf(dateTime)));

        Map<String, Condition> keyConditions = new HashMap<String, Condition>();
        keyConditions.put(AmazonGenericFields.FIELD_PACKAGE_NAME, hashKeyCondition);
        keyConditions.put(AmazonGenericFields.FIELD_DATE_TIME, rangeKeyCondition);

        return new QueryRequest()
                .withKeyConditions(keyConditions);
    }
}
