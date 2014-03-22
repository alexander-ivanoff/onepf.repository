package org.onepf.repository.model.amazon.db;

import com.amazonaws.services.dynamodbv2.model.*;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public class AmazonAppEntity extends AmazonDBEntity{

    public static final String FIELD_REPOSITORY = "repository";
    public static final String FIELD_PACKAGE_NAME = "packageName";
    public static final String FIELD_LAST_UPDATE = "lastUpdate";
    public static final String FIELD_LAST_REVIEW = "lastReview";
    public static final String FIELD_APPDF = "appdf";
    public static final String FIELD_APK = "apk";
    public static final String FIELD_DESCRIPTION = "description";

    public static final String INDEX_LAST_UPDATE = "lastUpdate-index";
    public static final String INDEX_LAST_REVIEW = "lastReview-index";

    public static final String DEFAULT_REPOSITORY = "default";


    public AmazonAppEntity() {
        super();
        put(FIELD_REPOSITORY, DEFAULT_REPOSITORY);
    }

    public AmazonAppEntity(Map<String, AttributeValue> item) {
        super(item);
    }


    public AmazonAppEntity withRepository(String repository) {
        put(FIELD_REPOSITORY, repository);
        return this;
    }

    public AmazonAppEntity withPackageName(String packageName) {
        put(FIELD_PACKAGE_NAME, packageName);
        return this;
    }

    public AmazonAppEntity withLastUpdate(long lastUpdateTime) {
        put(FIELD_LAST_UPDATE, lastUpdateTime);
        return this;
    }

    public AmazonAppEntity withLastReview(long lastReviewTime) {
        put(FIELD_LAST_REVIEW, lastReviewTime);
        return this;
    }

    public AmazonAppEntity withAppdf(String appdfS3key) {
        put(FIELD_APPDF, appdfS3key);
        return this;
    }

    public AmazonAppEntity withDescription(String descriptionS3key) {
        put(FIELD_DESCRIPTION, descriptionS3key);
        return this;
    }

    public AmazonAppEntity withApks(Collection<String> apkS3keys) {
        put(FIELD_APK, apkS3keys);
        return this;
    }

    public List<String> getApks() {
        return getStringList(FIELD_APK);
    }

    public String getAppdf() {
        return getString(FIELD_APPDF);
    }

    public String getDescription() {
        return getString(FIELD_DESCRIPTION);
    }

    public ApplicationDescriptor getDescriptor() {
        return getDescriptor(item);
    }


    public static ApplicationDescriptor getDescriptor(Map<String, AttributeValue> item ) {
        ApplicationDescriptor appDescriptor = new ApplicationDescriptor();
        appDescriptor.packageName = getString(item, FIELD_PACKAGE_NAME);
        appDescriptor.build = 0; //TODO no information in DB
        appDescriptor.version = "Unknown"; //TODO no information in DB
        appDescriptor.updatedTime = getLong(item, FIELD_LAST_UPDATE);
        appDescriptor.reviewedTime = getLong(item, FIELD_LAST_REVIEW);
        return  appDescriptor;
    }

    public static GetItemRequest getRequestByPackageName(String packageName) {
        AmazonAppEntity keyEntity = new AmazonAppEntity().withPackageName(packageName);

        return new GetItemRequest().withKey(keyEntity.getItem());
    }

    public static QueryRequest searchRequestByLastUpdatedTime(long lastUpdatedTime) {
        Condition hashKeyCondition = new Condition()
                .withComparisonOperator(ComparisonOperator.EQ.toString())
                .withAttributeValueList(new AttributeValue().withS(DEFAULT_REPOSITORY));

        Condition rangeKeyCondition = new Condition()
                .withComparisonOperator(ComparisonOperator.GT.toString())
                .withAttributeValueList(new AttributeValue().withN(String.valueOf(lastUpdatedTime)));

        Map<String, Condition> keyConditions = new HashMap<String, Condition>();
        keyConditions.put(AmazonAppEntity.FIELD_REPOSITORY, hashKeyCondition);
        keyConditions.put(AmazonAppEntity.FIELD_LAST_UPDATE, rangeKeyCondition);

        return new QueryRequest()
                .withIndexName(INDEX_LAST_UPDATE)
                .withKeyConditions(keyConditions);
    }

}
