package org.onepf.repository.model.services.amazon.entities;

import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.ComparisonOperator;
import com.amazonaws.services.dynamodbv2.model.Condition;
import com.amazonaws.services.dynamodbv2.model.QueryRequest;
import org.onepf.repository.model.auth.AppstoreDescriptor;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public class AmazonAppstoreEntity extends AmazonDBEntity<AppstoreDescriptor> {

    public static final String FIELD_REPOSITORY = "repository";
    public static final String FIELD_AUTH_TOKEN = "authtoken";
    public static final String FIELD_APPSTORE_NAME = "appstorename";
    public static final String FIELD_IP_DNS = "ip-dns";
    public static final String FIELD_PUBLICKEY = "publickey";
    public static final String FIELD_DESCRIPTION = "description";

    public static final String DEFAULT_REPOSITORY = "default";

    public static final String TABLE_NAME = "appstores";



    public AmazonAppstoreEntity() {
        super(TABLE_NAME);
        put(FIELD_REPOSITORY, DEFAULT_REPOSITORY);
    }

    public AmazonAppstoreEntity(Map<String, AttributeValue> item) {
        super(TABLE_NAME, item);
    }


    public AmazonAppstoreEntity withRepository(String repository) {
        put(FIELD_REPOSITORY, repository);
        return this;
    }

    public AmazonAppstoreEntity withAuthToken(String authToken) {
        put(FIELD_AUTH_TOKEN, authToken);
        return this;
    }

    public AmazonAppstoreEntity withAppstoreName(String appstoreName) {
        put(FIELD_APPSTORE_NAME, appstoreName);
        return this;
    }
    public AmazonAppstoreEntity withIpDns(String ipDns) {
        put(FIELD_IP_DNS, ipDns);
        return this;
    }

    public AmazonAppstoreEntity withPublickKey(String publickKey) {
        put(FIELD_PUBLICKEY, publickKey);
        return this;
    }

    public AmazonAppstoreEntity withDescription(String descriptionS3key) {
        put(FIELD_DESCRIPTION, descriptionS3key);
        return this;
    }

    public String getDescription() {
        return getString(FIELD_DESCRIPTION);
    }

    public AppstoreDescriptor getDescriptor() {
        return getDescriptor(item);
    }


    public AppstoreDescriptor getDescriptor(Map<String, AttributeValue> item ) {
        AppstoreDescriptor appDescriptor = new AppstoreDescriptor();
        appDescriptor.authToken = getString(item, FIELD_AUTH_TOKEN);
        appDescriptor.appstoreId = getString(item, FIELD_APPSTORE_NAME);
        appDescriptor.description = getString(item, FIELD_DESCRIPTION);
        appDescriptor.openaepUrl = getString(item, FIELD_IP_DNS);
        appDescriptor.publickKey = getString(item, FIELD_PUBLICKEY);
        return  appDescriptor;
    }


    public static QueryRequest searchRequestAllAppstores() {
        Condition hashKeyCondition = new Condition()
                .withComparisonOperator(ComparisonOperator.EQ.toString())
                .withAttributeValueList(new AttributeValue().withS(DEFAULT_REPOSITORY));

        Map<String, Condition> keyConditions = new HashMap<String, Condition>();
        keyConditions.put(AmazonAppstoreEntity.FIELD_REPOSITORY, hashKeyCondition);

        return new QueryRequest()
                .withKeyConditions(keyConditions);
    }


}
