package org.onepf.repository.model.amazon.db;

import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.ComparisonOperator;
import com.amazonaws.services.dynamodbv2.model.Condition;
import com.amazonaws.services.dynamodbv2.model.QueryRequest;
import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public class AmazonDownloadEntity extends AmazonDBEntity{
    /*
        <download>
            <package>com.softspb.geo_game</package>
            <datetime>2013-02-22T23:30:30Z</datetime>
            <version>1.0</version>
            <build>50</build>
            <last-updated>20130227T110425</last-updated>
            <device-model>SHW-M130K</device-model>
            <device-name>Samsung GT-i9082</device-name>
            <country>US</country>
            <is-update>no</is-update>
        </download>
     */

    public static final String FIELD_PACKAGE_NAME = "package"; // DynamoDB Hash key
    public static final String FIELD_DATE_TIME = "datetime"; // DynamoDB Range key
    public static final String FIELD_VERSION = "version";
    public static final String FIELD_BUILD = "build";
    public static final String FIELD_LAST_UPDATE = "last-updated";
    public static final String FIELD_DEVICE_MODEL = "device-model";
    public static final String FIELD_DEVICE_NAME = "device-name";
    public static final String FIELD_COUNTRY = "country";
    public static final String FIELD_IS_UPDATE = "is-update";


    public AmazonDownloadEntity(Map<String, AttributeValue> item) {
        super(item);
    }

    public AmazonDownloadEntity withPackageName(String packageName) {
        put(FIELD_PACKAGE_NAME, packageName);
        return this;
    }

    public AmazonDownloadEntity withIsUpdate(String isUpdate) {
        put(FIELD_IS_UPDATE, isUpdate);
        return this;
    }

    public AmazonDownloadEntity withCountry(String country) {
        put(FIELD_COUNTRY, country);
        return this;
    }

    public AmazonDownloadEntity withDeviceName(String deviceName) {
        put(FIELD_DEVICE_NAME, deviceName);
        return this;
    }

    public AmazonDownloadEntity withDeviceModel(String deviceModel) {
        put(FIELD_DEVICE_MODEL, deviceModel);
        return this;
    }
    
    public AmazonDownloadEntity withVersion(String version) {
        put(FIELD_VERSION, version);
        return this;
    }

    public AmazonDownloadEntity withDateTime(long dateTime) {
        put(FIELD_DATE_TIME, dateTime);
        return this;
    }

    public AmazonDownloadEntity withLastUpdate(long lastUpdateTime) {
        put(FIELD_LAST_UPDATE, lastUpdateTime);
        return this;
    }

    public AmazonDownloadEntity withBuild(long build) {
        put(FIELD_BUILD, build);
        return this;
    }


    public DownloadDescriptor getDescriptor() {
        return getDescriptor(item);
    }

    public static DownloadDescriptor getDescriptor(Map<String, AttributeValue> item) {
        DownloadDescriptor downloadDescriptor = new DownloadDescriptor();
        downloadDescriptor.packageName = getString(item, FIELD_PACKAGE_NAME);
        downloadDescriptor.dateTime = getLong(item, FIELD_DATE_TIME);
        downloadDescriptor.lastUpdate = getLong(item, FIELD_LAST_UPDATE);
        downloadDescriptor.build = getInt(item, FIELD_BUILD);
        downloadDescriptor.version = getString(item, FIELD_VERSION);
        downloadDescriptor.country = getString(item, FIELD_COUNTRY);
        downloadDescriptor.deviceModel = getString(item, FIELD_DEVICE_MODEL);
        downloadDescriptor.deviceName= getString(item, FIELD_DEVICE_NAME);
        downloadDescriptor.isUpdate = getString(item, FIELD_IS_UPDATE);
        return  downloadDescriptor;
    }

    public static QueryRequest queryRequestByPackageAndDate(String packageName, long dateTime) {
        Condition hashKeyCondition = new Condition()
                .withComparisonOperator(ComparisonOperator.EQ.toString())
                .withAttributeValueList(new AttributeValue().withS(packageName));

        Condition rangeKeyCondition = new Condition()
                .withComparisonOperator(ComparisonOperator.GT.toString())
                .withAttributeValueList(new AttributeValue().withN(String.valueOf(dateTime)));

        Map<String, Condition> keyConditions = new HashMap<String, Condition>();
        keyConditions.put(AmazonDownloadEntity.FIELD_PACKAGE_NAME, hashKeyCondition);
        keyConditions.put(AmazonDownloadEntity.FIELD_DATE_TIME, rangeKeyCondition);

        return new QueryRequest()
                .withKeyConditions(keyConditions);
    }

}
