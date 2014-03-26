package org.onepf.repository.model.services.mysql.entities;

import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.ComparisonOperator;
import com.amazonaws.services.dynamodbv2.model.Condition;
import com.amazonaws.services.dynamodbv2.model.QueryRequest;
import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public class SqlDownloadEntity extends SqlDBEntity {

    public static final String FIELD_ID = "id";
    public static final String FIELD_PACKAGE_NAME = "package"; // DynamoDB Hash key
    public static final String FIELD_DATE_TIME = "opDate"; // DynamoDB Range key
    public static final String FIELD_VERSION = "version";
    public static final String FIELD_BUILD = "build";
    public static final String FIELD_LAST_UPDATE = "lastUpdate";
    public static final String FIELD_DEVICE_MODEL = "deviceModel";
    public static final String FIELD_DEVICE_NAME = "deviceName";
    public static final String FIELD_COUNTRY = "country";
    public static final String FIELD_IS_UPDATE = "isUpdate";

    public SqlDownloadEntity() {
        super();
    }

    public SqlDownloadEntity(Map<String, String> item) {
        super(item);
    }

    public SqlDownloadEntity withId(String id) {
        put(FIELD_ID, id);
        return this;
    }

    public SqlDownloadEntity withPackageName(String packageName) {
        put(FIELD_PACKAGE_NAME, packageName);
        return this;
    }

    public SqlDownloadEntity withIsUpdate(String isUpdate) {
        put(FIELD_IS_UPDATE, isUpdate);
        return this;
    }

    public SqlDownloadEntity withCountry(String country) {
        put(FIELD_COUNTRY, country);
        return this;
    }

    public SqlDownloadEntity withDeviceName(String deviceName) {
        put(FIELD_DEVICE_NAME, deviceName);
        return this;
    }

    public SqlDownloadEntity withDeviceModel(String deviceModel) {
        put(FIELD_DEVICE_MODEL, deviceModel);
        return this;
    }
    
    public SqlDownloadEntity withVersion(String version) {
        put(FIELD_VERSION, version);
        return this;
    }

    public SqlDownloadEntity withDateTime(long dateTime) {
        put(FIELD_DATE_TIME, dateTime);
        return this;
    }

    public SqlDownloadEntity withLastUpdate(long lastUpdateTime) {
        put(FIELD_LAST_UPDATE, lastUpdateTime);
        return this;
    }

    public SqlDownloadEntity withBuild(long build) {
        put(FIELD_BUILD, build);
        return this;
    }


    public static DownloadDescriptor getDescriptor(ResultSet item) throws SQLException {
        DownloadDescriptor downloadDescriptor = new DownloadDescriptor();
        downloadDescriptor.packageName = item.getString(FIELD_PACKAGE_NAME);
        downloadDescriptor.dateTime = item.getString(FIELD_DATE_TIME);
        downloadDescriptor.lastUpdate = item.getString(FIELD_LAST_UPDATE);
        downloadDescriptor.build = item.getInt(FIELD_BUILD);
        downloadDescriptor.version = item.getString(FIELD_VERSION);
        downloadDescriptor.country = item.getString(FIELD_COUNTRY);
        downloadDescriptor.deviceModel = item.getString(FIELD_DEVICE_MODEL);
        downloadDescriptor.deviceName= item.getString(FIELD_DEVICE_NAME);
        downloadDescriptor.isUpdate = item.getString(FIELD_IS_UPDATE);
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
        keyConditions.put(SqlDownloadEntity.FIELD_PACKAGE_NAME, hashKeyCondition);
        keyConditions.put(SqlDownloadEntity.FIELD_DATE_TIME, rangeKeyCondition);

        return new QueryRequest()
                .withKeyConditions(keyConditions);
    }

}
