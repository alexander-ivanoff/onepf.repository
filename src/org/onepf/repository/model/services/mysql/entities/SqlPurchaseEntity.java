package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.api.responsewriter.descriptors.PurchaseDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public class SqlPurchaseEntity extends SqlDBEntity {

    public static final String FIELD_ID = "id"; //PK
    public static final String FIELD_PACKAGE_NAME = "package"; // FK(applications)
    public static final String FIELD_DATE_TIME = "opDate";
    public static final String FIELD_VERSION = "version";
    public static final String FIELD_BUILD = "build";
    public static final String FIELD_LAST_UPDATE = "lastUpdate";
    public static final String FIELD_DEVICE_MODEL = "deviceModel";
    public static final String FIELD_DEVICE_NAME = "deviceName";
    public static final String FIELD_COUNTRY = "country";
    public static final String FIELD_USER_PRICE = "userPrice";
    public static final String FIELD_USER_CURRENCY = "userCurrency";
    public static final String FIELD_INNER_PRICE = "innerPrice";
    public static final String FIELD_INNER_CURRENCY = "innerCurrency";


    public SqlPurchaseEntity() {
        super();
    }

    public SqlPurchaseEntity(Map<String, String> item) {
        super(item);
    }

    public SqlPurchaseEntity withPackageName(String packageName) {
        put(FIELD_PACKAGE_NAME, packageName);
        return this;
    }

    public SqlPurchaseEntity withId(String id) {
        put(FIELD_ID, id);
        return this;
    }

    public SqlPurchaseEntity withInnerCurrency(String innerCurrency) {
        put(FIELD_INNER_CURRENCY, innerCurrency);
        return this;
    }

    public SqlPurchaseEntity withInnerPrice(String innerPrice) {
        put(FIELD_INNER_PRICE, innerPrice);
        return this;
    }

    public SqlPurchaseEntity withUserCurrency(String userCurrency) {
        put(FIELD_USER_CURRENCY, userCurrency);
        return this;
    }

    public SqlPurchaseEntity withUserPrice(String userPrice) {
        put(FIELD_USER_PRICE, userPrice);
        return this;
    }

    public SqlPurchaseEntity withCountry(String country) {
        put(FIELD_COUNTRY, country);
        return this;
    }

    public SqlPurchaseEntity withDeviceName(String deviceName) {
        put(FIELD_DEVICE_NAME, deviceName);
        return this;
    }

    public SqlPurchaseEntity withDeviceModel(String deviceModel) {
        put(FIELD_DEVICE_MODEL, deviceModel);
        return this;
    }
    
    public SqlPurchaseEntity withVersion(String version) {
        put(FIELD_VERSION, version);
        return this;
    }

    public SqlPurchaseEntity withDateTime(long dateTime) {
        put(FIELD_DATE_TIME, dateTime);
        return this;
    }

    public SqlPurchaseEntity withLastUpdate(long lastUpdateTime) {
        put(FIELD_LAST_UPDATE, lastUpdateTime);
        return this;
    }

    public SqlPurchaseEntity withBuild(long build) {
        put(FIELD_BUILD, build);
        return this;
    }


    public static PurchaseDescriptor getDescriptor(ResultSet item) throws SQLException {
        PurchaseDescriptor descriptor = new PurchaseDescriptor();
        descriptor.packageName = item.getString(FIELD_PACKAGE_NAME);
        descriptor.dateTime = item.getString(FIELD_DATE_TIME);
        descriptor.lastUpdate = item.getString(FIELD_LAST_UPDATE);
        descriptor.build = item.getInt(FIELD_BUILD);
        descriptor.version = item.getString(FIELD_VERSION);
        descriptor.country = item.getString(FIELD_COUNTRY);
        descriptor.deviceModel = item.getString(FIELD_DEVICE_MODEL);
        descriptor.deviceName= item.getString(FIELD_DEVICE_NAME);
        descriptor.innerPrice = item.getString(FIELD_INNER_PRICE);
        descriptor.innerCurrency = item.getString(FIELD_INNER_CURRENCY);
        descriptor.userPrice = item.getString(FIELD_USER_PRICE);
        descriptor.userCurrency = item.getString(FIELD_USER_CURRENCY);
        descriptor.id = item.getString(FIELD_ID);
        descriptor.currPageHash = item.getInt(FIELD_CURR_PAGE_HASH);
        descriptor.prevPageHash = item.getInt(FIELD_PREV_PAGE_HASH);
        return  descriptor;
    }

}
