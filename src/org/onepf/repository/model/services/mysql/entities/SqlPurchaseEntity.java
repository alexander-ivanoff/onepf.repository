package org.onepf.repository.model.services.mysql.entities;


import org.onepf.repository.api.responsewriter.entity.PurchaseEntity;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * PurchaseEntity entity for MySQL.
 * <p/>
 * Create PurchaseDescriptor from ResultSet.
 *
 * @author Alexander Ivanoff
 * @see org.onepf.repository.api.responsewriter.descriptors.PurchaseDescriptor
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


    public static PurchaseEntity getDescriptor(ResultSet item) throws SQLException {
        PurchaseEntity descriptor = new PurchaseEntity();
        descriptor.setPackageName(item.getString(FIELD_PACKAGE_NAME));
        descriptor.setDateTime(item.getString(FIELD_DATE_TIME));
        descriptor.setLastUpdate(item.getString(FIELD_LAST_UPDATE));
        descriptor.setBuild(item.getInt(FIELD_BUILD));
        descriptor.setVersion(item.getString(FIELD_VERSION));
        descriptor.setCountry(item.getString(FIELD_COUNTRY));
        descriptor.setDeviceModel(item.getString(FIELD_DEVICE_MODEL));
        descriptor.setDeviceName(item.getString(FIELD_DEVICE_NAME));
        descriptor.setInnerPrice(item.getString(FIELD_INNER_PRICE));
        descriptor.setInnerCurrency(item.getString(FIELD_INNER_CURRENCY));
        descriptor.setUserPrice(item.getString(FIELD_USER_PRICE));
        descriptor.setInnerCurrency(item.getString(FIELD_USER_CURRENCY));
        descriptor.setId(item.getInt(FIELD_ID));
        descriptor.setCurrPageHash(item.getInt(FIELD_CURR_PAGE_HASH));
        descriptor.setPrevPageHash(item.getInt(FIELD_PREV_PAGE_HASH));
        return descriptor;
    }

}
