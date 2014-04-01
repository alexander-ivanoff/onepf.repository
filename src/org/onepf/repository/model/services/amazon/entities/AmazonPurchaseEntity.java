package org.onepf.repository.model.services.amazon.entities;

import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import org.onepf.repository.utils.responsewriter.descriptors.PurchaseDescriptor;

import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public class AmazonPurchaseEntity extends AmazonDBEntity<PurchaseDescriptor> implements AmazonGenericFields{

    public static final String FIELD_ID = "id";
    public static final String FIELD_VERSION = "version";
    public static final String FIELD_BUILD = "build";
    public static final String FIELD_LAST_UPDATE = "last-updated";
    public static final String FIELD_DEVICE_MODEL = "device-model";
    public static final String FIELD_DEVICE_NAME = "device-name";
    public static final String FIELD_COUNTRY = "country";
    public static final String FIELD_USER_PRICE = "user-price";
    public static final String FIELD_USER_CURRENCY = "user-currency";
    public static final String FIELD_INNER_PRICE = "inner-price";
    public static final String FIELD_INNER_CURRENCY = "inner-currency";

    public static final String TABLE_NAME = "purchases";

    public AmazonPurchaseEntity() {
        super(TABLE_NAME);
    }


    public AmazonPurchaseEntity(Map<String, AttributeValue> item) {
        super(TABLE_NAME, item);
    }

    public AmazonPurchaseEntity withPackageName(String packageName) {
        put(FIELD_PACKAGE_NAME, packageName);
        return this;
    }

    public AmazonPurchaseEntity withId(String id) {
        put(FIELD_ID, id);
        return this;
    }

    public AmazonPurchaseEntity withInnerCurrency(String innerCurrency) {
        put(FIELD_INNER_CURRENCY, innerCurrency);
        return this;
    }

    public AmazonPurchaseEntity withInnerPrice(String innerPrice) {
        put(FIELD_INNER_PRICE, innerPrice);
        return this;
    }

    public AmazonPurchaseEntity withUserCurrency(String userCurrency) {
        put(FIELD_USER_CURRENCY, userCurrency);
        return this;
    }

    public AmazonPurchaseEntity withUserPrice(String userPrice) {
        put(FIELD_USER_PRICE, userPrice);
        return this;
    }

    public AmazonPurchaseEntity withCountry(String country) {
        put(FIELD_COUNTRY, country);
        return this;
    }

    public AmazonPurchaseEntity withDeviceName(String deviceName) {
        put(FIELD_DEVICE_NAME, deviceName);
        return this;
    }

    public AmazonPurchaseEntity withDeviceModel(String deviceModel) {
        put(FIELD_DEVICE_MODEL, deviceModel);
        return this;
    }
    
    public AmazonPurchaseEntity withVersion(String version) {
        put(FIELD_VERSION, version);
        return this;
    }

    public AmazonPurchaseEntity withDateTime(String dateTime) {
        put(FIELD_DATE_TIME, dateTime);
        return this;
    }

    public AmazonPurchaseEntity withLastUpdate(String lastUpdateTime) {
        put(FIELD_LAST_UPDATE, lastUpdateTime);
        return this;
    }

    public AmazonPurchaseEntity withBuild(long build) {
        put(FIELD_BUILD, build);
        return this;
    }

    public PurchaseDescriptor getDescriptor() {
        return getDescriptor(item);
    }


    public PurchaseDescriptor getDescriptor(Map<String, AttributeValue> item) {
        PurchaseDescriptor purchaseDescriptor = new PurchaseDescriptor();
        purchaseDescriptor.packageName = getString(item, FIELD_PACKAGE_NAME);
        purchaseDescriptor.dateTime = getString(item, FIELD_DATE_TIME);
        purchaseDescriptor.lastUpdate = getString(item, FIELD_LAST_UPDATE);
        purchaseDescriptor.build = getInt(item, FIELD_BUILD);
        purchaseDescriptor.version = getString(item, FIELD_VERSION);
        purchaseDescriptor.country = getString(item, FIELD_COUNTRY);
        purchaseDescriptor.deviceModel = getString(item, FIELD_DEVICE_MODEL);
        purchaseDescriptor.deviceName= getString(item, FIELD_DEVICE_NAME);
        purchaseDescriptor.innerPrice = getString(item, FIELD_INNER_PRICE);
        purchaseDescriptor.innerCurrency = getString(item, FIELD_INNER_CURRENCY);
        purchaseDescriptor.userPrice = getString(item, FIELD_USER_PRICE);
        purchaseDescriptor.userCurrency = getString(item, FIELD_USER_CURRENCY);
        purchaseDescriptor.id = getString(item, FIELD_ID);
        return  purchaseDescriptor;
    }

}
