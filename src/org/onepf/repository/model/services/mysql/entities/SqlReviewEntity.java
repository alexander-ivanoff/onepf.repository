package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.api.responsewriter.descriptors.ReviewDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public class SqlReviewEntity extends SqlDBEntity {

    public static final String FIELD_ID = "id";
    public static final String FIELD_PACKAGE_NAME = "package";
    public static final String FIELD_VERSION = "version";
    public static final String FIELD_BUILD = "build";
    public static final String FIELD_LAST_UPDATE = "lastUpdate";
    public static final String FIELD_DEVICE_MODEL = "deviceModel";
    public static final String FIELD_DEVICE_NAME = "deviceName";
    public static final String FIELD_COUNTRY = "country";
    public static final String FIELD_STARS = "stars";
    public static final String FIELD_USER_NAME = "userName";
    public static final String FIELD_USER_URL = "userUrl";
    public static final String FIELD_TITLE = "title";
    public static final String FIELD_TEXT_BODY = "textBody";



    public SqlReviewEntity() {
        super();
    }

    public SqlReviewEntity(Map<String, String> item) {
        super(item);
    }

    public SqlReviewEntity withId(String id) {
        put(FIELD_ID, id);
        return this;
    }

    public SqlReviewEntity withPackageName(String packageName) {
        put(FIELD_PACKAGE_NAME, packageName);
        return this;
    }

    public SqlReviewEntity withCountry(String country) {
        put(FIELD_COUNTRY, country);
        return this;
    }

    public SqlReviewEntity withDeviceName(String deviceName) {
        put(FIELD_DEVICE_NAME, deviceName);
        return this;
    }

    public SqlReviewEntity withDeviceModel(String deviceModel) {
        put(FIELD_DEVICE_MODEL, deviceModel);
        return this;
    }
    
    public SqlReviewEntity withVersion(String version) {
        put(FIELD_VERSION, version);
        return this;
    }

    public SqlReviewEntity withLastUpdate(long lastUpdateTime) {
        put(FIELD_LAST_UPDATE, lastUpdateTime);
        return this;
    }

    public SqlReviewEntity withBuild(long build) {
        put(FIELD_BUILD, build);
        return this;
    }


    public static ReviewDescriptor getDescriptor(ResultSet item) throws SQLException {
        ReviewDescriptor descriptor = new ReviewDescriptor();
        descriptor.packageName = item.getString(FIELD_PACKAGE_NAME);
        descriptor.lastUpdate = item.getString(FIELD_LAST_UPDATE);
        descriptor.build = item.getInt(FIELD_BUILD);
        descriptor.version = item.getString(FIELD_VERSION);
        descriptor.country = item.getString(FIELD_COUNTRY);
        descriptor.deviceModel = item.getString(FIELD_DEVICE_MODEL);
        descriptor.deviceName= item.getString(FIELD_DEVICE_NAME);
        descriptor.stars = item.getInt(FIELD_STARS);
        descriptor.userName = item.getString(FIELD_USER_NAME);
        descriptor.userUrl = item.getString(FIELD_USER_URL);
        descriptor.title = item.getString(FIELD_TITLE);
        descriptor.body = item.getString(FIELD_TEXT_BODY);
        descriptor.currPageHash = item.getInt(FIELD_CURR_PAGE_HASH);
        descriptor.prevPageHash = item.getInt(FIELD_PREV_PAGE_HASH);
        return  descriptor;
    }


}
