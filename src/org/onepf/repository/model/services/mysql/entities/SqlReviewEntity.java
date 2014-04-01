package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.model.services.DataException;
import org.onepf.repository.utils.responsewriter.descriptors.ReviewDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Created by ivanoff on 18.03.14.
 */
public class SqlReviewEntity extends SqlDBEntity<ReviewDescriptor> implements GenericFields {

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


    public static final String TABLE_NAME = "reviews";

    public SqlReviewEntity() {
        super(TABLE_NAME);
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

    @Override
    public ReviewDescriptor getDescriptor(ResultSet rs) throws DataException {
        try {
            ReviewDescriptor descriptor = new ReviewDescriptor();
            descriptor.packageName = rs.getString(FIELD_PACKAGE_NAME);
            descriptor.lastUpdate = rs.getString(FIELD_LAST_UPDATE);
            descriptor.build = rs.getInt(FIELD_BUILD);
            descriptor.version = rs.getString(FIELD_VERSION);
            descriptor.country = rs.getString(FIELD_COUNTRY);
            descriptor.deviceModel = rs.getString(FIELD_DEVICE_MODEL);
            descriptor.deviceName = rs.getString(FIELD_DEVICE_NAME);
            descriptor.stars = rs.getInt(FIELD_STARS);
            descriptor.userName = rs.getString(FIELD_USER_NAME);
            descriptor.userUrl = rs.getString(FIELD_USER_URL);
            descriptor.title = rs.getString(FIELD_TITLE);
            descriptor.body = rs.getString(FIELD_TEXT_BODY);
            descriptor.currPageHash = rs.getInt(FIELD_CURR_PAGE_HASH);
            descriptor.prevPageHash = rs.getInt(FIELD_PREV_PAGE_HASH);
            return descriptor;
        } catch (SQLException e) {
            throw new DataException(e);
        }
    }


}
