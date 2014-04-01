package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.model.services.DataException;
import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Created by ivanoff on 18.03.14.
 */
public class SqlDownloadEntity extends SqlDBEntity<DownloadDescriptor> implements GenericFields{

    public static final String FIELD_DATE_TIME = "opDate"; // DynamoDB Range key
    public static final String FIELD_VERSION = "version";
    public static final String FIELD_BUILD = "build";
    public static final String FIELD_DEVICE_MODEL = "deviceModel";
    public static final String FIELD_DEVICE_NAME = "deviceName";
    public static final String FIELD_COUNTRY = "country";
    public static final String FIELD_IS_UPDATE = "isUpdate";

    public static final String TABLE_NAME = "downloads";

    public SqlDownloadEntity() {
        super(TABLE_NAME);
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

    public SqlDownloadEntity withDateTime(String dateTime) {
        put(FIELD_DATE_TIME, dateTime);
        return this;
    }

    public SqlDownloadEntity withLastUpdate(String lastUpdateTime) {
        put(FIELD_LAST_UPDATE, lastUpdateTime);
        return this;
    }

    public SqlDownloadEntity withBuild(long build) {
        put(FIELD_BUILD, build);
        return this;
    }


    public DownloadDescriptor getDescriptor(ResultSet item) throws DataException {
        try {
            DownloadDescriptor descriptor = new DownloadDescriptor();
            descriptor.packageName = item.getString(FIELD_PACKAGE_NAME);
            descriptor.dateTime = item.getString(FIELD_DATE_TIME);
            descriptor.lastUpdate = item.getString(FIELD_LAST_UPDATE);
            descriptor.build = item.getInt(FIELD_BUILD);
            descriptor.version = item.getString(FIELD_VERSION);
            descriptor.country = item.getString(FIELD_COUNTRY);
            descriptor.deviceModel = item.getString(FIELD_DEVICE_MODEL);
            descriptor.deviceName= item.getString(FIELD_DEVICE_NAME);
            descriptor.isUpdate = item.getString(FIELD_IS_UPDATE);
            descriptor.currPageHash = item.getInt(FIELD_CURR_PAGE_HASH);
            descriptor.prevPageHash = item.getInt(FIELD_PREV_PAGE_HASH);
            return  descriptor;
        } catch (SQLException e) {
            throw new DataException(e);
        }
    }

}
