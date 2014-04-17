package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.api.responsewriter.entity.DownloadEntity;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * DownloadEntity entity for MySQL.
 *
 * Create DownloadDescriptor from ResultSet.
 *
 * @see org.onepf.repository.api.responsewriter.descriptors.DownloadDescriptor
 * @author Alexander Ivanoff
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
    public static final String TABLE_NAME = "downloads";

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


    public static DownloadEntity getDescriptor(ResultSet item) throws SQLException {
        DownloadEntity descriptor = new DownloadEntity();
        descriptor.setPackageName(item.getString(FIELD_PACKAGE_NAME));
        descriptor.setDateTime(item.getString(FIELD_DATE_TIME));
        descriptor.setLastUpdate(item.getString(FIELD_LAST_UPDATE));
        descriptor.setBuild(item.getInt(FIELD_BUILD));
        descriptor.setVersion(item.getString(FIELD_VERSION));
        descriptor.setCountry(item.getString(FIELD_COUNTRY));
        descriptor.setDeviceModel(item.getString(FIELD_DEVICE_MODEL));
        descriptor.setDeviceName(item.getString(FIELD_DEVICE_NAME));
        descriptor.setIsUpdate(item.getString(FIELD_IS_UPDATE));
        descriptor.setCurrPageHash(item.getInt(FIELD_CURR_PAGE_HASH));
        descriptor.setPrevPageHash(item.getInt(FIELD_PREV_PAGE_HASH));
        return  descriptor;
    }

}
