package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * Application entity for MySQL.
 *
 * Create ApplicationDescriptor from ResultSet.
 *
 * @see org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor
 * @author Alexander Ivanoff
 */
public class SqlAppEntity extends SqlDBEntity {

    public static final String TABLE_NAME= "applications";

    public static final String FIELD_ID= "id";
    public static final String FIELD_PACKAGE_NAME = "package";
    public static final String FIELD_APPSTORE_ID = "appstoreId";
    public static final String FIELD_DEVELOPERS_CONTACT = "devContact";
    public static final String FIELD_LAST_UPDATE = "lastUpdate";
    public static final String FIELD_APPDF = "appdfLink";
    public static final String FIELD_DESCRIPTION = "descrLink";
    public static final String FIELD_VERSION = "version";
    public static final String FIELD_BUILD = "build";
    public static final String FIELD_HASH = "hash";


    public SqlAppEntity() {
        super();
    }

    public SqlAppEntity(Map<String, String> item) {
        super(item);
    }


    public SqlAppEntity withPackageName(String packageName) {
        put(FIELD_PACKAGE_NAME, packageName);
        return this;
    }

    public SqlAppEntity withLastUpdate(String lastUpdateTime) {
        put(FIELD_LAST_UPDATE, lastUpdateTime);
        return this;
    }

    public SqlAppEntity withVersion(String version) {
        put(FIELD_VERSION, version);
        return this;
    }

    public SqlAppEntity withBuild(String build) {
        put(FIELD_VERSION, build);
        return this;
    }

    public SqlAppEntity withAppdf(String appdfKey) {
        put(FIELD_APPDF, appdfKey);
        return this;
    }

    public SqlAppEntity withDescription(String descriptionkey) {
        put(FIELD_DESCRIPTION, descriptionkey);
        return this;
    }

    public SqlAppEntity withAppstore(String appstoreId) {
        put(FIELD_APPSTORE_ID, appstoreId);
        return this;
    }

    public SqlAppEntity withDevelopersContact(String developersContact) {
        put(FIELD_DEVELOPERS_CONTACT, developersContact);
        return this;
    }

    public SqlAppEntity withHash(String hash) {
        put(FIELD_HASH, hash);
        return this;
    }

    public String getAppdf() {
        return getString(FIELD_APPDF);
    }

    public String getDescription() {
        return getString(FIELD_DESCRIPTION);
    }

    public static ApplicationDescriptor getDescriptor(ResultSet item) throws SQLException {
        ApplicationDescriptor descriptor = new ApplicationDescriptor();
        descriptor.packageName = item.getString(FIELD_PACKAGE_NAME);
        descriptor.build = item.getInt(FIELD_BUILD); //TODO no information in DB
        descriptor.version = item.getString(FIELD_VERSION); //"Unknown"; //TODO no information in DB
        descriptor.lastUpdated = item.getString(FIELD_LAST_UPDATE);
        descriptor.developerContact = item.getString(FIELD_DEVELOPERS_CONTACT);
        descriptor.appstoreId = item.getString(FIELD_APPSTORE_ID);
        descriptor.appdfLink = item.getString(FIELD_APPDF);
        descriptor.descriptionLink = item.getString(FIELD_DESCRIPTION);
        descriptor.currPageHash = item.getInt(FIELD_CURR_PAGE_HASH);
        descriptor.prevPageHash = item.getInt(FIELD_PREV_PAGE_HASH);
        descriptor.appdfHash = item.getString(FIELD_HASH);
        return  descriptor;
    }

}
