package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public class SqlAppEntity extends SqlDBEntity {

    public static final String FIELD_PACKAGE_NAME = "package";
    public static final String FIELD_APPSTORE_ID = "appstoreId";
    public static final String FIELD_DEVELOPERS_CONTACT = "devContact";
    public static final String FIELD_LAST_UPDATE = "lastUpdate";
    public static final String FIELD_APPDF = "appdfLink";
    public static final String FIELD_DESCRIPTION = "descrLink";
    public static final String FIELD_VERSION = "version";
    public static final String FIELD_BUILD = "build";


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

    public SqlAppEntity withAppdf(String appdfS3key) {
        put(FIELD_APPDF, appdfS3key);
        return this;
    }

    public SqlAppEntity withDescription(String descriptionS3key) {
        put(FIELD_DESCRIPTION, descriptionS3key);
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

    public String getAppdf() {
        return getString(FIELD_APPDF);
    }

    public String getDescription() {
        return getString(FIELD_DESCRIPTION);
    }

    public static ApplicationDescriptor getDescriptor(ResultSet item) throws SQLException {
        ApplicationDescriptor appDescriptor = new ApplicationDescriptor();
        appDescriptor.packageName = item.getString(FIELD_PACKAGE_NAME);
        appDescriptor.build = item.getInt(FIELD_BUILD); //TODO no information in DB
        appDescriptor.version = item.getString(FIELD_VERSION); //"Unknown"; //TODO no information in DB
        appDescriptor.lastUpdated = item.getString(FIELD_LAST_UPDATE);
        appDescriptor.developerContact = item.getString(FIELD_DEVELOPERS_CONTACT);
        appDescriptor.appstoreId = item.getString(FIELD_APPSTORE_ID);
        return  appDescriptor;
    }

}
