package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * ApplicationEntity entity for MySQL.
 * <p/>
 * Create ApplicationDescriptor from ResultSet.
 *
 * @author Alexander Ivanoff
 */
public class SqlAppEntity extends SqlDBEntity {

    public static final String TABLE_NAME = "applications";

    public static final String FIELD_ID = "id";
    public static final String FIELD_PACKAGE_NAME = "package";
    public static final String FIELD_APPSTORE_ID = "appstoreId";
    public static final String FIELD_LAST_UPDATE = "datetime";
    public static final String FIELD_APPDF = "appdfLink";
    public static final String FIELD_VERSION = "version";
    public static final String FIELD_BUILD = "versionCode";
    public static final String FIELD_HASH = "hash";

    public static ApplicationEntity getDescriptor(ResultSet item) throws SQLException {
        ApplicationEntity descriptor = new ApplicationEntity();
        descriptor.setPackageName(item.getString(FIELD_PACKAGE_NAME));
        descriptor.setBuild(item.getInt(FIELD_BUILD));
        descriptor.setVersion(item.getString(FIELD_VERSION));
        descriptor.setAppstoreId(item.getString(FIELD_APPSTORE_ID));
        descriptor.setAppdfLink(item.getString(FIELD_APPDF));

        descriptor.setCurrPageHash(item.getInt(FIELD_CURR_PAGE_HASH));
        descriptor.setPrevPageHash(item.getInt(FIELD_PREV_PAGE_HASH));
        descriptor.setAppdfHash(item.getString(FIELD_HASH));
        return descriptor;
    }

}
