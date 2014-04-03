package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.appstorelooter.LastUpdateDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public class SqlLastUpdateEntity extends SqlDBEntity {

    public static final String FIELD_APPSTORE_ID = "appstoreId";
    public static final String FIELD_LAST_UPDATE_HASH= "lastUpdateHash";
    public static final String FIELD_LAST_UPDATE_DATETIME = "lastUpdateDateTime";
    public static final String FIELD_LAST_UPDATE_OFFSET = "lastUpdateOffset";


    public SqlLastUpdateEntity() {
        super();
    }

    public SqlLastUpdateEntity(Map<String, String> item) {
        super(item);
    }


    public SqlLastUpdateEntity withAppstoreId(String appstoreId) {
        put(FIELD_APPSTORE_ID, appstoreId);
        return this;
    }
    public SqlLastUpdateEntity withHash(String hash) {
        put(FIELD_LAST_UPDATE_HASH, hash);
        return this;
    }

    public SqlLastUpdateEntity withDateTime(String datetime) {
        put(FIELD_LAST_UPDATE_DATETIME, datetime);
        return this;
    }

    public SqlLastUpdateEntity withOffset(String offset) {
        put(FIELD_LAST_UPDATE_OFFSET, offset);
        return this;
    }


    public static LastUpdateDescriptor getDescriptor(ResultSet item ) throws SQLException {
        LastUpdateDescriptor lastUpdateDescriptor = new LastUpdateDescriptor();
        lastUpdateDescriptor.appstoreId = item.getString(FIELD_APPSTORE_ID);
        lastUpdateDescriptor.lastResponseHash = item.getString(FIELD_LAST_UPDATE_HASH);
        lastUpdateDescriptor.lastResponseDatetime = item.getString(FIELD_LAST_UPDATE_DATETIME);
        lastUpdateDescriptor.prevOffset = item.getString(FIELD_LAST_UPDATE_OFFSET);
        return lastUpdateDescriptor;
    }


}
