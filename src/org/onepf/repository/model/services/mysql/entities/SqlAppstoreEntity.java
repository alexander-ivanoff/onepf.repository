package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.model.auth.AppstoreDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * Created by ivanoff on 18.03.14.
 */
public class SqlAppstoreEntity extends SqlDBEntity {

    public static final String FIELD_AUTH_TOKEN = "authToken";
    public static final String FIELD_APPSTORE_ID = "appstoreId";
    public static final String FIELD_OPENAEP_URL = "openaepUrl";
    public static final String FIELD_PUBLICKEY = "publicKey";
    public static final String FIELD_DESCRIPTION = "description";


    public SqlAppstoreEntity() {
        super();
    }

    public SqlAppstoreEntity(Map<String, String> item) {
        super(item);
    }

    public SqlAppstoreEntity withAuthToken(String authToken) {
        put(FIELD_AUTH_TOKEN, authToken);
        return this;
    }

    public SqlAppstoreEntity withAppstoreId(String appstoreId) {
        put(FIELD_APPSTORE_ID, appstoreId);
        return this;
    }
    public SqlAppstoreEntity withOpenaepUrl(String openaepUrl) {
        put(FIELD_OPENAEP_URL, openaepUrl);
        return this;
    }

    public SqlAppstoreEntity withPublickKey(String publickKey) {
        put(FIELD_PUBLICKEY, publickKey);
        return this;
    }

    public SqlAppstoreEntity withDescription(String descriptionS3key) {
        put(FIELD_DESCRIPTION, descriptionS3key);
        return this;
    }

    public String getDescription() {
        return getString(FIELD_DESCRIPTION);
    }


    public static AppstoreDescriptor getDescriptor(ResultSet item ) throws SQLException {
        AppstoreDescriptor appDescriptor = new AppstoreDescriptor();
        appDescriptor.authToken = item.getString(FIELD_AUTH_TOKEN);
        appDescriptor.appstoreId = item.getString(FIELD_APPSTORE_ID);
        appDescriptor.description = item.getString(FIELD_DESCRIPTION);
        appDescriptor.openaepUrl = item.getString(FIELD_OPENAEP_URL);
        appDescriptor.publickKey = item.getString(FIELD_PUBLICKEY);
        return  appDescriptor;
    }


}
