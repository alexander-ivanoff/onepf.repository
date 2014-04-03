package org.onepf.repository.model.services.mysql.entities;

import org.onepf.repository.model.auth.AppstoreDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Created by ivanoff on 18.03.14.
 */
public class SqlAppstoreEntity extends SqlDBEntity {

    public static final String FIELD_APPSTORE_ACCESS_TOKEN = "appstoreAccessToken";
    public static final String FIELD_REPOSITORY_ACCESS_TOKEN = "repositoryAccessToken";
    public static final String FIELD_APPSTORE_ID = "appstoreId";
    public static final String FIELD_OPENAEP_URL = "openaepUrl";
    public static final String FIELD_PUBLICKEY = "publicKey";
    public static final String FIELD_DESCRIPTION = "description";



    public static AppstoreDescriptor getDescriptor(ResultSet item ) throws SQLException {
        AppstoreDescriptor appDescriptor = new AppstoreDescriptor();
        appDescriptor.appstoreAccessToken = item.getString(FIELD_APPSTORE_ACCESS_TOKEN);
        appDescriptor.repositoryAccessToken = item.getString(FIELD_REPOSITORY_ACCESS_TOKEN);
        appDescriptor.appstoreId = item.getString(FIELD_APPSTORE_ID);
        appDescriptor.description = item.getString(FIELD_DESCRIPTION);
        appDescriptor.openaepUrl = item.getString(FIELD_OPENAEP_URL);
        appDescriptor.publickKey = item.getString(FIELD_PUBLICKEY);
        return  appDescriptor;
    }


}
