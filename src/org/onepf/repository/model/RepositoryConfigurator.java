package org.onepf.repository.model;

import org.onepf.repository.model.auth.AppstoreAuthenticator;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.DataServiceOptions;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.model.services.StorageServiceOptions;
import org.onepf.repository.model.services.amazon.AmazonOptions;
import org.onepf.repository.model.services.filesystem.FilesystemOptions;
import org.onepf.repository.model.services.mysql.SqlOptions;

import javax.servlet.ServletContext;

/**
 * Created by ivanoff on 14.03.14.
 */
public class RepositoryConfigurator {

    private static final String authPropertiesPath = "/WEB-INF/auth/authToken.properties";

    private static final String PARAM_DATASERVICE = "dataService";
    private static final String PARAM_DATASERVICE_URL = "dbUrl";
    private static final String PARAM_STORAGESERVICE = "storageService";

    private static final String DATASERVICE_MYSQL= "mysql";

    private static final String STORAGESERVICE_FILESYSTEM = "filesystem";
    private static final String STORAGESERVICE_AMAZONS3 = "amazons3";

    private static volatile RepositoryFactory repositoryFactory;
    private static volatile AppstoreAuthenticator appstoreAuthenticator;

    private static volatile DataService dataService;
    private static volatile StorageService storageService;


    public static synchronized RepositoryFactory getRepositoryFactory(ServletContext context) {
        if (repositoryFactory == null) {
            DataServiceOptions dsOptions = null;
            StorageServiceOptions ssOptions = null;

            if (DATASERVICE_MYSQL.equals(context.getInitParameter(PARAM_DATASERVICE))) {
                SqlOptions sqlOptions = new SqlOptions();
                String dsUrl = context.getInitParameter(PARAM_DATASERVICE_URL);
                if (dsUrl != null) {
                    sqlOptions.dbUrl = dsUrl;
                    //init dataservice only if it is mysql and dbUrl is present in web.xml
                    dsOptions = sqlOptions;
                }
            }

            if (STORAGESERVICE_FILESYSTEM.equals(context.getInitParameter(PARAM_STORAGESERVICE))) {
                ssOptions = new FilesystemOptions(context);
            } else if (STORAGESERVICE_AMAZONS3.equals(context.getInitParameter(PARAM_STORAGESERVICE))) {
                ssOptions = new AmazonOptions();
            }

            if (dsOptions == null || ssOptions == null) {
                throw new IllegalArgumentException("Can't configure services. check your web.xml config.");
            }

            dataService = dsOptions.createDataService();
            storageService = ssOptions.createStorageService();
            repositoryFactory = new RepositoryFactory(dataService, storageService);
        }
        return repositoryFactory;
    }

    public static synchronized AppstoreAuthenticator getAppstoreAuthenticator(ServletContext context){
        if (appstoreAuthenticator == null) {
            appstoreAuthenticator = getRepositoryFactory(context).createAuthenticator();
        }
        return appstoreAuthenticator;
    }


}
