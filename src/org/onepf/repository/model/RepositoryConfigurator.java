package org.onepf.repository.model;

import org.onepf.repository.model.auth.AppstoreAuthenticator;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.model.services.filesystem.FilesystemOptions;
import org.onepf.repository.model.services.filesystem.FilesystemStorageService;
import org.onepf.repository.model.services.mysql.SqlDataService;
import org.onepf.repository.model.services.mysql.SqlOptions;

import javax.servlet.ServletContext;

/**
 * Created by ivanoff on 14.03.14.
 */
public class RepositoryConfigurator {

    private static final String authPropertiesPath = "/WEB-INF/auth/authToken.properties";

    private static volatile RepositoryFactory repositoryFactory;
    private static volatile AppstoreAuthenticator appstoreAuthenticator;

    private static volatile DataService dataService;
    private static volatile StorageService storageService;



    public static synchronized RepositoryFactory getRepositoryFactory(ServletContext context) {
        if (repositoryFactory == null) {
            dataService = new SqlDataService(new SqlOptions());
            //storageService = new AmazonStorageService(new AmazonOptions());
            storageService = new FilesystemStorageService(new FilesystemOptions(context));
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
