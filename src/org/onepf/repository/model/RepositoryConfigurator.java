package org.onepf.repository.model;

import org.onepf.repository.model.amazon.AmazonRepositoryFactory;
import org.onepf.repository.model.auth.AppstoreAuthenticator;
import org.onepf.repository.model.auth.SimpleAppstoreAuthenticator;

import javax.servlet.ServletContext;
import java.io.File;

/**
 * Created by ivanoff on 14.03.14.
 */
public class RepositoryConfigurator {

    private static final String authPropertiesPath = "/WEB-INF/auth/authToken.properties";

    private static RepositoryFactory repositoryFactory;
    private static AppstoreAuthenticator appstoreAuthenticator;



    public static synchronized RepositoryFactory getRepositoryFactory(ServletContext context) {
        if (repositoryFactory == null) {
            repositoryFactory = new AmazonRepositoryFactory(context);
        }
        return repositoryFactory;
    }

    public static synchronized AppstoreAuthenticator getAppstoreAuthenticator(ServletContext context){
        if (appstoreAuthenticator == null) {
            File authFile = new File(context.getRealPath(authPropertiesPath));
            appstoreAuthenticator = new SimpleAppstoreAuthenticator(authFile);
        }
        return appstoreAuthenticator;
    }

}
