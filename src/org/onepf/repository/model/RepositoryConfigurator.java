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
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

/**
 *This class generate repositoryFactory with specified data and storage services based on parameters provided in web.xml.
 *
 * @see RepositoryFactory
 * @author Alexander Ivanov
 */
public class RepositoryConfigurator {


    private static final String PARAM_CONFIGURATION_FILE = "configuration";

    private static final String PARAM_DATASERVICE = "dataService";
    private static final String PARAM_STORAGESERVICE = "storageService";

    private static final String DATASERVICE_MYSQL= "mysql";


    private static volatile RepositoryFactory repositoryFactory;
    private static volatile AppstoreAuthenticator appstoreAuthenticator;

    private static volatile DataService dataService;
    private static volatile StorageService storageService;


    /**
     * Initialize and return RepositoryFactory.
     *
     * @param context - servlet context
     * @return RepositoryFactory with linked DataService and StorageService
     */
    public static synchronized RepositoryFactory getRepositoryFactory(ServletContext context) {
        if (repositoryFactory == null) {
            DataServiceOptions dsOptions = null;
            StorageServiceOptions ssOptions = null;
            String configurationFile = context.getInitParameter(PARAM_CONFIGURATION_FILE);

            if (configurationFile != null) {
                Properties configProps = new Properties();
                configurationFile = context.getRealPath(configurationFile);
                try {
                    configProps.load(new FileInputStream(configurationFile));
                } catch (IOException e) {
                    throw new IllegalArgumentException("Can't load configuration file.");
                }

                dsOptions = getDataServiceOptions(configProps) ;
                ssOptions = getStorageServiceOptions(configProps, context);
            }

            if (dsOptions == null || ssOptions == null) {
                throw new IllegalArgumentException("Can't configure services. check your configuration file.");
            }

            dataService = dsOptions.createDataService();
            storageService = ssOptions.createStorageService();
            repositoryFactory = new RepositoryFactory(dataService, storageService);
        }
        return repositoryFactory;
    }

    /**
     * Initialize and return AppstoreAuthenticator (also initialize RepositoryFactory).
     *
     * @param context - servlet context
     * @return AppstoreAuthenticator
     */
    public static synchronized AppstoreAuthenticator getAppstoreAuthenticator(ServletContext context){
        if (appstoreAuthenticator == null) {
            appstoreAuthenticator = getRepositoryFactory(context).createAuthenticator();
        }
        return appstoreAuthenticator;
    }

    private static DataServiceOptions getDataServiceOptions(Properties props) {
        DataServiceOptions options = null;
        String serviceName = props.getProperty(PARAM_DATASERVICE);
        if (SqlOptions.SERVICE_NAME.equals(serviceName)) {
            options = new SqlOptions(props);
        }
        return options;
    }

    private static StorageServiceOptions getStorageServiceOptions(Properties props, ServletContext context) {
        StorageServiceOptions options = null;
        String serviceName = props.getProperty(PARAM_STORAGESERVICE);
        if (FilesystemOptions.SERVICE_NAME.equals(serviceName)) {
            options = new FilesystemOptions(context);
        } else {
            options = new AmazonOptions(context, props);
        }
        return options;
    }

}
