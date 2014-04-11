package org.onepf.repository.model.services.amazon;

import com.amazonaws.regions.Regions;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.model.services.StorageServiceOptions;

import javax.servlet.ServletContext;
import java.util.Properties;

/**
 * Implementation of StorageServiceOptions for AmazonStorageService
 *
 * @author Alexander Ivanoff on 25.03.14.
 */
public class AmazonOptions implements StorageServiceOptions {

    public static final String SERVICE_NAME= "amazon";

    private static final String PROPERTY_CREDENTIALS_FILE_PATH= "amazon_credentials";
    private static final String PROPERTY_REGION= "amazon_region";
    private static final String PROPERTY_BUCKET= "amazon_bucket";

    public String bucket = null;
    public String credentialsFile = null;
    public Regions region = null;


    public AmazonOptions(ServletContext context, Properties props) {
        credentialsFile = props.getProperty(PROPERTY_CREDENTIALS_FILE_PATH);
        credentialsFile = context.getRealPath(credentialsFile);
        bucket = props.getProperty(PROPERTY_BUCKET);
        region = Regions.fromName(props.getProperty(PROPERTY_REGION));
        if (credentialsFile == null || bucket == null || region == null) {
            throw new IllegalArgumentException("configuration file is not completed");
        }
    }

    @Override
    public StorageService createStorageService() {
        return new AmazonStorageService(this);
    }
}
