package org.onepf.repository.model.services.amazon;

import com.amazonaws.regions.Regions;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.DataServiceOptions;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.model.services.StorageServiceOptions;

/**
 * Created by ivanoff on 25.03.14.
 */
public class AmazonOptions implements DataServiceOptions, StorageServiceOptions {

    // TODO maybe move all setting to separate .properties file
    public String appstoreTable = "appstores";
    public String packageTable = "packages";
    public String purchaseTable = "purchases";
    public String downloadTable = "downloads";

    public String bucket = "onepf.repository";

    public String credentialsFile = "/org/onepf/repository/model/services/amazon/AwsCredentials.properties";

    public Regions region = Regions.US_WEST_2;

    @Override
    public DataService createDataService() {
        return new AmazonDataService(this);
    }

    @Override
    public StorageService createStorageService() {
        return new AmazonStorageService(this);
    }
}
