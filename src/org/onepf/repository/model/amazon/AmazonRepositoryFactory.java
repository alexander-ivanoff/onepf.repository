package org.onepf.repository.model.amazon;

import com.amazonaws.regions.Regions;
import org.onepf.repository.model.*;
import org.onepf.repository.model.auth.AppstoreAuthenticator;

import javax.servlet.ServletContext;

/**
 * Created by ivanoff on 12.03.14.
 */
public class AmazonRepositoryFactory extends RepositoryFactory {

    public static class RepositoryOptions {

        // TODO maybe move all setting to separate .properties file
        public String appstoreTable = "appstores";
        public String packageTable = "packages";
        public String purchaseTable = "purchases";
        public String downloadTable = "downloads";

        public String bucket = "onepf.repository";

        public String credentialsFile = "/org/onepf/repository/model/amazon/AwsCredentials.properties";

        public Regions region = Regions.US_WEST_2;
    }


    private AmazonServices amazonServices;
    private RepositoryOptions repositoryOptions;

    public AmazonRepositoryFactory(ServletContext context) {
        repositoryOptions = new RepositoryOptions();
        long time = System.currentTimeMillis();
        amazonServices = new AmazonServices(repositoryOptions);
        System.out.println("Init amazon connection time: " + (System.currentTimeMillis() - time));  // TODO move to Log4J
    }

    @Override
    public ObjectToDownload createFileRequester() {
        return new AmazonObjectToDownload(amazonServices, repositoryOptions);
    }

    @Override
    public AppdfToUpload createAppDFFileHandler() {
        return new AmazonAppdfToUpload(amazonServices, repositoryOptions);
    }

    @Override
    public ApplicationsList createApplicationsList() {
        return new AmazonApplicationsList(amazonServices, repositoryOptions);
    }

    @Override
    public PurchasesList createPurchasesList() {
        return new AmazonPurchasesList(amazonServices, repositoryOptions);
    }

    @Override
    public DownloadsList createDownloadsList() {
        return new AmazonDownloadsList(amazonServices, repositoryOptions);
    }

    @Override
    public AppstoreAuthenticator createAuthenticator() {
        return new AmazonAppstoreAuthenticator(amazonServices, repositoryOptions);
    }
}
