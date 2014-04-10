package org.onepf.repository.model;

import org.onepf.repository.model.auth.AppstoreAuthenticator;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;

/**
 *
 * Factory to create Handlers with given DataService and StorageService
 *
* @author Alexander Ivanoff on 12.03.14.
 */
public class RepositoryFactory {

    private DataService dataService;
    private StorageService storageService;

    public RepositoryFactory(DataService dataService, StorageService storageService) {
        this.dataService = dataService;
        this.storageService = storageService;
    }


    public DownloadObjectRequestHandler createDownloadHandler() {
        return new DownloadObjectRequestHandler(dataService, storageService);
    }

    public UploadAppdfRequestHandler createAppDFFileHandler() {
        return new UploadAppdfRequestHandler(dataService, storageService);
    }

    public GetApplicationsRequestHandler createApplicationsHandler() {
        return new GetApplicationsRequestHandler(dataService, storageService);
    }

    public GetPurchasesRequestHandler createPurchasesHandler() {
        return new GetPurchasesRequestHandler(dataService, storageService);
    }

    public GetDownloadsRequestHandler createDownloadsHandler() {
        return new GetDownloadsRequestHandler(dataService, storageService);
    }

    public AppstoreAuthenticator createAuthenticator() {
        return  new AppstoreAuthenticator(dataService, storageService);
    }

    public GetReviewsRequestHandler createReviewsHandler() {
        return new GetReviewsRequestHandler(dataService, storageService);
    }

    public DataService getDataService() {
        return dataService;
    }

    public StorageService getStorageService() {
        return storageService;
    }
}
