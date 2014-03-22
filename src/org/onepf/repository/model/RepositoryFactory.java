package org.onepf.repository.model;

/**
 * Created by ivanoff on 12.03.14.
 */
public abstract class RepositoryFactory {

    abstract public ObjectToDownload createFileRequester();
    abstract public AppdfToUpload createAppDFFileHandler();
    abstract public ApplicationsList createApplicationsList();
    abstract public PurchasesList createPurchasesList();
    abstract public DownloadsList createDownloadsList();

}
