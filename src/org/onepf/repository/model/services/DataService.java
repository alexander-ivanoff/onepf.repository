package org.onepf.repository.model.services;

import org.onepf.repository.appstorelooter.LastUpdateDescriptor;
import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.DownloadDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.PurchaseDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.ReviewDescriptor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Abstraction of service to handle different data.
 * Actually Database wrapper.
 *
 * @see org.onepf.repository.model.services.mysql.SqlDataService
 * @see org.onepf.repository.model.services.amazon.AmazonDataService
 * @author Alexander Ivanoff
 */
public interface DataService {


    // TODO refactoring: move method in different req    uests (Maybe Entities), here should be only generic requests

    void store(ApplicationDescriptor applicationDescriptor) throws DataException;

    void saveLastUpdate(LastUpdateDescriptor lastUpdateDescriptor) throws DataException;
    void addDownload(DownloadDescriptor downloadDescriptor) throws DataException;

    List<ApplicationDescriptor> getApplicationsLog() throws DataException;
    List<ApplicationDescriptor> getApplicationsLog(String packageName, int page) throws DataException;
    Map<String,AppstoreDescriptor> getAppstores() throws DataException;

    /**
     * @param appstoreId
     * @return List of LastUpdateDescriptor for specified appstore ID
     * @throws DataException
     *
     * @see org.onepf.repository.appstorelooter.LastUpdateDescriptor
     * @see org.onepf.repository.model.services.DataException
     */
    List<LastUpdateDescriptor> getLastUpdate(String appstoreId) throws DataException;

    /**
     * @param packageName
     * @param hash - MD5 calculated hash of appdf file
     * @return List of ApplicationDescriptor with specified package name and MD5 hash
     * @throws DataException
     *
     * @see org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor
     * @see org.onepf.repository.model.services.DataException
     */
    List<ApplicationDescriptor> getApplicationByHash(String packageName, String hash) throws DataException;

    ArrayList<DownloadDescriptor> getDownloads(String packageName, long updateTime) throws DataException;
    ArrayList<PurchaseDescriptor> getPurchases(String packageName, long updateTime) throws DataException;
    ArrayList<ReviewDescriptor> getReviews(String packageName, int pageHash) throws DataException;
}
