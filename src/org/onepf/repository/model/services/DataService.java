package org.onepf.repository.model.services;

import org.onepf.repository.api.responsewriter.entity.*;
import org.onepf.repository.appstorelooter.LastUpdateDescriptor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Abstraction of service to handle different data.
 * Actually Database wrapper.
 *
 * @author Alexander Ivanoff
 * @see org.onepf.repository.model.services.mysql.SqlDataService
 */
public interface DataService {


    // TODO refactoring: move method in different req    uests (Maybe Entities), here should be only generic requests

    void store(ApplicationEntity application) throws DataException;

    void saveLastUpdate(LastUpdateDescriptor lastUpdateDescriptor) throws DataException;

    void addDownload(DownloadEntity download) throws DataException;

    List<ApplicationEntity> getApplicationsLog() throws DataException;

    List<ApplicationEntity> getApplicationsLog(String packageName, int page) throws DataException;

    Map<String, AppstoreEntity> getAppstores() throws DataException;

    /**
     * @param appstoreId
     * @return List of LastUpdateDescriptor for specified appstore ID
     * @throws DataException
     * @see org.onepf.repository.appstorelooter.LastUpdateDescriptor
     * @see org.onepf.repository.model.services.DataException
     */
    List<LastUpdateDescriptor> getLastUpdate(String appstoreId) throws DataException;

    /**
     * @param hash        - MD5 calculated hash of appdf file
     * @return List of ApplicationDescriptor with specified package name and MD5 hash
     * @throws DataException
     * @see org.onepf.repository.api.responsewriter.entity.ApplicationEntity
     * @see org.onepf.repository.model.services.DataException
     */
    List<ApplicationEntity> getApplicationByHash(String packageName, String hash) throws DataException;

    ArrayList<DownloadEntity> getDownloads(String homeStoreId, long page) throws DataException;

    ArrayList<PurchaseEntity> getPurchases(String homeStoreId, long page) throws DataException;

    ArrayList<ReviewEntity> getReviews(String homeStoreId, long page) throws DataException;
}
