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
 * Created by ivanoff on 25.03.14.
 */
public interface DataService {


    // TODO refactoring: move method in different requests (Maybe Entities), here should be only generic requests

    void store(ApplicationDescriptor applicationDescriptor) throws DataException;
    void addDownload(DownloadDescriptor downloadDescriptor) throws DataException;

    List<ApplicationDescriptor> getApplicationsLog() throws DataException;
    List<ApplicationDescriptor> getApplicationsLog(String packageName, int page) throws DataException;
    Map<String,AppstoreDescriptor> getAppstores() throws DataException;

    List<LastUpdateDescriptor> getLastUpdate(String appstoreId) throws DataException;

    ArrayList<DownloadDescriptor> getDownloads(String packageName, long updateTime) throws DataException;
    ArrayList<PurchaseDescriptor> getPurchases(String packageName, long updateTime) throws DataException;
    ArrayList<ReviewDescriptor> getReviews(String packageName, int pageHash) throws DataException;
}
