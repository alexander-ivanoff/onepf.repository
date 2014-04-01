package org.onepf.repository.model.services;

import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Created by ivanoff on 25.03.14.
 *
 * T - is concrete Descriptor of entity
 * E - is source where we take data from
 * K - is representation to store data
 */

public interface DataService<E, K> {




    // base requests

    <T extends AbstractDescriptor> void store(DBEntity<T, E, K> entity) throws DataException;
    <T extends AbstractDescriptor> ArrayList<T> query(DBEntity<T, E, K> entity, String selection, String[] selectionArgs, String order) throws DataException;

    // TODO refactoring: move below method in different requests (Maybe Entities), here should be only generic requests

    void storeApplication(ApplicationDescriptor applicationDescriptor) throws DataException;
    Map<String,AppstoreDescriptor> getAppstores() throws DataException;
    List<ApplicationDescriptor> getApplicationsLog() throws DataException;
    List<ApplicationDescriptor> getApplicationsLog(String packageName, int currPageHash) throws DataException;
    ArrayList<DownloadDescriptor> getDownloads(String packageName, int currPageHash) throws DataException;
    ArrayList<PurchaseDescriptor> getPurchases(String packageName, int currPageHash) throws DataException;
    ArrayList<ReviewDescriptor> getReviews(String packageName, int currPageHash) throws DataException;
}
