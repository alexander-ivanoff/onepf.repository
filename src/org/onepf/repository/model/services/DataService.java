package org.onepf.repository.model.services;

import org.onepf.repository.model.auth.AppstoreDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.PurchaseDescriptor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Created by ivanoff on 25.03.14.
 */
public interface DataService {

    void store(ApplicationDescriptor applicationDescriptor) throws DataException;
    List<ApplicationDescriptor> getApplications() throws DataException;
    Map<String,AppstoreDescriptor> getAppstores() throws DataException;
    ArrayList<DownloadDescriptor> getDownloads(String packageName, long updateTime) throws DataException;
    ArrayList<PurchaseDescriptor> getPurchases(String packageName, long updateTime) throws DataException;
}
