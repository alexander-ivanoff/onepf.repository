package org.onepf.repository.model;

import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by ivanoff on 12.03.14.
 */
public class GetDownloadsRequestHandler extends BaseRequestHandler {


    public GetDownloadsRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    public List<DownloadDescriptor> getDownloads(String packageName, long updateTime) throws DataException{
        long time = System.currentTimeMillis();
        ArrayList<DownloadDescriptor> descriptors = dataService.getDownloads(packageName, updateTime);
        System.out.println("List downloads time: " + (System.currentTimeMillis() - time));  // TODO move to Log4J
        return descriptors;
    }
}
