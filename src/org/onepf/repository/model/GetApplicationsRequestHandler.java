package org.onepf.repository.model;

import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;

import java.util.List;

/**
 * Created by ivanoff on 12.03.14.
 */
public class GetApplicationsRequestHandler extends BaseRequestHandler {

    public GetApplicationsRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    public List<ApplicationDescriptor> getApplications(int pageHash) throws DataException {
        long time = System.currentTimeMillis();
        List<ApplicationDescriptor> apps = dataService.getApplicationsLog(null, pageHash);
        System.out.println("List applications time: " + (System.currentTimeMillis() - time));  // TODO move to Log4J
        return apps;
    }
}
