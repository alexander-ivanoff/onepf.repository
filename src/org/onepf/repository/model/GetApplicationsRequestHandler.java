package org.onepf.repository.model;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;

import java.util.List;

/**
 * Created by ivanoff on 12.03.14.
 */
public class GetApplicationsRequestHandler extends BaseRequestHandler {

    private final Logger logger = LogManager.getLogger(GetApplicationsRequestHandler.class.getName());

    public GetApplicationsRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    public List<ApplicationDescriptor> getApplications(int pageHash) throws DataException {
        long time = System.currentTimeMillis();
        List<ApplicationDescriptor> apps = dataService.getApplicationsLog(null, pageHash);
        logger.debug("List application time: {}", (System.currentTimeMillis() - time));
        return apps;
    }
}
