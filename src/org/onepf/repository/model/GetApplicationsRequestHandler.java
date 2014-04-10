package org.onepf.repository.model;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;

import java.util.List;

/**
 *  Provide get applications request to underlying DataService
* @author Alexander Ivanoff on 12.03.14.
 */
public class GetApplicationsRequestHandler extends BaseRequestHandler {

    private final Logger logger = LogManager.getLogger(GetApplicationsRequestHandler.class.getName());

    public GetApplicationsRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    /**
     * return list of applications for given page
     *
     * @param pageHash - hash of the page to return
     * @return List of ApplicationDescriptors
     * @throws DataException
     */
    public List<ApplicationDescriptor> getApplications(int pageHash) throws DataException {
        long time = System.currentTimeMillis();
        List<ApplicationDescriptor> apps = dataService.getApplicationsLog(null, pageHash);
        logger.debug("List application time: {}", (System.currentTimeMillis() - time));
        return apps;
    }
}
