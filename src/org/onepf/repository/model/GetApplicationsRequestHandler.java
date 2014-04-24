package org.onepf.repository.model;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.SimpleListRequestHandler;
import org.onepf.repository.model.services.StorageService;

import java.util.List;

/**
 *  Provide get applications request to underlying DataService
* @author Alexander Ivanoff on 12.03.14.
 */
public class GetApplicationsRequestHandler extends SimpleListRequestHandler<ApplicationEntity> {

    private final Logger logger = LogManager.getLogger(GetApplicationsRequestHandler.class.getName());

    public GetApplicationsRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    @Override
    public List<ApplicationEntity> getList(String appstoreId, int pageHash) throws DataException {
        long time = System.currentTimeMillis();
        List<ApplicationEntity> apps = dataService.getApplicationsLog(null, pageHash);
        logger.debug("List application time: {}", (System.currentTimeMillis() - time));
        return apps;
    }

}
