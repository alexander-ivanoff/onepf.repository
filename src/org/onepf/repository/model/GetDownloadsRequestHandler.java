package org.onepf.repository.model;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.api.responsewriter.entity.DownloadEntity;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.SimpleListRequestHandler;
import org.onepf.repository.model.services.StorageService;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * Provide get downloads request to underlying DataService
 *
* @author Alexander Ivanoff on 12.03.14.
 */
public class GetDownloadsRequestHandler extends SimpleListRequestHandler<DownloadEntity> {

    private final Logger logger = LogManager.getLogger(GetDownloadsRequestHandler.class.getName());


    public GetDownloadsRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    @Override
    public List<DownloadEntity> getList(String appstoreId, int pageHash) throws DataException {
        long time = System.currentTimeMillis();
        ArrayList<DownloadEntity> descriptors = dataService.getDownloads(appstoreId, pageHash);
        logger.debug("List downloads time: {}", (System.currentTimeMillis() - time));
        return descriptors;
    }

}
