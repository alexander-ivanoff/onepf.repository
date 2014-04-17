package org.onepf.repository.model;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.api.responsewriter.entity.DownloadEntity;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * Provide get downloads request to underlying DataService
 *
* @author Alexander Ivanoff on 12.03.14.
 */
public class GetDownloadsRequestHandler extends BaseRequestHandler {

    private final Logger logger = LogManager.getLogger(GetDownloadsRequestHandler.class.getName());


    public GetDownloadsRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    /**
     * return list of downloads for given packageName and page
     *
     * @param pageHash - hash of the page to return
     * @return List of DownloadDescriptors
     * @throws DataException
     */
    public List<DownloadEntity> getDownloads(String homeStoreId, long pageHash) throws DataException{
        long time = System.currentTimeMillis();
        ArrayList<DownloadEntity> descriptors = dataService.getDownloads(homeStoreId, pageHash);
        logger.debug("List downloads time: {}", (System.currentTimeMillis() - time));
        return descriptors;
    }
}
