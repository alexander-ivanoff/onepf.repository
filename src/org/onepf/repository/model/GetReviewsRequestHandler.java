package org.onepf.repository.model;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.api.responsewriter.entity.ReviewEntity;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * Provide get reviews request to underlying DataService
 *
* @author Alexander Ivanoff on 12.03.14.
 */
public class GetReviewsRequestHandler extends BaseRequestHandler {

    private final Logger logger = LogManager.getLogger(GetReviewsRequestHandler.class.getName());


    public GetReviewsRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    /**
     * return list of reviews for given packageName and page
     *
     * @param pageHash - hash of the page to return
     * @return List of ReviewDescriptors
     * @throws DataException
     */
    public List<ReviewEntity> getReviews(String homeStoreId, int pageHash) throws DataException {
        long time = System.currentTimeMillis();
        ArrayList<ReviewEntity> reviews = dataService.getReviews(homeStoreId, pageHash);
        logger.debug("List reviews time: {}", (System.currentTimeMillis() - time));
        return reviews;
    }
}
