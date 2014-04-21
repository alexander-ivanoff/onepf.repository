package org.onepf.repository.model;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.api.responsewriter.entity.ReviewEntity;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.SimpleListRequestHandler;
import org.onepf.repository.model.services.StorageService;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * Provide get reviews request to underlying DataService
 *
* @author Alexander Ivanoff on 12.03.14.
 */
public class GetReviewsRequestHandler extends SimpleListRequestHandler<ReviewEntity> {

    private final Logger logger = LogManager.getLogger(GetReviewsRequestHandler.class.getName());


    public GetReviewsRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    @Override
    public List<ReviewEntity> getList(String appstoreId, int pageHash) throws DataException {
        long time = System.currentTimeMillis();
        ArrayList<ReviewEntity> reviews = dataService.getReviews(appstoreId, pageHash);
        logger.debug("List reviews time: {}", (System.currentTimeMillis() - time));
        return reviews;
    }
}
