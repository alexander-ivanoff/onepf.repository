package org.onepf.repository.model;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.api.responsewriter.descriptors.ReviewDescriptor;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by ivanoff on 12.03.14.
 */
public class GetReviewsRequestHandler extends BaseRequestHandler {

    private final Logger logger = LogManager.getLogger(GetReviewsRequestHandler.class.getName());


    public GetReviewsRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    public List<ReviewDescriptor> getReviews(String packageName, int pageHash) throws DataException {
        long time = System.currentTimeMillis();
        ArrayList<ReviewDescriptor> reviews = dataService.getReviews(packageName, pageHash);
        logger.debug("List reviews time: {}", (System.currentTimeMillis() - time));
        return reviews;
    }
}
